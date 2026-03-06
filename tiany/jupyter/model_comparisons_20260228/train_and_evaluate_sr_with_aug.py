"""
Train and evaluate 3 super-resolution models (SSMIS -> GMI) using your load_dataset().
Metrics: Bias, RMSE, Pearson correlation (in original physical units).

Models:
  1) SRCNN-like baseline
  2) Residual CNN (EDSR-lite style)
  3) Lightweight U-Net

Usage:
  python train_and_evaluate_sr.py

Notes:
- Works with your load_mat_images.load_dataset() function.
- If your load_dataset returns 3 values (train_ds, test_ds, scale) or 4 values
  (train_ds, test_ds, scale, meta), both are supported.

Author: tailored for Yudong's pipeline.
"""

import os
import math
import numpy as np
import tensorflow as tf

# Ensure TF sees GPU if available
print("TensorFlow:", tf.__version__)
print("GPU available:", tf.config.list_physical_devices('GPU'))

# ---- Reproducibility (best-effort) ----
SEED = 42
np.random.seed(SEED)
tf.random.set_seed(SEED)
os.environ["PYTHONHASHSEED"] = str(SEED)

# ---- Import your data loader ----
from load_mat_images import load_dataset  # your module

# ========= Data augmentation ================ 

def _random_k(seed2):
    """Pick k in {0,1,2,3} with stateless RNG."""
    return tf.random.stateless_uniform([],
                                       seed=seed2,
                                       minval=0,
                                       maxval=4,
                                       dtype=tf.int32)

def _rand_bool(seed2, p=0.5):
    """Bernoulli with probability p using stateless RNG."""
    u = tf.random.stateless_uniform([], seed=seed2, minval=0.0, maxval=1.0)
    return u < p

def _geom_augment_pair(x, y, seed2, enable_vflip=True):
    """
    Apply identical geometric transforms to (x, y).
    x, y: (B, 100, 100, 1)
    seed2: shape (2,) int32 stateless RNG seed
    """
    # Random 90-degree rotation
    k = _random_k(seed2 + tf.constant([0, 1], dtype=seed2.dtype))
    x = tf.image.rot90(x, k=k)
    y = tf.image.rot90(y, k=k)

    # Random horizontal flip
    if _rand_bool(seed2 + tf.constant([1, 2], dtype=seed2.dtype), p=0.5):
        x = tf.image.flip_left_right(x)
        y = tf.image.flip_left_right(y)

    # Optional random vertical flip (disable if geophysically inappropriate)
    if enable_vflip and _rand_bool(seed2 + tf.constant([2, 3], dtype=seed2.dtype), p=0.5):
        x = tf.image.flip_up_down(x)
        y = tf.image.flip_up_down(y)

    return x, y

def add_geometric_augmentation(train_ds: tf.data.Dataset,
                               seed: int = 42,
                               enable_vflip: bool = True) -> tf.data.Dataset:
    """
    Wrap a batched (x, y) dataset with geometric-only augmentation.
    Preserves shape (B, 100, 100, 1), applies identical geometry to x and y.
    Uses stateless RNG for reproducibility and performance.
    """
    counter = tf.data.experimental.Counter()  # 0,1,2,... per batch

    def _map_with_seed(batch, idx):
        # Compose a deterministic per-batch seed from (idx, user_seed)
        seed2 = tf.stack([tf.cast(idx, tf.int32), tf.constant(seed, tf.int32)])
        x, y = batch
        return _geom_augment_pair(x, y, seed2, enable_vflip=enable_vflip)

    # Zip with counter and map in parallel
    aug = tf.data.Dataset.zip((train_ds, counter)).map(
        _map_with_seed, num_parallel_calls=tf.data.AUTOTUNE
    )
    return aug
# ==========================
# Model builders
# ==========================


def build_srcnn(input_shape=(101, 101, 1)) -> tf.keras.Model:
    """
    Simple SRCNN-like architecture (same-size regression).
    """
    inputs = tf.keras.Input(shape=input_shape)
    x = tf.keras.layers.Conv2D(64, (9, 9), padding="same", activation="relu")(inputs)
    x = tf.keras.layers.Conv2D(32, (5, 5), padding="same", activation="relu")(x)
    x = tf.keras.layers.Conv2D(1,  (5, 5), padding="same", activation="linear")(x)
    return tf.keras.Model(inputs, x, name="SRCNN_like")


def _res_block(x, filters=64, kernel_size=3, scale=0.1):
    """
    A small residual block: Conv-ReLU-Conv with residual scaling.
    """
    skip = x
    y = tf.keras.layers.Conv2D(filters, kernel_size, padding="same", activation="relu")(x)
    y = tf.keras.layers.Conv2D(filters, kernel_size, padding="same", activation=None)(y)
    if scale is not None:
        y = tf.keras.layers.Lambda(lambda t: t * scale)(y)
    out = tf.keras.layers.Add()([skip, y])
    return out


def build_rescnn(input_shape=(101, 101, 1), filters=64, num_res_blocks=8) -> tf.keras.Model:
    """
    EDSR-lite style residual CNN (no upscaling; 1x SR mapping SSMIS->GMI).
    """
    inputs = tf.keras.Input(shape=input_shape)
    x = tf.keras.layers.Conv2D(filters, 3, padding="same")(inputs)
    conv1 = x
    for _ in range(num_res_blocks):
        x = _res_block(x, filters=filters, kernel_size=3, scale=0.1)
    x = tf.keras.layers.Conv2D(filters, 3, padding="same")(x)
    x = tf.keras.layers.Add()([x, conv1])  # global skip
    outputs = tf.keras.layers.Conv2D(1, 3, padding="same")(x)
    return tf.keras.Model(inputs, outputs, name="ResCNN_EDSRlite")


def conv_block(x, filters, k=3):
    x = tf.keras.layers.Conv2D(filters, k, padding="same")(x)
    x = tf.keras.layers.BatchNormalization()(x)
    x = tf.keras.layers.Activation("relu")(x)
    return x


def build_unet_light(input_shape=(101, 101, 1), base=32) -> tf.keras.Model:
    """
    Lightweight U-Net (few levels; keeps same output size).
    """
    inputs = tf.keras.Input(shape=input_shape)

    # Encoder
    c1 = conv_block(inputs, base)
    p1 = tf.keras.layers.MaxPooling2D((2, 2))(c1)

    c2 = conv_block(p1, base * 2)
    p2 = tf.keras.layers.MaxPooling2D((2, 2))(c2)

    c3 = conv_block(p2, base * 4)

    # Decoder
    u2 = tf.keras.layers.Conv2DTranspose(base * 2, 2, strides=2, padding="same")(c3)
    u2 = tf.keras.layers.Concatenate()([u2, c2])
    c4 = conv_block(u2, base * 2)

    u1 = tf.keras.layers.Conv2DTranspose(base, 2, strides=2, padding="same")(c4)
    u1 = tf.keras.layers.Concatenate()([u1, c1])
    c5 = conv_block(u1, base)

    outputs = tf.keras.layers.Conv2D(1, 1, padding="same", activation="linear")(c5)
    return tf.keras.Model(inputs, outputs, name="UNet_Light")

# ==========================
# Training & Evaluation
# ==========================

def compile_model(model: tf.keras.Model, lr=1e-3):
    opt = tf.keras.optimizers.Adam(learning_rate=lr)
    # Using MSE for SR regression; MAE as a training metric
    model.compile(optimizer=opt, loss="mse", metrics=[tf.keras.metrics.MeanAbsoluteError(name="mae")])
    return model


def get_callbacks(patience=5, min_delta=1e-4):
    return [
        tf.keras.callbacks.EarlyStopping(monitor="val_loss", patience=patience, min_delta=min_delta,
                                         restore_best_weights=True),
        tf.keras.callbacks.ReduceLROnPlateau(monitor="val_loss", factor=0.5, patience=max(2, patience // 2),
                                             min_delta=min_delta, verbose=1)
    ]


def collect_arrays(dataset: tf.data.Dataset):
    """
    Concatenate all batches from a (X, y) dataset into numpy arrays.
    Returns arrays of shape (N, H, W, 1).
    """
    xs, ys = [], []
    for x, y in dataset:
        xs.append(x.numpy())
        ys.append(y.numpy())
    X = np.concatenate(xs, axis=0) if xs else np.empty((0,))
    Y = np.concatenate(ys, axis=0) if ys else np.empty((0,))
    return X, Y


def compute_metrics(y_true, y_pred, scale=1.0):
    """
    Compute bias, RMSE, Pearson correlation in original units.
    Ignores NaNs/Infs if present.
    """
    # Back to physical units
    y_true = y_true * scale
    y_pred = y_pred * scale

    # Flatten
    yt = y_true.reshape(-1)
    yp = y_pred.reshape(-1)

    # Finite mask
    m = np.isfinite(yt) & np.isfinite(yp)
    yt = yt[m]
    yp = yp[m]

    if yt.size == 0:
        return np.nan, np.nan, np.nan

    bias = float(np.mean(yp - yt))
    rmse = float(np.sqrt(np.mean((yp - yt) ** 2)))

    yt_std = float(np.std(yt))
    yp_std = float(np.std(yp))
    if yt_std == 0.0 or yp_std == 0.0:
        corr = np.nan
    else:
        corr = float(np.corrcoef(yt, yp)[0, 1])

    return bias, rmse, corr


def train_and_eval(model_builder,
                   model_name: str,
                   train_ds: tf.data.Dataset,
                   test_ds: tf.data.Dataset,
                   scale: float,
                   epochs=30,
                   lr=1e-3,
                   patience=5):
    """
    Train one model and compute metrics on test set.
    """
    print(f"\n===== {model_name} =====")
    # Determine input shape from a sample batch
    example_x, example_y = next(iter(train_ds.take(1)))
    input_shape = example_x.shape[1:]  # (H, W, 1)
    model = model_builder(input_shape=input_shape) if "input_shape" in model_builder.__code__.co_varnames \
            else model_builder()
    compile_model(model, lr=lr)
    model.summary()

    # Training
    history = model.fit(
        train_ds,
        validation_data=test_ds,
        epochs=epochs,
        callbacks=get_callbacks(patience=patience),
        verbose=1
    )

    # Collect test arrays and predict
    X_te, Y_te = collect_arrays(test_ds)
    Y_hat = model.predict(X_te, verbose=0)

    # Metrics in physical units
    bias, rmse, corr = compute_metrics(Y_te, Y_hat, scale=scale)

    print(f"{model_name} -> Bias: {bias:.6f}, RMSE: {rmse:.6f}, Corr: {corr:.4f}")
    return {
        "model": model,
        "history": history.history,
        "metrics": {"bias": bias, "rmse": rmse, "corr": corr},
        "preds": Y_hat,
    }


def main():
    # ---- Load data from your module, clip to 100x100 from 101x101 ----
    # Handles both 3-tuple and 4-tuple returns.
    ret = load_dataset(batch_size=16, fullset=True, height=100, width=100)
    meta = None
    if isinstance(ret, tuple) and len(ret) == 3:
        train_ds, test_ds, scale = ret
    elif isinstance(ret, tuple) and len(ret) == 4:
        train_ds, test_ds, scale, meta = ret
    else:
        raise RuntimeError("Unexpected return signature from load_dataset().")

    # Sanity: print a batch shape
    x1, y1 = next(iter(train_ds.take(1)))
    print("Train batch shapes:", x1.shape, y1.shape)
    print("Scale (to physical units):", scale)

    # ---- Define the three models ----
    models = [
        ("SRCNN-like", build_srcnn),
        ("ResCNN (EDSR-lite)", build_rescnn),
        ("UNet-Light", build_unet_light),
    ]

    results = []
    for name, builder in models:
        res = train_and_eval(
            model_builder=builder,
            model_name=name,
            train_ds=train_ds,
            test_ds=test_ds,
            scale=scale,
            epochs=30,       # adjust as needed
            lr=1e-3,
            patience=5
        )
        results.append((name, res["metrics"]))

    # ---- Summary table ----
    print("\n===== Summary (physical units) =====")
    print(f"{'Model':<20} {'Bias':>12} {'RMSE':>12} {'Corr':>8}")
    for name, m in results:
        b, r, c = m["bias"], m["rmse"], m["corr"]
        print(f"{name:<20} {b:12.6f} {r:12.6f} {c:8.4f}")

    # (Optional) Save results to a CSV
    try:
        import csv
        with open("sr_eval_results_with_aug.csv", "w", newline="") as f:
            w = csv.writer(f)
            w.writerow(["model", "bias", "rmse", "corr"])
            for name, m in results:
                w.writerow([name, m["bias"], m["rmse"], m["corr"]])
        print("Saved: sr_eval_results_with_aug.csv")
    except Exception as e:
        print("Could not save CSV:", e)


if __name__ == "__main__":
    main()
