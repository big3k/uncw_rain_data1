#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Assemble PNG frames into an animated GIF.
"""

import argparse
import glob
from PIL import Image
import os


def main(frames_dir, out_gif, fps):
    files = sorted(glob.glob(os.path.join(frames_dir, "frame_*.png")))
    if not files:
        raise RuntimeError("No PNG frames found")

    images = [Image.open(f) for f in files]

    duration_ms = int(1000 / fps)

    images[0].save(
        out_gif,
        save_all=True,
        append_images=images[1:],
        duration=duration_ms,
        loop=0,
        optimize=True,
    )

    print(f"GIF written: {out_gif}")


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("--frames", required=True, help="Directory with frame_XXX.png")
    ap.add_argument("--out", required=True, help="Output GIF filename")
    ap.add_argument("--fps", type=float, default=4.0,
                    help="Frames per second (half-hour = 10–15 works well)")
    args = ap.parse_args()

    main(args.frames, args.out, args.fps)
