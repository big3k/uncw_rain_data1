#! /usr/bin/bash
# submit 12 jobs, 1 for each month of the given year 

#for year in {2023..2024}; do 
for year in {2011..2022}; do 

cat > gpu_$year.sh <<EOF
#! /usr/bin/bash
# process one year's data, est. 33 hours on GPU
#SBATCH --job-name=gpu$year
#SBATCH --ntasks=1                      # Number of tasks. This should stay as 1.
#SBATCH --cpus-per-task=16              # Number of CPU cores per task.
#SBATCH --mem=32G                      # Total memory limit.
#SBATCH --time=3-00:00:00                 # Time limit days-hrs:min:sec
#SBATCH --partition=gpu             # Partition name (general or gpu)
#SBATCH --gres=gpu:1

mkdir /scratch/tiany/gprof_ir/output_data/$year/
# gpu
export TORCHDYNAMO_DISABLE=1
module load cuda

gprof_ir run --device cuda --output_path /scratch/tiany/gprof_ir/output_data/$year/  /storage/cms/youy_lab/tiany/gprof_ir/input_data/$year/

#/scratch/tiany/gprof_ir/input_data/$year/

EOF

chmod u+x gpu_$year.sh 
sbatch gpu_$year.sh 

done

