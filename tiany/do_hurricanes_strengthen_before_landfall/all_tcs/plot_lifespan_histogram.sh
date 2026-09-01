
grep lifespan log.step0.txt |awk -F'=' '{print $4}' |awk -F\. '{print $1}' > life_hours.txt

./plot_histogram.py life_hours.txt 3 

