
(
s=0
for i in `seq 1 14`; do 
 date -d "2020/1/1 $s sec" +%H%M%S
 let s=s+6171
done
echo 235959
) | awk '{printf "%s,", $1}' 
