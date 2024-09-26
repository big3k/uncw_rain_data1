


i=1
cat input_files |while read file; do 
./reproj_data $file $i
cat $i >> outf.bin
let i=i+1
done



