

# Have not finished yet....

# modify c source code to commodate long dir and file names. Need to run 
# under ama2nc/ or amb2nc/, and then run gmake

mkdir backup
cp SWATH.h ESWATH.h AMA2NC_FCDR.c rama_wfcdr.c backup
sed -i -e 's/\[80\]/\[1080\]/' SWATH.h
sed -i -e 's/\[80\]/\[1080\]/' ESWATH.h
sed -i -e 's/direct\[80\]/direct\[1080\]/g' -e 's/mkcmd\[100\]/mkcmd\[1100\]/' AMA2NC_FCDR.c
sed -i -e 's/filename\[80\]/filename\[1080\]/' rama_wfcdr.c



