#! /bin/bash
# 2/11/2024
#  Based on pick_cols_by_names.sh, but the column list can be a mix of column names and
#    column numbers, separated by commas.
#
# usage:
#  $0 colx,coly,colz [inputfile]

# Example
#
# $ cat test.csv
# Col1,Col2,Col3,Col4
# a,a2,b,c
# d,f,g,,
# a,x
#
# $ pick_cols.sh Col3,1,2,2,Col2 test.csv
# Col3,Col1,Col2,Col2,Col2
# b,a,a2,a2,a2
# g,d,f,f,f
# ,a,x,x,x

if [ $# -lt 1 ]; then
   echo usage:
   echo "$0 colx,coly,colz [inputfile]"
   exit -1
fi

cols=$1
input=$2

cat $input | awk -F, -vcols="$cols" -vOFS=, '
BEGIN {
    split(cols,out,",")
}
NR==1 {
    #printf "%s\n", cols   # input verification
    for(i=1; i <= length(out); i++) {
        if (out[i]+0 == out[i]) {  # column id is column number, not name
           out[i]=$out[i]  # change to column name
        }
        printf "%s,", out[i]  # new header
    }
    printf "\n"
    for (i=1; i<=NF; i++) {  # load mapping between column names and column number
        col=$i
        gsub(/^ /, "", col)
        ix[col] = i
    }
}
NR>1 {
    for(i=1; i <= length(out); i++) {
        if ( ix[out[i]] > 0 )
           field=$ix[out[i]]
        else
           field="null"
        printf "%s%s", field, OFS
    }
    print ""
}' |sed -e 's/,$//'
