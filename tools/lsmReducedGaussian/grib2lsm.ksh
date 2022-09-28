#!/bin/ksh

set -e

in=$1
out=$2
# read grib message and write to file land mask as bit values

if [[ $# != 2 ]]
then
	echo "Syntax: $0 <infile> <outfile>"
	echo "Example: $0 /scratch/ma/mas/lsm.0.25 lsm.0.25.data"
	exit 1
fi

touch $out
p=./bitPack
grib_get_data -F "%g" $in | grep -v Value | less | awk '{print $3;}' | $p > $out
