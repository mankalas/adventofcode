#!/bin/sh

y=$1
i=$2
echo "Year $y $i"
if [ ! -d $y ]; then
    echo "Directory '$y' doesn't exist. Creating..."
    mkdir $y
fi
echo "Entering $y"
cd $y
printf -v padded_i "%02d" $i
file_name="day_$padded_i"
if [ ! -e $file_name ]; then
    echo "Downloading day $i..."
    curl -s -o $file_name https://adventofcode.com/$y/day/$i/input -X GET -H 'Cookie: session=53616c7465645f5f4a4f4ceddcec706e8c87b74bd4ee018181466861443d2429a410699c8556396ffd0486e9f7a23820'
    echo done
else
    echo "$file_name already exists"
fi
echo "Finished $y. Exiting..."
cd ..
