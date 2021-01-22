#!/bin/sh

for y in {2015..2020}; do
    echo "Year $y"
    if [ ! -d $y ]; then
        echo "Directory '$y' doesn't exist. Creating..."
        mkdir $y
    fi
    echo "Entering $y"
    cd $y
    for i in {1..25}; do
        printf -v padded_i "%02d" $i
        file_name="day_$padded_i"
        if [ ! -e $file_name ]; then
            echo "Downloading day $i..."
            curl -s -o $file_name https://adventofcode.com/$y/day/$i/input -X GET -H 'Cookie: session=53616c7465645f5f6f91b385a54dd8335b8f27db0517c5e9b7cedbb49e52cd21083099035d86253ed809fa793267da30'
            echo done
        else
            echo "$file_name already exists"
        fi
    done
    echo "Finished $y. Exiting..."
    cd ..
done
