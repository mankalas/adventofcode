#!/bin/sh

INPUT_BASE_DIR="./input"
OUTPUT_BASE_DIR="./output"

# Tip: to iterate from 01 to 25: seq -w 01 25

y=$1
d=$2
printf -v pad_d "%02d" $d
echo "Year $y $d"

## Input
echo "Getting the puzzle's input..."
input_year="$INPUT_BASE_DIR/$y"
if [ ! -d $input_year ]; then
    echo "Directory '$input_year' doesn't exist. Creating..."
    mkdir $input_year
    echo "Done."
fi
echo "Entering $input_year..."
cd $input_year
file_name="day_$pad_d"
if [ ! -e $file_name ]; then
    echo "Downloading day $d..."
    curl -s -o $file_name https://adventofcode.com/$y/day/$d/input -X GET -H 'Cookie: session=53616c7465645f5f4a4f4ceddcec706e8c87b74bd4ee018181466861443d2429a410699c8556396ffd0486e9f7a23820'
    echo done
else
    echo "$file_name already exists"
fi
cd ../..
echo "Puzzle's input done."

# Output
echo "Creating empty output..."
output_year="$OUTPUT_BASE_DIR/$y"
if [ ! -d $output_year ]; then
    echo "Directory '$output_year' doesn't exist. Creating..."
    mkdir $output_year
    echo "Done."
fi
echo "Entering $output_year..."
cd $output_year
touch day_"$pad_d"_part_1
touch day_"$pad_d"_part_2
cd ../..
echo "Puzzle's output done"

# Mustache
data="---
\nday_i: $d
\nday_s: '$pad_d'
\n---"

echo $data

# Haskell
echo "Generating Haskell files..."
cd haskell
echo $data | mustache - templates/Day.hs.mustache > aoc$y/src/Day$pad_d.hs
cd ..

echo "Done"

echo "Finished year $y day $d."
