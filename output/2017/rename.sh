#!/bin/sh

for i in `seq -w 01 15`; do
    sed -i '' -e '$ d' day_"$i"_part_1
    sed -i '' -e '$ d' day_"$i"_part_2
done
