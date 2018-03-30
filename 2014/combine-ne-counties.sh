#!/usr/bin/bash

for file in *.csv
do
    # for each county, delete the first header line, append to tmp-nebraska 
    sed -e '1d' "$file" >> tmp-nebraska.csv
    # Note that I have to quote protect the file name, since some filenames contain a space
    # e.g. Red Willow, Keya Paha, Scotts Bluff and Box Butte
done
# insert the header line again
sed -e '1icounty,precinct,office,district,party,candidate,votes' tmp-nebraska.csv > 20141104_ne_general_statewide_precinct.csv
# too bad sed doesn't have in-place editing, like perl, but this is easy enough
rm tmp-nebraska.csv
