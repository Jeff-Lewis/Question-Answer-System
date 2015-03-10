#!/bin/bash
#chmod u+x filename.sh

#enter allPostings directory
cd /Users/Nick/Documents/IEMS_395_HW4/allPostings
#remove .txt extensions
find . -type f -name '*.txt' | while read f; do mv "$f" "${f%.txt}"; done
#split files into new directory, 1 line per file
for f in *; do split -l 1 "$f" ~/Documents/IEMS_395_HW4/reformattedPostings/"$f"; done;
#put .txt extension back
for i in *; do mv "$i" "$i.txt"; done;
#enter reformatted directory
cd /Users/Nick/Documents/IEMS_395_HW4/reformattedPostings
#add .txt extensions
for i in *; do mv "$i" "$i.txt"; done;