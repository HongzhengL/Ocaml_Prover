#!/bin/bash

dune test
printf "lex and parse sample.ml to file 1.txt:\n"
./_build/default/bin/main.exe --printback sample.ml
./_build/default/bin/main.exe --printback sample.ml > 1.txt
printf "\nlex and parse 1.txt to file 2.txt:\n"
./_build/default/bin/main.exe --printback 1.txt
./_build/default/bin/main.exe --printback 1.txt > 2.txt
printf "\ndiff of 1.txt and 2.txt:\n"
diff 1.txt 2.txt
rm 1.txt 2.txt
