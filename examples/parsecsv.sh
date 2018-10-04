#! /bin/bash
#
#  Example of two-pass parser script
#  Parse fixed column type csv, using first row as definition 
#  of the column names.
#

INPUT=${1:-data.csv}

tmp=./tmp.$$

rm_tmpfiles () {
        rm $tmp.*
}

die () {
        echo "$*"
        exit 1
}

trap rm_tmpfiles EXIT

#  First pass - generating second pass grammar
cat csvi.peg > $tmp.peg   
../pegi csv.peg "$INPUT" >> $tmp.peg || die  Error parsing data

#  Second pass 
../pegi $tmp.peg "$INPUT" 
