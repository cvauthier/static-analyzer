#!/bin/sh
set -e

for EX in examples/*.c; do
	bname=`basename $EX .c` 
	echo "Analyzing example $EX"
	./analyzer --domain=interval $EX >> examples/${bname}_res.txt
	dot -Tpdf cfg.dot >> examples/${bname}_cfg.pdf
	mv cfg.dot examples/${bname}_cfg.dot
done
