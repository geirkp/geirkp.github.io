#!/bin/sh
list="solny ipol regmod les ilib farf permsol bispline airysou geom"
list="solny ipol  ilib  permsol "
for fil in $list; do
f77 -O -Wimplicit -c $fil.f
olist="$olist $fil.o"
done
ar -r bibl.a  $olist
ar -ts bibl.a; ranlib bibl.a
rm  $olist

