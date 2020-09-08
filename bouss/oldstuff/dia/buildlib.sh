#!/bin/sh
list="dia1 dia2n dia3n diafn diamen diadd simles gpri"
list="dia1 dia2n dia3n diafn diamen diadd  gpri"

for fil in $list; do
f77 -O -Wimplicit -c $fil.f
olist="$olist $fil.o"
done

ar -r dia.a  $olist
ar -ts dia.a; ranlib dia.a
rm  $olist

