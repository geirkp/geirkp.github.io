#!/bin/sh
marker=mpol
dir=$marker
sdir=/hom/geirkp/lib/i386

if [ ! -d $dir ]; then
  mkdir $dir
fi

cd $dir

cat<<EOF>korn.dat
1  2 0.2
3  5 0.4
7  1 0.6
11 7 1.1
EOF

cat<<EOF>indat
!gi fil
korn.dat
!give interval
1 11
!give n/100/
100
!give undef value
-100
!give tolerence at boundaries/1.0e-7/

EOF
$sdir/poly < indat


/hom/geirkp/bin/genkurv -sys plotxy -dob -xinch 4.5 -yinch 3.5n -anot 0.3 3 3 0.5  -dev psC korn.dat -polyg.  smpoly.dat -smooth 

convert -density 100x100 myplot.ps pol_test.png
eog pol_test.png &

