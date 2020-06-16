#!/bin/bash
tag=demo;redf=1.0;nflist=4,6;glord=2;ng=10
# some variables are set here. The rest will be set as defaults below.
# Each variable set above this line will overrule a default.
# tag is an identifier in the path for the data.
# nflist is a series of relative refinement. The CPU time does not(!) scale
# well with increasing nf.
# glord is order of filtering to avoid nonlinear instabilities.
# ng is order og Gaussian quadrature used in panel integrals.
# Up to 20 is possible, but for smooth surfaces usually 5, say, is eneough.
#redf governs the temporal resolution relative to the spatial one. The value
# should be small when the solutions becomes "extreme".
marker=newbreak
her=`pwd`
dxref=0.6

if [ -n "$Geirkp" ]; then
    HJEM=$Geirkp
else
    HJEM=$HOME
fi

if [ -z "$ng" ]; then
  ng=5
fi

if  [ -z "$mh" ]; then
  mh=1
fi

if [ -n "$hl" ]; then
  hllist=$hl
else
  hllist=0.1
fi

hllist=`echo $hllist | sed 's/,/ /g'`


if [ -z "$L" ]; then
  L=16
fi

if [ -z "$glord" ]; then
  glord=2
fi
 

if [ -z "$slopl" ]; then
  slopl=5.8
fi

if [ -z "$nut" ]; then
  nut=150
  skriv="0:150;10 120:150"
else
  if [ -z "$skriv" ]; then
  skriv=alle
  fi
fi

if [ -z "$a" ]; then
  a='0.6'
fi

if [ -z "$nflist" ]; then
    nflist='4,6,9'
fi
nflist=`echo $nflist | sed 's/,/ /g'`


pdir=$HJEM/laplace/rusource/i386
prog=nru



cd $her 

for hl in $hllist; do
  if [ -z "$t" ]; then
     case $hl in
       0.1)t=11.25;shelf=3;;
       0.2)t=11.25;shelf=3.5;; 
       0.4)t=12;shelf=7;skriv="0:150;10 110:120";; 
       0.6)t=14;shelf=10;; 
      esac
  fi
if [ -z "$shelf" ]; then
  shelf=3
fi


for nf in $nflist; do
  cd $her

# slopl added and  path rearranged 7/6 2018 
  dir=$marker$tag$glord/"$a"hl$hl'sl'$slopl/nf$nf
  if [ ! -d $dir ]; then
      mkdir -p $dir
  else
      rm $dir/*
  fi

  cd $dir

  dx=`echo $dxref $nf | gawk -- '{print $1/$2}'`
  n=`echo $nf $a | gawk -- '{print $1*200/sqrt(3*$2)}'` 


  # fullsol produces the solitary wave.
  $HJEM/bin/fullsol -a $a -dx $dx -n $n -eps 0.001 -keep
  # expand2.pe turn the solitary wave data into suitable files inp.eta, inp.f
  # and inp.vn. It also produces bottom data in inp.bott.
  # $hl is the shallow depth, $slopl is the length of the slope, $L is distance
  # from initial peak of solitary wave to left wall.
  $HJEM/laplace/soliton/expand2.pe -slopl $slopl -L $L -hl $hl -shelf $shelf fsol$a
  
  lam=`tail -1 fsol$a | gawk -- '{print $1}'`
  c=`getnew.pe -par fullsol.par c`
  if [ -z "$t" ]; then
    t=`echo $L  $c| gawk  -- '{print $1/$2}'` 
  fi
  tint=`echo $t  $nut| gawk  -- '{print $1/$2}'`

bpoints=`wc -l inp.bott | gawk -- '{print $1}'`

echo par= $t $hl $nf
#
# Lines in input-file starting with ! are treatted as comments by the program
#
cat <<+++ > indat
 !give bathimetry type/simple-shelf/
fil
 !gi dybdefil
inp.bott
 !gi punkter i vertikalen (2 tall)/ 2 2 /
3 4
 !give number of surface points/ 155 /
 
 !give number of bottom points/ 25 /
$bpoints
 !give interp. meth./linear/
splin
  !gi ising/ 0 /
2
! regne ikkelineart?/nei/
j
! splineder i konvek-ledd.?/nei/
j
 !gi glatteorden/ 2 /
 $glord
 !gi glattefak/ 1.000000000000000 /
 
 !gi glattehyppighet/ 1 /
 
 !give Gauss-point number and mult/ 3 1 /
$ng $mh
 !give time increment-style/uniform/

  !GI ANTALL UTSKRIFTER/ 0 /
$nut
 !
 !gi tidsintervall (sek)/ 1.000000000000000 /
$tint
 !gi red.faktor/ 1.000000000000000 /
 $redf
 !maksimal verd:1
 !gi tider for utdata/alle/
 $skriv
! write intermediate data?/nei/
n
 %%kilde dialog
 !give initial-settings/oscill-linear/
init
 !give input-stem/inp/
 
 !gi punkter for tids-serier
 !avslutt med blank linje
 !gi mode of timesr
 none
 ! give material points
 -1
+++
#/hom/geirkp/laplace/rusource/i386/lruLF8 <indat >& $ver.log
#shift of version 28/8 2019, name on log file as well
$pdir/$prog <indat 2> p.err 1>p.log
#lruLF8
done
done

echo slutt $marker
