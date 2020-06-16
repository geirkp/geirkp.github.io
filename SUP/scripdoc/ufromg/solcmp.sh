
dir=test

prog=ufromg
pdir=/hom/geirkp/labproj/progs/i386


if [ ! -d $dir ]; then
  mkdir -p $dir
fi

cd $dir

inpfil=ineta

/hom/geirkp/lagbouss/bin/rurun.pe -nodefpath -l A20 -input file-paddle -randfile /info/SUP/soldata/amp0.15/Serre.lag -nut 10 -t 25  -jbname Jsol -sti soliton -bryt -0.95  -tser 25 -heln 10 -n 160

ddir=soliton/n160

gawk -- '{print $1,$2}' $ddir/hgauge > $inpfil
gawk -- '{print $1,-$2}' $ddir/ugauge > usimul



if [ -z $glatt ]; then
  glatt=0
fi

cat<<EOF>indat
$inpfil
$glatt
EOF

$pdir/$prog < indat
mv uxx uxx$glatt 
genkurv -ostem u -sys plotxy -dev psC usimul umid $inpfil
genkurv -ostem uxx -sys plotxy -dev psC  uxx$glatt 

gv u.ps &
gv uxx.ps &


