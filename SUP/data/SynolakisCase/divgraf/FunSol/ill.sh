cp -f TFunSol.aux ill.aux
cp -f TFunSol.bbl ill.bbl
cp -f TFunSol.blg ill.blg
latex ill.tex
mv ill.dvi TFunSol.dvi
mv ill.aux TFunSol.aux
mv -f ill.bbl TFunSol.bbl
mv -f ill.blg TFunSol.blg
dvipdf TFunSol.dvi
