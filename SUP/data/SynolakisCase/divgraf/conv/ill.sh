cp -f Conver.aux ill.aux
cp -f Conver.bbl ill.bbl
cp -f Conver.blg ill.blg
latex ill.tex
mv ill.dvi Conver.dvi
mv ill.aux Conver.aux
mv -f ill.bbl Conver.bbl
mv -f ill.blg Conver.blg
dvipdf Conver.dvi
