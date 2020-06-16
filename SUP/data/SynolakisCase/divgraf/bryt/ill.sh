cp -f BRYT.aux ill.aux
cp -f BRYT.bbl ill.bbl
cp -f BRYT.blg ill.blg
latex ill.tex
mv ill.dvi BRYT.dvi
mv ill.aux BRYT.aux
mv -f ill.bbl BRYT.bbl
mv -f ill.blg BRYT.blg
dvipdf BRYT.dvi
