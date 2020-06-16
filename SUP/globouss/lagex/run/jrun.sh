her=`pwd`
cd /hom/geirkp/bouss/trappe/i386
if make -f mggloGF8; then
cd $her
nice -19 /hom/geirkp/bouss/trappe/i386/ggloGF8 < indat 1>pp.log 2>pp.err
fi
