/hom/geirkp/TRANSFER/Benchmarks/bin/CarrN.pe -n 100 -a1 -0.1 -a2 0.1 -x1 6 -x2 14 -k1 0.2 -k2 0.2 -form 16.7f -xmax 30  >pad.pos 
/hom/geirkp/bin/derv -mid pad.pos
mv pad.pos_der pad.inp
/hom/geirkp/lagbouss/bin/rurun.pe -nodefpath -l A30 -input file-paddle -randfile pad.inp -nut 10 -t 30  -jbname JNW -sti NW -bryt -0.95  -heln 10 -n 80,160,320