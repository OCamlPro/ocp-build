set terminal postscript eps enhanced "NimbusSanL-Regu" 20

set autoscale xy
set key top left
set grid

set style data linespoints

set out 'hash.eps'
#set logscale xyy2
set xlabel 'Entries in hash table'
set ylabel 'Words in heap'
#set y2label 'Snapshots'
#set y2tics
#set ytics nomirror
#set yrange [0.5:100]
#set y2range [0.3:900]
#set ytics (1,10,25,50,75,100)
#set y2tics (1,10,100,852)
set xrange [100:10000]

set title 'Space usage of hash tables'
plot \
   'hash.txt' using ($1):($3) axes x1y1 with linespoints ps 2 title 'OCamlPro Hashtbl', \
   'hash.txt' using ($1):($4) axes x1y1 with linespoints ps 2 title 'Standard Hashtbl', \
   0 notitle
