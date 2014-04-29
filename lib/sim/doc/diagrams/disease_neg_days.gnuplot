set ylabel "P(x)"
set xlabel "x"
set border 1+2
set ytics nomirror
set xtics nomirror
set xrange [-5:3]
set yrange [0:0.7]
set label "a" at 1,-0.1 center
set label "b" at 4,-0.1 center
set label "c" at 2,-0.1 center
set arrow from 0,0 to 0,0.7 nohead lt 0 lw 3
plot '-' notitle w l lw 5,\
 '-' notitle w boxes lw 5,\
 '-' notitle w boxes lw 5
-4 0
-1 0.33333
2 0
e
-4 0.01389
-3 0.11111
-2 0.22222
-1 0.30556
0 0.22222
1 0.11111
2 0.01389
e
0 0.64
1 0.32
2 0.04
e
