set encoding iso_8859_1
set xrange [0:9]
set yrange [-4.5:4.5]
set size square
set xlabel "km (E-W)"
set ylabel "km (N-S)"
set arrow from 0,0 to 9,0 nohead lt 0
set arrow from 0,-4.5 to 0,4.5 nohead lt 0
set label "1\nA" at 1.3,-0.3 left
set label "2\nA" at 2.3,-0.3 left
set label "3\nA" at 3.3,-0.3 left
set label "4\nA" at 4.3,-0.3 left
set label "5\nB" at 5.3,-0.3 left
set label "6\nB" at 6.3,-0.3 left
set label "7\nB" at 7.3,-0.3 left
set label "8\nB" at 8.3,-0.3 left
plot '-' notitle w p lt 7 pt 71 ps 2, \
'-' notitle w p lt 7 pt 71 ps 2, \
'-' notitle w p lt 7 pt 71 ps 2, \
'-' notitle w p lt 7 pt 71 ps 2, \
'-' notitle w p lt 7 pt 71 ps 2, \
'-' notitle w p lt 7 pt 71 ps 2, \
'-' notitle w p lt 7 pt 71 ps 2, \
'-' notitle w p lt 7 pt 71 ps 2
1 0
e
2 0
e
3 0
e
4 0
e
5 0
e
6 0
e
7 0
e
8 0
e
