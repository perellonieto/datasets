#!/usr/bin/gnuplot

set datafile separator ","
class0=("< paste -d ',' features/test/ip1.csv db/test_label.csv | awk -F',' '{if($3 == '0') print}'")
class1=("< paste -d ',' features/test/ip1.csv db/test_label.csv | awk -F',' '{if($3 == '1') print}'")
#data=system("< paste -d ',' features/test/ip1.csv db/test_label.csv")

#set output "ip1_vs_label.pdf"
plot class0 using 1:2 t "class 0" with points pointtype 0 pointsize 5, \
     class1 using 1:2 t "class 1" with points pointtype 0 pointsize 2
pause mouse
