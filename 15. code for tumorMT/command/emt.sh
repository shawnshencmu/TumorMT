/usr/bin/perl /root/emt/command/quartile_norm.pl -c -1 -q 75 -t 1000 -o $4/$2-normalized.txt -s 1 $1 
/usr/bin/Rscript /root/emt/command/emt.R $1 $2 $3 $4 
