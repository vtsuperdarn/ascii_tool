#!/bin/bash

rm mapplot_idl.tar.bz2
tar -cf mapplot_idl.tar mapplot_idl
bzip2 mapplot_idl.tar

scp -r mapplot_idl davit@davit.ece.vt.edu:/var/www/data/ascii/
scp -r mapplot_idl.tar.bz2 davit@davit.ece.vt.edu:/var/www/data/ascii/mapplot_idl.tar.bz2
scp -r rad_map_data2ascii.pro davit@davit.ece.vt.edu:/davit/lib/vt/idl/io/rad_map_data2ascii.pro

exit
