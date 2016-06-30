#!/bin/bash

while read line; do
    
   echo "Analizando $line"
   eval "root -l -b -q 'Threshold.C+g(\""$line"\")'"
   
done < Directorio.dat
