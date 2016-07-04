#!/bin/bash

TEMP=20
COUNT=0
VBB=0

while read line; do

   echo "Analizando $line"
   eval "root -l -b -q 'comp.C+g(\""$line"\","$TEMP","$VBB")'"

   if [ "$COUNT" -eq 9 ]; then
   VBB=$((VBB+3))
   COUNT=0
   fi
   COUNT=$((COUNT+1))
   if [ "$VBB" -eq 9 ]; then
   TEMP=$((TEMP+5))
   VBB=0
   fi   

done < Directorio.dat
