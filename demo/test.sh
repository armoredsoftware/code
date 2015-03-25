#!/bin/bash

i=0;
while [ 1 ]
do
  echo -n "" > testLog;
  for i in `seq 1 50`
  do
     echo $i >> testLog;
     sleep 2;
  done
done

