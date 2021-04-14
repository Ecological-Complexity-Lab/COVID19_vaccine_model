#!/bin/bash


for element in {1..15}
do
 echo Experiment submitted: $element;
 qsub run_sim.sh $element
done

# for j in 8008193 8013487 8033954 8033955 8033956 8033957 8033958 8033959 8033960 8033961 8033962 8033963 8033964 8034495 8034496 8034497 8034498 8034499
# do
#   echo Job parsed: $j;
#   qsub parse_sim.sh $j 15
# done
