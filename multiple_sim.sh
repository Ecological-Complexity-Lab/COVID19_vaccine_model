#!/bin/bash


# for element in {16..18}
# do
#  echo Experiment submitted: $element;
#  qsub run_sim.sh $element
# done

for j in {8299548..8299562}
do
  echo Job parsed: $j;
  qsub parse_sim.sh $j 15
done

for j in {8299615..8299617}
do
  echo Job parsed: $j;
  qsub parse_sim.sh $j 24
done
