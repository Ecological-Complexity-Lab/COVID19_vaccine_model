#!/bin/bash

rm -f ParseRes.*
cd Results
find -name "8*results*.csv" -exec mv -t /gpfs0/shai/projects/COVID19/ {} +
find -name "8*_beta_ma*" -exec mv -t /gpfs0/shai/projects/COVID19/ {} +
find -name "8*_log*" -exec mv -t /gpfs0/shai/projects/COVID19/ {} +
find -name "*.e8*" -exec mv -t /gpfs0/shai/projects/COVID19/ {} +
find -name "*.o8*" -exec mv -t /gpfs0/shai/projects/COVID19/ {} +
rm -rf *.*