#!/bin/bash

rm -f ParseRes.*
cd Results
find -name "8299*results*.csv" -exec mv -t /gpfs0/shai/projects/COVID19/ {} +
find -name "8299*_beta_ma*" -exec mv -t /gpfs0/shai/projects/COVID19/ {} +
find -name "8299*_log*" -exec mv -t /gpfs0/shai/projects/COVID19/ {} +
find -name "*.e8299*" -exec mv -t /gpfs0/shai/projects/COVID19/ {} +
find -name "*.o8299*" -exec mv -t /gpfs0/shai/projects/COVID19/ {} +
