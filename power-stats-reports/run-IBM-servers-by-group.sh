#!/bin/bash -l
# (module load R && cd /home/qnguye/power-stats-reports && R CMD BATCH IBM-servers.R)
module load R/3.1.2 && cd /home/qnguye/power-stats-reports && R CMD BATCH IBM-servers-by-group-power-consumption-since-beginning.R

echo "Daily Power Consumption By Group" | mailx -s power-stats-by-group-report -a IBM-servers-by-group-power-consumption-since-beginning.Rout -a IBM-servers-by-group-power-consumption.pdf quan.nguyen@calculquebec.ca
