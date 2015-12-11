#!/bin/bash -l
# (module load R && cd /home/qnguye/power-stats-reports && R CMD BATCH IBM-servers.R)
module load R/3.1.2 && cd /home/qnguye/power-stats-reports && R CMD BATCH IBM-servers-power-consumption-since-beginning.R

module load R/3.1.2 && cd /home/qnguye/power-stats-reports && R CMD BATCH MGE-UPS-consumption-per-hour.R

#echo "Daily Power Consumption for all IBM Servers" | mailx -s power-stats-report -a IBM-servers-power-consumption-since-beginning.Rout -a IBM-servers-power-consumption.pdf -a MGE-UPS-consumption-per-hour.Rout -a MGE-UPS-power-consumption.pdf  quan.nguyen@calculquebec.ca
echo "Daily Power Consumption for all IBM Servers" | mailx -s power-stats-report -a IBM-servers-power-consumption-since-beginning.Rout -a IBM-servers-power-consumption.pdf -a MGE-UPS-consumption-per-hour.Rout -a MGE-UPS-power-consumption.pdf  quan.nguyen@calculquebec.ca,bryan.caron@calculquebec.ca
