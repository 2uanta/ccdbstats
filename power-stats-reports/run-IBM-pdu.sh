#!/bin/bash -l
# (module load R && cd /home/qnguye/power-stats-reports && R CMD BATCH IBM-servers.R)
module load R/3.1.2 && cd /home/qnguye/power-stats-reports && R CMD BATCH ibm-pdu-t1-power-consumption-per-hour.R

module load R/3.1.2 && cd /home/qnguye/power-stats-reports && R CMD BATCH ibm-pdu-t2-power-consumption-per-hour.R

echo "Daily Power Consumption for some PDUs" | mailx -s power-stats-pdu-report -a ibm-pdu-t1-power-consumption-per-hour.Rout -a ibm-pdu-t1-power-consumption.pdf -a ibm-pdu-t2-power-consumption-per-hour.Rout -a ibm-pdu-t2-power-consumption.pdf  quan.nguyen@calculquebec.ca
