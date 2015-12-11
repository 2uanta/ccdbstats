#!/bin/bash -l
module load R/3.1.2 && cd /home/ccdbstats && R CMD BATCH cc_weekly_report.R

rsync -av -e ssh /home/ccdbstats/storage_usage/  cq@ccdbstats.sharcnet.ca:/data/stats/cq/storage_usage/2015/mcgill/
