#!/bin/sh
./trendSummary.sh | csvcut -c  1,170,172,174,180,327 | ./head-tail 1 10 | csvlook
