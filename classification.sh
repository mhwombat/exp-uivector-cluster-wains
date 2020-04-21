#!/bin/sh

source ./config.sh
grep " agrees" $log | \
  sed 's/.*\t\([0-9]*\)\t/\1,/; s/,.* that /,/; s/ has label /,/' | \
  grep Image | \
  grep -E -v -e 'anomaly|squiggle' | \
  grep '^[0-9][0-9][0-9]' | \
  sed 's/_[0-9]*.png//; s/Image //; s/^\([0-9]*\)[0-9][0-9]/\1xx/; s/.png//' | \
head -n 5000 | \
  sort | \
  uniq -c
#  sed 's/.png//' | \
# grep -E -v -e 'anomaly|squiggle' | \
#  uniq -c |\
#  sort -nr
