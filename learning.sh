#!/bin/sh
grep -E -e "$1 learns from .* that Image .* is|$1 sees .* and chooses to|$1 agrees with| agrees with $1" $log
