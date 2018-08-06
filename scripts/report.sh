#!/bin/sh
slax p | awk 'BEGIN {last=""}{if (last!=$1) {print $1 " " NR;last=$1;}}'
