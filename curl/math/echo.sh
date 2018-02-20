#!/bin/bash

# Echo.
# Usage: ./math.sh [plus|minus] <integer> <integer>

OpType=$1
NmLeft=$2
NmRight=$3

ROUTE="http://localhost:3000"

DATA='{"type":'\"$OpType\"',"left":'$NmLeft',"right":'$NmRight'}'

CURL=/usr/bin/curl

$CURL -i -H "Accept: application/json" -X POST -d $DATA $ROUTE

echo ""
