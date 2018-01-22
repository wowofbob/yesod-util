#!/bin/bash

# Read file from server.
# Usage: ./read.sh <name>

PATH=$1

ROUTE="http://localhost:3000/file"

DATA='{"file":'\"$PATH\"'}'

CURL=/usr/bin/curl

$CURL -i -H "Accept: application/json" -X GET -d $DATA $ROUTE

echo ""
