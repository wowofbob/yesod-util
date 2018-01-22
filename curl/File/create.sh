#!/bin/bash

# Create file on server.
# Usage: ./create.sh <name> <contents>

PATH=$1
TEXT=$2

ROUTE="http://localhost:3000/file"

DATA='{"file":'\"$PATH\"',"text":'\"$TEXT\"'}'

CURL=/usr/bin/curl

$CURL -i -H "Accept: application/json" -X PUT -d $DATA $ROUTE

echo ""
