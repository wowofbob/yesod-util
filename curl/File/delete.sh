#!/bin/bash

# Delete file on server.
# Usage: ./delete.sh <name>

PATH=$1

ROUTE="http://localhost:3000/file"

DATA='{"file":'\"$PATH\"'}'

CURL=/usr/bin/curl

$CURL -i -H "Accept: application/json" -X DELETE -d $DATA $ROUTE

echo ""
