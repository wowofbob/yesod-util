#!/bin/bash

# Echo.
# Usage: ./echo.sh <text>

TEXT=$1

ROUTE="http://localhost:3000/echo"

DATA='{"text":'\"$TEXT\"'}'

CURL=/usr/bin/curl

$CURL -i -H "Accept: application/json" -X POST -d $DATA $ROUTE

echo ""
