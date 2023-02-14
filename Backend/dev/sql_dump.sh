#!/bin/bash

SCHEMA=$1
TABLE=$2

CONN_STRING="host=localhost port=5434 user=atlas dbname=atlas_dev password=atlas"

pg_dump "$CONN_STRING" -ax --column-inserts --rows-per-insert=10000 --schema=$SCHEMA -t "$SCHEMA.$TABLE" \
  | grep -i -v -e '^--' -e '^set' -e 'pg_catalog' -e '^$'
