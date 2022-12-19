#!/usr/bin/env bash

MIGDIR=$1

MIGFILES=$(ls -q $MIGDIR)
CONN_STRING="host=localhost port=5434 user=postgres dbname=atlas_dev password=root"

for migfile in $MIGFILES ; do
  psql "$CONN_STRING" -f $MIGDIR/$migfile
done
