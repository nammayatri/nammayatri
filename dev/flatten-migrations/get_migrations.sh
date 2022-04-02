#!/bin/bash

SCHEMA=$1
SUFFIX=$2

SCHEMA_MIGRATIONS_DIR=$(./schema_info.sh $SCHEMA migration_dir)
OUTPUT_FILE="$SCHEMA.$SUFFIX"

MIG_FILES=$(ls $MIGRATIONS_DIR/$SCHEMA_MIGRATIONS_DIR)

rm $OUTPUT_FILE
touch $OUTPUT_FILE
for file in $MIG_FILES ; do
  echo $file >> $OUTPUT_FILE
done
