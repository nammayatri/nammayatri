#!/bin/bash
SCHEMA=$1
CONN_STRING="host=localhost port=5434 user=atlas dbname=atlas_dev password=atlas"

SCHEMA_FILE="$SCHEMA.insert.sql"
#LOCAL_DATA_DIR='../local-testing-data/'
LOCAL_DATA_FILE_REL=$(./schema_info.sh $SCHEMA test_data_file)

LOCAL_DATA_FILE="$LOCAL_DATA_DIR/$LOCAL_DATA_FILE_REL"

TABLES_TO_FETCH=$(grep "$SCHEMA\." $LOCAL_DATA_FILE | sed "s/.*INTO\s\s*$SCHEMA\.\([_a-z]\+\).*$/\1/" | sort | uniq)
#TABLES_TO_FETCH=$(grep "$SCHEMA\." $LOCAL_DATA_FILE | sed "s/.*$SCHEMA\.\([_a-z]\+\).*$/\1/" | awk '!seen[$0]++')
EXTRA_TABLES_TO_FETCH=$(./schema_info.sh $SCHEMA extra_test_data_tables)

TABLES_TO_FETCH_FLAGS=''
for fettable in $TABLES_TO_FETCH; do
  TABLES_TO_FETCH_FLAGS="$TABLES_TO_FETCH_FLAGS -t $SCHEMA.$fettable"
done

for fettable in $EXTRA_TABLES_TO_FETCH; do
  TABLES_TO_FETCH_FLAGS="$TABLES_TO_FETCH_FLAGS -t $SCHEMA.$fettable"
done

rm $SCHEMA_FILE
pg_dump "$CONN_STRING" -ax --column-inserts --rows-per-insert=10000 --schema=$SCHEMA -T "$SCHEMA.schema_migrations" $TABLES_TO_FETCH_FLAGS \
  | grep -i -v -e '^--' -e '^set' -e 'pg_catalog' -e '^$' >> $SCHEMA_FILE

