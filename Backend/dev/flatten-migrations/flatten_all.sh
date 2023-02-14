#!/bin/bash

#SQL_SEED_DIR='../sql-seed/'
#LOCAL_DATA_DIR='../local-testing-data'
#MIGRATIONS_DIR='.'
ACTION=_$1

echo $SQL_SEED_DIR
echo $LOCAL_DATA_DIR
echo $MIGRATIONS_DIR


SCHEMAS='atlas_public_transport atlas_fmd_wrapper atlas_app atlas_transporter atlas_registry'

if [ $ACTION = _fetch_inserts ] ; then
  echo 'start fetching test data'
# fetch test data


for schema in $SCHEMAS; do
  SCHEMA=$schema
  ./fetch_inserts.sh $SCHEMA
  TEST_DATA_FILE=$(./schema_info.sh $SCHEMA test_data_file)
  mv "$LOCAL_DATA_DIR/$TEST_DATA_FILE" "$LOCAL_DATA_DIR/$TEST_DATA_FILE.old"
  mv "$SCHEMA.insert.sql" "$LOCAL_DATA_DIR/$TEST_DATA_FILE"
done

echo 'end fetching test data'

elif [ $ACTION = _flatten ] ; then
echo 'start flattening'

for schema in $SCHEMAS; do
  SCHEMA=$schema
  SEED_FILE=$(./schema_info.sh $SCHEMA seed_file)
  SCHEMA_MIGRATIONS_DIR=$(./schema_info.sh $SCHEMA migration_dir)
  ./flatten.sh $SCHEMA
  mv "$SCHEMA.sql" "$SQL_SEED_DIR/$SEED_FILE"
  rm $MIGRATIONS_DIR/$SCHEMA_MIGRATIONS_DIR/*
done

echo 'end flattening'

fi
