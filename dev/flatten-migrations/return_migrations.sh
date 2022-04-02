#!/bin/bash

SCHEMAS='atlas_parking atlas_public_transport atlas_fmd_wrapper atlas_app atlas_transporter atlas_registry'

for schema in $SCHEMAS ; do
  SCHEMA_MIGRATIONS_DIR=$(./schema_info.sh $schema migration_dir)
  for migfile in $(ls $schema.mig) ; do
    mv $schema.mig/$migfile $MIGRATIONS_DIR/$SCHEMA_MIGRATIONS_DIR/
  done
done

