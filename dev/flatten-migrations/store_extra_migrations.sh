#!/bin/bash

SCHEMAS='atlas_public_transport atlas_fmd_wrapper atlas_app atlas_transporter atlas_registry'

for schema in $SCHEMAS ; do
  mkdir $schema.mig
  SCHEMA_MIGRATIONS_DIR=$(./schema_info.sh $schema migration_dir)
  for migfile in $(cat $schema.diff) ; do
    mv $MIGRATIONS_DIR/$SCHEMA_MIGRATIONS_DIR/$migfile $schema.mig
  done
done

