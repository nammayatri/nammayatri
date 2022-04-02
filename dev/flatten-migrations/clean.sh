#!/bin/bash

SCHEMAS='atlas_parking atlas_public_transport atlas_fmd_wrapper atlas_app atlas_transporter atlas_registry'
TRASH_FOLDER=~/.beckn-trash

for schema in $SCHEMAS ; do
  mv $schema.diff $TRASH_FOLDER/
  mv $schema.old $TRASH_FOLDER/
  mv $schema.new $TRASH_FOLDER/
  cp -r $schema.mig $TRASH_FOLDER/
  rm -r $schema.mig
done

