#!/bin/bash
SCHEMA=$1
LOCAL_DATA_FILE_REL=$2
LOCAL_DATA_DIR='../local-testing-data'
#LOCAL_DATA_FILE_REL=$(./schema_to_localdatafile.sh $SCHEMA)

LOCAL_DATA_FILE="$LOCAL_DATA_DIR/$LOCAL_DATA_FILE_REL"

grep "$SCHEMA\." $LOCAL_DATA_FILE | sed "s/.*$SCHEMA\.\([_a-z]\+\).*$/\1/" | sort | uniq
#grep "$SCHEMA\." $LOCAL_DATA_FILE | sed "s/.*$SCHEMA\.\([_a-z]\+\).*$/\1/" | awk '!seen[$0]++'

