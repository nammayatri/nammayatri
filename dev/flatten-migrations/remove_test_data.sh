#!/bin/bash

LOCAL_DATA_DIR='../local-testing-data'
for testdatafile in $(ls $LOCAL_DATA_DIR | grep '\.sql$'); do
  mv $LOCAL_DATA_DIR/$testdatafile $LOCAL_DATA_DIR/$testdatafile.bak
  touch $LOCAL_DATA_DIR/$testdatafile
done

