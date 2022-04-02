#!/bin/bash
SCHEMA=$1
ACTION=$2

if [ $ACTION = test_data_file ] ; then
  if [ $SCHEMA = atlas_app ] ; then
    echo "app-backend.sql"
  elif [ $SCHEMA = atlas_transporter ] ; then
    echo "beckn-transport.sql"
  elif [ $SCHEMA = atlas_fmd_wrapper ] ; then
    echo "fmd-wrapper.sql"
  elif [ $SCHEMA = atlas_registry ] ; then
    echo "mock-registry.sql"
  elif [ $SCHEMA = atlas_parking ] ; then
    echo "parking-bap.sql"
  elif [ $SCHEMA = atlas_public_transport ] ; then
    echo "public-transport-bap.sql"
  fi
fi

if [ $ACTION = extra_test_data_tables ] ; then
  if [ $SCHEMA = atlas_app ] ; then
    echo 'location_backup search_request_location'
  elif [ $SCHEMA = atlas_transporter ] ; then
    echo 'driver_location fare_policy_discount location_backup fare_policy_per_extra_km_rate'
  elif [ $SCHEMA = atlas_fmd_wrapper ] ; then
    echo ''
  elif [ $SCHEMA = atlas_registry ] ; then
    echo ''
  elif [ $SCHEMA = atlas_parking ] ; then
    echo ''
  elif [ $SCHEMA = atlas_public_transport ] ; then
    echo ''
  fi
fi

if [ $ACTION = seed_file ] ; then
  if [ $SCHEMA = atlas_app ] ; then
    echo 'app-backend-seed.sql'
  elif [ $SCHEMA = atlas_transporter ] ; then
    echo 'transporter-backend-seed.sql'
  elif [ $SCHEMA = atlas_fmd_wrapper ] ; then
    echo 'fmd-wrapper-backend-seed.sql'
  elif [ $SCHEMA = atlas_registry ] ; then
    echo 'mock-registry-seed.sql'
  elif [ $SCHEMA = atlas_parking ] ; then
    echo 'parking-bap-seed.sql'
  elif [ $SCHEMA = atlas_public_transport ] ; then
    echo 'public-transport-bap-seed.sql'
  fi
fi

if [ $ACTION = migration_dir ] ; then
  if [ $SCHEMA = atlas_app ] ; then
    echo 'app-backend'
  elif [ $SCHEMA = atlas_transporter ] ; then
    echo 'beckn-transport'
  elif [ $SCHEMA = atlas_fmd_wrapper ] ; then
    echo 'fmd-wrapper'
  elif [ $SCHEMA = atlas_registry ] ; then
    echo 'mock-registry'
  elif [ $SCHEMA = atlas_parking ] ; then
    echo 'parking-bap'
  elif [ $SCHEMA = atlas_public_transport ] ; then
    echo 'public-transport-bap'
  fi
fi



#if [ $ACTION = test_data ] ; then
#  if [ $SCHEMA = atlas_app ] ; then
#    echo
#  elif [ $SCHEMA = atlas_transporter ] ; then
#    echo
#  elif [ $SCHEMA = atlas_fmd_wrapper ] ; then
#    echo
#  elif [ $SCHEMA = atlas_registry ] ; then
#    echo
#  elif [ $SCHEMA = atlas_parking ] ; then
#    echo
#  elif [ $SCHEMA = atlas_public_transport ] ; then
#    echo
#  fi
#fi


