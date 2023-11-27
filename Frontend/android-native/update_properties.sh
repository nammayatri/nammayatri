#!/bin/bash

touch local.properties

echo "CONFIG_URL_DRIVER=\\"$1\\"" >> local.properties
echo "CONFIG_URL_USER=\\"$2\\"" >> local.properties
echo "MAP_KEY=\\"$3\\"" >> local.properties
echo "MERCHANT_ID_USER=$4" >> local.properties
echo "MERCHANT_ID_DRIVER=$5" >> local.properties
