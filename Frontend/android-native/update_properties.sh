#!/bin/bash

cd android-native/app
echo $1 > google-services.json
cd ..
echo $2 > merch_config.json

if ! command -v jq &> /dev/null; then
    echo "jq not found. Installing..."
    sudo apt-get update
    sudo apt-get install jq -y
fi


json_file="merch_config.json"

if [ ! -f "$json_file" ]; then
    echo "JSON file not found: $json_file"
    exit 1
fi

json_data=$(cat "$json_file")

variant=$3
case $variant in
    YatriDriverProdDebug|YatriUserProdDebug)
        config=$(echo "$json_data" | jq -r '.yatri')
        ;;
    NyUserProdDebug|NyDriverProdDebug)
        config=$(echo "$json_data" | jq -r '.nammayatri')
        ;;
    YsUserProdDebug|YsDriverProdDebug)
        config=$(echo "$json_data" | jq -r '.ys')
        ;;
    *)
        echo "Invalid variant provided: $variant"
        exit 1
        ;;
esac

configUrlDriver=$(echo "$config" | jq -r '.configUrlDriver')
configUrlUser=$(echo "$config" | jq -r '.configUrlUser')
mapKey=$(echo "$config" | jq -r '.mapKey')
merchantIdUser=$(echo "$config" | jq -r '.merchantIdUser')
merchantIdDriver=$(echo "$config" | jq -r '.merchantIdDriver')

echo "CONFIG_URL_DRIVER=\"$configUrlDriver\"" >> local.properties
echo "CONFIG_URL_USER=\"$configUrlUser\"" >> local.properties
echo "MAPS_API_KEY=\"$mapKey\"" >> local.properties
echo "MERCHANT_ID_USER=\"$merchantIdUser\"" >> local.properties
echo "MERCHANT_ID_DRIVER=\"$merchantIdDriver\"" >> local.properties
