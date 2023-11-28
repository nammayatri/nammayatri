#!/bin/bash

# Install jq if not already installed
if ! command -v jq &> /dev/null; then
    echo "jq not found. Installing..."
    sudo apt-get update
    sudo apt-get install jq -y
fi

# Get the JSON data from the file in the same directory as the script
script_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
json_file="$script_dir/merch_config.json"

# Check if the JSON file exists
if [ ! -f "$json_file" ]; then
    echo "JSON file not found: $json_file"
    exit 1
fi

# Read JSON data from the file
json_data=$(cat "$json_file")

# Get values based on the provided variant
variant=$1
case $variant in
    yatriDriverProdDebug|yatriUserProdDebug)
        config=$(echo "$json_data" | jq -r '.yatri')
        ;;
    nyUserProdDebug|nyDriverProdDebug)
        config=$(echo "$json_data" | jq -r '.nammayatri')
        ;;
    ysUserProdDebug|ysDriverProdDebug)
        config=$(echo "$json_data" | jq -r '.ys')
        ;;
    *)
        echo "Invalid variant provided: $variant"
        exit 1
        ;;
esac

# Extract individual values
configUrlDriver=$(echo "$config" | jq -r '.configUrlDriver')
configUrlUser=$(echo "$config" | jq -r '.configUrlUser')
mapKey=$(echo "$config" | jq -r '.mapKey')
merchantIdUser=$(echo "$config" | jq -r '.merchantIdUser')
merchantIdDriver=$(echo "$config" | jq -r '.merchantIdDriver')

# Update local.properties
echo "CONFIG_URL_DRIVER=\"$configUrlDriver\"" >> android-native/app/local.properties
echo "CONFIG_URL_USER=\"$configUrlUser\"" >> android-native/app/local.properties
echo "MAP_KEY=\"$mapKey\"" >> android-native/app/local.properties
echo "MERCHANT_ID_USER=\"$merchantIdUser\"" >> android-native/app/local.properties
echo "MERCHANT_ID_DRIVER=\"$merchantIdDriver\"" >> android-native/app/local.properties
