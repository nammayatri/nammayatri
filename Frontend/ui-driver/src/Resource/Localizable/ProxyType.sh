#!/bin/bash

# Function to process each line and convert it into the desired format
transform_proxies() {
    while IFS= read -r line; do
        # Remove leading pipe character and whitespace
        clean_line=$(echo "$line" | sed 's/| *//')
        
        # Extract the key (first word) and convert to lowercase
        key=$(echo "$clean_line" | awk '{print $1}')
        lower_key=$(echo "$key" | tr '[:upper:]' '[:lower:]')

        # Print in the desired format
        echo "$lower_key :: Proxy \"$lower_key\""
        echo "$lower_key = a"
        echo ""
    done < "$1"
}

# Input and output file paths
input_file=$1
output_file=$2

# Check if both input and output files are provided
if [[ -z $input_file || -z $output_file ]]; then
    echo "Usage: ./transform_proxies.sh <input_file> <output_file>"
    exit 1
fi

# Process the input file and write the output
transform_proxies "$input_file" > "$output_file"

echo "Transformed content written to $output_file"
