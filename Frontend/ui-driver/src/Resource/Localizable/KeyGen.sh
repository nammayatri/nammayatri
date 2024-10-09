#!/bin/bash

# Function to process each line and convert it into the desired format
transform_keys() {
    while IFS= read -r line; do
        # Remove leading pipe character and whitespace
        clean_line=$(echo "$line" | sed 's/| *//')
        
        # Extract the key (first word) and check for additional type information
        key=$(echo "$clean_line" | awk '{print $1}')
        
        # Count the number of words
        word_count=$(echo "$clean_line" | awk '{print NF}')

        # Prepare the underscore based on the number of words
        type_info=""
        while [[ $word_count -gt 1 ]]; do
            type_info="${type_info} _"
            word_count=$((word_count - 1))
        done 
        # if [[ $word_count -gt 1 ]]; then
        #     # If there are additional words, we only need one underscore
        #     type_info="_"
        # else
            
        # fi

        # Convert key to lowercase for output
        lower_key=$(echo "$key" | tr '[:upper:]' '[:lower:]')
        
        # Print in the desired format, adding the underscore only when needed
        echo "$key$type_info-> ${lower_key// /_}"
    done < "$1"
}

# Input and output file paths
input_file=$1
output_file=$2

# Check if both input and output files are provided
if [[ -z $input_file || -z $output_file ]]; then
    echo "Usage: ./transform_keys.sh <input_file> <output_file>"
    exit 1
fi

# Process the input file and write the output
transform_keys "$input_file" > "$output_file"

echo "Transformed content written to $output_file"
