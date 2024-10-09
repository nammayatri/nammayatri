#!/bin/bash

# Function to process each line and convert it into the desired format
convert_string() {
    while IFS= read -r line; do
        # Use sed to extract the parts of the line
        string_name=$(echo "$line" | sed -E 's/^([^ ]+)[ ]+.*->.*$/\1/' | tr '[:upper:]' '[:lower:]') # Extract the string name and convert to lowercase
        args=$(echo "$line" | sed -E 's/^[^ ]+[ ]+([^->]*)->.*/\1/' | xargs) # Extract the arguments and trim whitespace
        text=$(echo "$line" | sed -E 's/.*->[ ]?"(.*)"/\1/') # Extract the text after -> and remove extra spaces

        # Check if there are arguments
        if [[ -n $args ]]; then
            lambda_args=$(echo "$args" | xargs) # Trim whitespace in arguments
            echo ", $string_name : (\\$lambda_args -> \"$text\")"
        else
            echo ", $string_name : \"$text\""
        fi
    done < "$1"
}

# Input and output file paths
input_file=$1
output_file=$2

# Check if both input and output files are provided
if [[ -z $input_file || -z $output_file ]]; then
    echo "Usage: ./convert.sh <input_file> <output_file>"
    exit 1
fi

# Process the input file and write the output
convert_string "$input_file" > "$output_file"

echo "Converted content written to $output_file"
