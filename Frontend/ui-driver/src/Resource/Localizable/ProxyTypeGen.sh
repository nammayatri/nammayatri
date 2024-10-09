#!/bin/bash

# Function to process each line and convert it into the desired format
transform_types() {
    while IFS= read -r line; do
        # Remove leading pipe character and whitespace
        clean_line=$(echo "$line" | sed 's/| *//')
        
        # Extract the key (first word)
        key=$(echo "$clean_line" | awk '{print $1}')
        # Count the number of words in the line
        word_count=$(echo "$clean_line" | awk '{print NF}')

        # Convert key to lowercase for output
        lower_key=$(echo "$key" | tr '[:upper:]' '[:lower:]')

        # Create type declaration based on the number of words
        if [[ $word_count -gt 1 ]]; then
            type_declaration=$(printf 'String -> %.0s' $(seq 1 $((word_count - 1)))) # Append String for each additional word
            type_declaration+="String"
        else
            type_declaration="String"
        fi

        # Print in the desired format
        echo "$lower_key:: $type_declaration,"
    done < "$1"
}

# Input and output file paths
input_file=$1
output_file=$2

# Check if both input and output files are provided
if [[ -z $input_file || -z $output_file ]]; then
    echo "Usage: ./transform_types.sh <input_file> <output_file>"
    exit 1
fi

# Process the input file and write the output
transform_types "$input_file" > "$output_file"

echo "Transformed content written to $output_file"
