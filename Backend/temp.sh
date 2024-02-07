#!/bin/bash

# Command to run
command_to_run="cabal build all"

# Expected output to stop the loop
expected_output="Up to date"

# Run the command in a loop until expected output is received
while true; do
    # Run the command and capture the output
    output=$(eval "$command_to_run")

    # Print the output
    echo "$output"

    # Check if the output contains the expected string
    if [[ "$output" == *"$expected_output"* ]]; then
        echo "Received expected output. Exiting loop."
        break
    fi

    # Adjust the delay between iterations as needed (e.g., 1 second)
    sleep 1
done

echo "Script finished"

