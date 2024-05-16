#!/bin/bash
set -e

postman_env="Dev.postman_environment.json"
commit_id=$(git rev-parse --short HEAD)

runPreTest() {
  folder=$1
  # Check if the pre-test.sh script exists in the folder
  if [ -f "$folder/pre-test.sh" ]; then
    echo "Running pre-test.sh"
    # Run the pre-test.sh script
    bash "$folder/pre-test.sh"
  fi
}

runPostTest() {
  folder=$1
  # Check if the post-test.sh script exists in the folder
  if [ -f "$folder/post-test.sh" ]; then
    echo "Running post-test.sh"
    # Run the post-test.sh script
    bash "$folder/post-test.sh"
  fi
}

runNewman() {
  folder=$1
  # Count the number of .json files in the directory
  json_files_count=$(find "$folder" -maxdepth 1 -type f -name "*.json" | wc -l)

  # If there are multiple .json files in the directory, throw an error
  if [ "$json_files_count" -gt 1 ]; then
    echo "Error: Multiple .json files found in directory '$folder'."
    exit 1
  fi

  # Get the .json file in the directory
  json_file=$(find "$folder" -maxdepth 1 -type f -name "*.json")

  # Run the newman command
  newman run "$json_file" -e $postman_env --bail --timeout-request 1000
}

# Check if Newman CLI is installed
if ! command -v newman &> /dev/null; then
  echo "Newman CLI is not installed."
fi

# Get the directory path from user input
directory="./tests"

# Check if the directory exists
if [ ! -d "$directory" ]; then
  echo "Error: Directory '$directory' not found."
  exit 1
fi

set +e
test_counter=0
test_pass_counter=0
passed_tests=()
failed_tests=()
# Iterate over each folder in the specified directory
for folder in "$directory"/*/; do
  echo "Running test case - $folder"
  test_counter=$((test_counter+1))

  runPreTest "$folder" && \
  runNewman "$folder" && \
  runPostTest "$folder" && \
  test_pass_counter=$((test_pass_counter+1)) && \
  passed_tests+=("$folder") || failed_tests+=("$folder")

done

echo
echo
echo "Tests completed."
echo
if [ ! $test_counter -eq $test_pass_counter ]; then
  echo -e "$commit_id \033[31mTests failed: $test_pass_counter/$test_counter passed."
  echo "Failed tests:"
  for test in "${failed_tests[@]}"; do
    echo "- $test"
  done
  echo -e "\033[00m";
else
  echo -e "$commit_id \033[32mTests $test_pass_counter/$test_counter passed."
  echo "Passed tests:"
  for test in "${passed_tests[@]}"; do
    echo "- $test"
  done
  echo -e "\033[00m";
fi