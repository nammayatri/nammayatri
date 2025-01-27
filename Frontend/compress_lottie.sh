#!/bin/bash

repo_root=$(git rev-parse --show-toplevel)

# Function to compress a JSON file
compress_json() {
    local file="$1"
    # Use jq to compress the JSON file
    if jq -c . "$file" > tmp.json; then
        mv tmp.json "$file"
        echo "Compressed: $file"
    else
        echo "Failed to compress: $file"
    fi
}

# Function to find and compress JSON files from git diff
find_and_compress_git_diff_files() {
    local commit_range="$1"

    # Get the list of changed files in the latest commit matching the pattern
    local changed_files
    # Get the list of changed JSON files matching the specific pattern
    changed_files=$(git diff --name-only HEAD~1 HEAD | grep '^Frontend/android-native/[^/]*/src/[^/]*/res/raw/.*\.json$')

    if [[ -z "$changed_files" ]]; then
        echo "No JSON files found to compress in the latest commit."
        return
    fi

    echo "$changed_files" | while read -r json_file; do    
        echo "Checking file: '$json_file'"
        full_path="$repo_root/$json_file"
        if [[ -f "$full_path" ]]; then
            compress_json "$full_path"
        else
            echo "File not found: $full_path"
        fi
    done
}

# Main
commit_range=$1  # Commit range is passed as the first argument to the script

if ! git rev-parse --is-inside-work-tree &>/dev/null; then
    echo "Error: Not inside a Git repository."
    exit 1
fi

if [[ -z "$commit_range" ]]; then
    echo "Error: Commit range not specified."
    exit 1
fi

find_and_compress_git_diff_files "$commit_range"
