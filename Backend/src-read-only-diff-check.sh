#!/bin/bash

, run-generator
# This script checks if there are any changes in the src-read-only folder
CHANGES_FOUND=false

git diff --unified=0 -- | grep -E "^\\+\\+\\+|^@@" | while read -r line; do
    file_path=$(grep -oP "^\\+\\+\\+ b/\K(.+)" <<< "$line")
    if [[ $file_path == *"/src-read-only/"* ]]; then
        git diff --unified=0 --relative="$file_path" | grep -E "^@@" | while read -r diff_line; do
            line_number=$(grep -oP "@@\\s-\\d+(,\d+)?\\s+\\+\\K\d+" <<< "$diff_line")
            echo $file_path:$line_number:"Changes found in src-read-only folder with diff line: $diff_line"
            CHANGES_FOUND=true
        done
    fi
done

git checkout -- .

if [ "$CHANGES_FOUND" = true ]; then
    exit 1
else
    exit 0

fi
