#!/bin/bash

# Fetch all branches
git fetch origin

# Using git to get the changed SQL files
changed_files=$(git diff --name-only origin/$GITHUB_BASE_REF..origin/$GITHUB_HEAD_REF | grep ".sql$")

for file in $changed_files; do
    # This will print filename:linenumber:detected issue
    grep -v -- '--' "$file" | grep -n "DROP TABLE\|DROP COLUMN\|ALTER COLUMN\|DROP PRIMARY KEY" | while read -r line; do
        echo "$file:$line:Potential backward incompatible DB change detected"
    done
done
