#!/bin/bash

git fetch origin

# Using git to get the changed SQL files
changed_files=$(git diff --name-only origin/$GITHUB_BASE_REF..origin/$GITHUB_HEAD_REF | grep ".sql$")

for file in $changed_files; do
    if grep -q "DROP TABLE\|DROP COLUMN\|NOT NULL\|ALTER COLUMN\|DROP PRIMARY KEY" "$file"; then
        echo "Potential backward incompatible DB change detected in $file"
        exit 1
    fi
done
