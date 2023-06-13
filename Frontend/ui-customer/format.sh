#!/bin/bash

find src -type f -name "*.purs" -print0 | while IFS= read -r -d '' file; do
  purty format --write "$file"
done
