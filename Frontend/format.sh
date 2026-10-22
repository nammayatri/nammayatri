#!/usr/bin/env sh NOTE::- Updating the script to run on all the files in the repo first via bot
purscriptFiles=purscriptFiles=$(find . -type d -name "src" -exec find {} -type f -name "*.purs" \;) # $(git diff --cached --name-only --diff-filter=ACM | grep ".purs$" || true;)
jsFiles=purscriptFiles=$(find . -type d -name "src" -exec find {} -type f -name "*.purs" \;) # $(git diff --cached --name-only --diff-filter=ACM | grep '.js$' || true;)
currentDir=$(pwd)
# Will Enable the pre-commit hook for purs files later
for file in $purscriptFiles
do
  basePath=$(echo $file | cut -f 2 -d "/")
  filePath=$(echo $file | cut -d "/" -f 3-)
  if [ $(echo $file | cut -f 2 -d "/") == "ui-common" ]; then
  basePath="ui-customer"
  filePath=$"ui-common/$filePath"
  fi
  cd $basePath
  echo "Formatting File $file"
  npx purty format --write "$filePath"
  cd $currentDir
done
