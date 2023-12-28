#!/usr/bin/env bash
if [ -z "$1" ]; then
  echo "src path is empty"
  exit 0
fi

echo "$1"
cd "$1" || return
echo "$2"


FILE="src/main/res/raw/juspay_$2_keep.xml"
DIR="src/main/res/raw"

if [ ! -d "$DIR" ]; then
    mkdir $DIR
fi


ans=""
for i in $(find src -name "drawable*"); do
  # This condition should not be present for static SDKs
  if [[ $i =~ "main" ]]; then
    echo "Processing: $i"
    for j in $(ls "$i"); do
      refined="@drawable/"$(echo "$j" | cut -d "." -f1)
      ans=${ans}${refined}","
    done
  fi
done

ans=${ans%,}","

layout=""
for i in $(find src -name "layout*"); do
  # This condition should not be present for static SDKs
  if [[ $i =~ "main" ]]; then
    echo "Processing: $i"
    for j in $(ls "$i"); do
      refined="@layout/"$(echo "$j" | cut -d "." -f1)
      layout=${layout}${refined}","
    done
  fi
done

ans=${ans}${layout%,}

touch "$FILE"

output="<?xml version=\"1.0\" encoding=\"utf-8\"?><resources xmlns:tools=\"http://schemas.android.com/tools\" tools:keep=\"$ans\" />"

echo "$output" >"$FILE"
