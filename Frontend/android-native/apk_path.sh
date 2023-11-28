#!/bin/bash

# Check if the number of arguments is correct
if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <variant>"
  exit 1
fi

variant=$1

case $variant in
  "yatriDriverProdDebug")
    path="yatriDriverProd/debug/app-yatriDriver-prod-debug.apk"
    ;;
  "yatriUserProdDebug")
    path="yatriUserProd/debug/app-yatriUser-prod-debug.apk"
    ;;
  "nyUserProdDebug")
    path="nyUserProd/debug/app-nyUser-prod-debug.apk"
    ;;
  "nyDriverProdDebug")
    path="nyDriverProd/debug/app-nyDriver-prod-debug.apk"
    ;;
  "ysUserProdDebug")
    path="ysUserProd/debug/app-ysUser-prod-debug.apk"
    ;;
  "ysDriverProdDebug")
    path="ysDriverProd/debug/app-ysDriver-prod-debug.apk"
    ;;
  *)
    path="unknown_variant/debug/app-unknown-variant-debug.apk"
    ;;
esac

# Create or overwrite the path.txt file
echo "$path" > path.txt
