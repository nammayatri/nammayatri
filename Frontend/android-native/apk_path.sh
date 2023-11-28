#!/bin/bash

variant=$1

case $variant in
  "YatriDriverProdDebug")
    path="Frontend/android-native/app/build/outputs/apk/yatriDriverProd/debug/app-yatriDriver-prod-debug.apk"
    ;;
  "YatriUserProdDebug")
    path="Frontend/android-native/app/build/outputs/apk/yatriUserProd/debug/app-yatriUser-prod-debug.apk"
    ;;
  "NyUserProdDebug")
    path="Frontend/android-native/app/build/outputs/apk/nyUserProd/debug/app-nyUser-prod-debug.apk"
    ;;
  "NyDriverProdDebug")
    path="Frontend/android-native/app/build/outputs/apk/nyDriverProd/debug/app-nyDriver-prod-debug.apk"
    ;;
  "YsUserProdDebug")
    path="Frontend/android-native/app/build/outputs/apk/ysUserProd/debug/app-ysUser-prod-debug.apk"
    ;;
  "YsDriverProdDebug")
    path="Frontend/android-native/app/build/outputs/apk/ysDriverProd/debug/app-ysDriver-prod-debug.apk"
    ;;
  *)
    echo "Unknown variant: $variant"
    exit 1
    ;;
esac

# Create or overwrite the path.txt file
echo "$path" > path.txt
