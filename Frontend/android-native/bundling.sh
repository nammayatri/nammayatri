#!/usr/bin/env bash
echo " ---------- Build/Bundle Customer :- --------------"

mkdir -p "android-native/app/src/user/assets/juspay"
rm ./android-native/app/src/user/assets/juspay/index_bundle.js
nix build .#android-customer-bundle -o android-native/app/src/user/assets/juspay/index_bundle.js
rm android-native/app/src/user/assets/juspay/juspay_assets.json
sh userJuspayAssets.sh

echo " ---------- Build/Bundle Driver :- --------------"

mkdir -p "android-native/app/src/driver/assets/juspay"
rm android-native/app/src/driver/assets/juspay/index_bundle.js
nix build .#android-driver-bundle -o android-native/app/src/driver/assets/juspay/index_bundle.js
rm android-native/app/src/driver/assets/juspay/juspay_assets.json
sh driverJuspayAssets.sh

