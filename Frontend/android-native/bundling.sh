#!/bin/bash
echo " ---------- Customer prod:android :- --------------"
cd ./../ui-customer
bower i
npm run prod:android
cd ./../android-native

echo " ---------- Copy index_bundle.js Customer :- --------------"

mkdir -p "app/src/user/assets/juspay"
rm -rf app/src/user/assets/juspay/index_bundle.js
cp ./../ui-customer/dist/android/index_bundle.js app/src/user/assets/juspay
rm -rf app/src/user/assets/juspay/juspay_assets.json
cd ..
sh userJuspayAssets.sh

echo " ---------- Driver prod:android :- --------------"

cd ui-driver
bower i
npm run prod:android
cd ./../android-native

echo " ---------- Copy index_bundle.js Driver :- --------------"

mkdir -p "app/src/driver/assets/juspay"
rm -rf app/src/driver/assets/juspay/index_bundle.js
cp ./../ui-driver/dist/android/index_bundle.js app/src/driver/assets/juspay
rm -rf app/src/driver/assets/juspay/juspay_assets.json
cd ..
sh driverJuspayAssets.sh

