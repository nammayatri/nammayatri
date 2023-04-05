#!/bin/bash
echo " ---------- Customer prod:android :- --------------"
cd ../ui-customer
bower i
npm run prod:android
cd ./../android-native

echo " ---------- Copy index_bundle.js Customer :- --------------"

mkdir -p "app/src/user/$1/assets/juspay"
rm -rf app/src/user/$1/assets/juspay/index_bundle.js
cp ./../ui-customer/dist/android/index_bundle.js app/src/user/$1/assets/juspay
rm -rf app/src/user/$1/assets/juspay/juspay_assets.json
cd ..
sh userJuspayAssets.sh $1

echo " ---------- Driver prod:android :- --------------"

cd ui-driver
bower i
npm run prod:android
cd ./../android-native

echo " ---------- Copy index_bundle.js Driver :- --------------"

mkdir -p "app/src/driver/$2/assets/juspay"
rm -rf app/src/driver/$2/assets/juspay/index_bundle.js
cp ./../ui-driver/dist/android/index_bundle.js app/src/driver/$2/assets/juspay
rm -rf app/src/driver/$2/assets/juspay/juspay_assets.json
cd ..
sh driverJuspayAssets.sh $2

