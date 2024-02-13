#!/bin/bash
export NODE_OPTIONS=--openssl-legacy-provider
user="nammaYatri"
partner="nammaYatriPartner"
if [[ "$1" =~ .*"ny".* ]];
then user="nammaYatri"
    partner="nammaYatriPartner"
    echo "--------------------------nammaYatri-----------------------"
    cat merchant-properties/nammaYatri.txt > local.properties
elif [[ "$1" =~ .*"yatri".* ]];
then user="yatri"
    partner="yatriPartner"
    echo "--------------------------Yatri----------------------------"
    cat merchant-properties/yatri.txt > local.properties
elif [[ "$1" =~ .*"ys".* ]];
then user="yatriSati"
    partner="yatriSatiPartner"
    echo "--------------------------yatriSati------------------------"
    cat merchant-properties/yatriSati.txt > local.properties
fi

if [[ "$1" =~ .*"User".* ]];
then 
    echo " ---------- Customer prod:android :- --------------"
    cd ../ui-customer
    npm run prod:android
    cd ./../android-native
    echo " ---------- Copy index_bundle.js Customer :- --------------"
    mkdir -p "app/src/user/$user/assets/juspay"
    rm -rf app/src/user/$user/assets/juspay/index_bundle.js
    cp ./../ui-customer/dist/android/index_bundle.js app/src/user/$user/assets/juspay
    rm -rf app/src/user/$user/assets/juspay/juspay_assets.json
    cd ..
    sh userJuspayAssets.sh $user
else
    echo " ---------- Driver prod:android :- --------------"
    cd ../ui-driver
    npm run prod:android
    cd ./../android-native
    echo " ---------- Copy index_bundle.js Driver :- --------------"
    mkdir -p "app/src/driver/$partner/assets/juspay"
    rm -rf app/src/driver/$partner/assets/juspay/index_bundle.js
    cp ./../ui-driver/dist/android/index_bundle.js app/src/driver/$partner/assets/juspay
    rm -rf app/src/driver/$partner/assets/juspay/juspay_assets.json
    cd ..
    sh driverJuspayAssets.sh $partner
fi
pwd
cd android-native
pwd
if  [[ ! -f "my-release-key.keystore" ]]
then 
    echo "-------------Generate a KeyStore-----------------------"
    keytool -genkey -v -keystore my-release-key.keystore -alias juspay -keyalg RSA -keysize 2048 -validity 10000 
fi
jarsigner -verbose -sigalg SHA1withRSA -digestalg SHA1 -keystore my-release-key.keystore app/build/outputs/apk/release/android-release-unsigned juspay
pwd
./gradlew assemble$1