# !/bin/bash
export NODE_OPTIONS=--openssl-legacy-provider

# Enable case-insensitive matching
shopt -s nocasematch

user="nammaYatri"
partner="nammaYatriPartner"
success=true

# Build options - NyUserProdDebug | YsUserProdDebug | YatriUserProdDebug | NyDriverProdDebug | YsDriverProdDebug | YatriDriverProdDebug 

print_status() {
    status=$1
    if [ "$status" == "success" ]; then
        echo -e "\e[32m✔ Passed\e[0m"
    elif [ "$status" == "failure" ]; then
        echo -e "\e[31m✘ Failed\e[0m"
        exit 1
    else
        echo "Unknown status"
        exit 1
    fi
}

if [[ "$1" =~ .*"User".* ]]; then
    echo " ---------- Customer prod:android :- --------------"
    # nix develop .#frontend 
    cd ../ui-customer || print_status "failure"
    npm install && npm install purescript
    npm run prod:android || print_status "failure"
    cd ./../android-native || print_status "failure"
    echo " ---------- Copy index_bundle.js Customer :- --------------"
    mkdir -p "app/src/user/$user/assets/juspay" || print_status "failure"
    rm -rf app/src/user/$user/assets/juspay/index_bundle.js || print_status "failure"
    cp ./../ui-customer/dist/android/index_bundle.js app/src/user/$user/assets/juspay || print_status "failure"
    rm -rf app/src/user/$user/assets/juspay/juspay_assets.json || print_status "failure"
    cd .. || print_status "failure"
    sh userJuspayAssets.sh $user || print_status "failure"
else
    echo " ---------- Driver prod:android :- --------------"
    # nix develop .#frontend
    cd ../ui-driver || print_status "failure"
    npm install && npm install purescript
    npm run prod:android || print_status "failure"
    cd ./../android-native || print_status "failure"
    echo " ---------- Copy index_bundle.js Driver :- --------------"
    mkdir -p "app/src/driver/$partner/assets/juspay" || print_status "failure"
    rm -rf app/src/driver/$partner/assets/juspay/index_bundle.js || print_status "failure"
    cp ./../ui-driver/dist/android/index_bundle.js app/src/driver/$partner/assets/juspay || print_status "failure"
    rm -rf app/src/driver/$partner/assets/juspay/juspay_assets.json || print_status "failure"
    cd .. || print_status "failure"
    sh driverJuspayAssets.sh $partner || print_status "failure"
fi

# Reset nocasematch to its default value (case-sensitive)
shopt -u nocasematch


# Update HTML file
cd android-native || print_status "failure"
echo " ---------- Update HTML file :- --------------"
sed -i 's|v1-assets_downloader.jsa|index_bundle.js|' app/src/main/assets/juspay/becknbase.html || print_status "failure"
print_status "success"
cd ..

# Generate KeyStore if not exists
cd android-native || print_status "failure"
if [[ ! -f "my-release-key.keystore" ]]; then
    echo "-------------Generate a KeyStore-----------------------"
    keytool -genkey -v -keystore my-release-key.keystore -alias juspay -keyalg RSA -keysize 2048 -validity 10000 || print_status "failure"
fi

# Build the app
echo " ---------- Build the app :- --------------"
./gradlew assemble$1 || print_status "failure"

# Display final summary
echo -e "\nFinal Summary:"
echo -n "Step 1: Update local.properties "
print_status "success"
echo -n "Step 2: Build and copy index_bundle.js "
print_status "success"
echo -n "Step 3: Update becknbase "
print_status "success"
echo -n "Step 4: Build process for $1 "
print_status "success"

build_path="app/build/outputs/apk"
variant_lower=$(echo "$1" | awk '{print tolower($0)}')  # Convert to lowercase

if [[ "$variant_lower" == *"yatriuserproddebug"* ]]; then
  build_path="app/build/outputs/apk/yatriUserProd/debug/"
elif [[ "$variant_lower" == *"nyuserproddebug"* ]]; then
  build_path="app/build/outputs/apk/nyUserProd/debug/"
elif [[ "$variant_lower" == *"ysuserproddebug"* ]]; then
  build_path="app/build/outputs/apk/ysUserProd/debug/"
elif [[ "$variant_lower" == *"yatriversproddebug"* ]]; then
  build_path="app/build/outputs/apk/yatriDriverProd/debug/"
elif [[ "$variant_lower" == *"nydriverproddebug"* ]]; then
  build_path="app/build/outputs/apk/nyDriverProd/debug/"
elif [[ "$variant_lower" == *"ysdriverproddebug"* ]]; then
  build_path="app/build/outputs/apk/ysDriverProd/debug/"
else
  # Add more conditions as needed
  build_path="app/build/outputs/apk/${variant_lower}/debug/"
fi


sed -i 's|index_bundle.js|v1-assets_downloader.jsa|' app/src/main/assets/juspay/becknbase.html || print_status "failure"


if [ "$success" = true ]; then
    echo -e "\n The APK is located at: $build_path"
else
    echo -e "\nBuild process encountered some issues."
fi