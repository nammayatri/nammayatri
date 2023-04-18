echo " ---------- Build/Bundle Customer :- --------------"

CUSTOMER_ASSET_PATH=app/src/user/$1/assets/juspay
mkdir -p "$CUSTOMER_ASSET_PATH"
rm "./$CUSTOMER_ASSET_PATH/index_bundle.js"
nix build .#android-customer-bundle 
cp result "$CUSTOMER_ASSET_PATH/index_bundle.js"
rm "$CUSTOMER_ASSET_PATH/juspay_assets.json"
pushd ..
sh userJuspayAssets.sh $1
popd

echo " ---------- Build/Bundle Driver :- --------------"

DRIVER_ASSET_PATH="app/src/driver/$2/assets/juspay"
mkdir -p "$DRIVER_ASSET_PATH"
rm "./$DRIVER_ASSET_PATH/index_bundle.js"
nix build .#android-driver-bundle -
cp result "$DRIVER_ASSET_PATH/index_bundle.js"
rm "$DRIVER_ASSET_PATH/juspay_assets.json"
pushd ..
sh driverJuspayAssets.sh $2
popd
