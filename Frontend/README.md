
# Frontend - NammaYatri

This is the frontend codebase for the Nammayatri app.

## Installation

To install the frontend dependencies, navigate to the frontend > ui-customer/ui-driver both and run the following commands:

```bash
  npm i
  bower i
```

## Android

To start the frontend app for Android, follow these steps:

    1. Open `android-native` in Android Studio
    2. select build variant
    3. Add the google-services.json file to the app folder.
    4. In the terminal, run 'gradlew createJSaFiles'

To create bundle for the app, follow these steps:
    1. Open `android-native` in terminal
    2. run the below command

```bash
  bash bundling.sh <userMerchantDir> <driverMerchantDir>
```

```bash
  ex :  bash bundling.sh nammaYatri nammaYatriPartner
```

To update the `juspay_assets.json`, follow these steps:
    1. Open `android-native` in terminal
    2. run the below command

# For user

```bash
  bash userJuspayAssets.sh <userMerchantDir>
```

   

```bash
   ex :  bash userJuspayAssets.sh nammaYatri
```

# For driver

```bash
  bash userJuspayAssets.sh <userMerchantDir>
```

```bash
   ex : bash userJuspayAssets.sh nammaYatri
```

**Starting the Server**

    npm run start:android:<master, sandbox, or prod>

## IOS

To start the frontend app for IOS, follow these steps:

**Starting the Server**

    npm run start:ios:<master, sandbox, or prod>
