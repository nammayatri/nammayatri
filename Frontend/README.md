
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

    1. Open android-native in Android Studio
    2. select build variant
    3. Add the google-services.json file to the app folder.
    4. In the terminal, run 'gradlew createJSaFiles'

**Starting the Server**

    npm run start:android:<master, sandbox, or prod>


## IOS

**Prerequisite**

    1.  Install:-
        -   xcode (through app store)
        -   cocoapods (brew install cocoapods)

To start the frontend app for IOS, follow these steps:

    1. Run ```pod install```
    2. Change IP address in becknbase file
    3. Add GoogleService file
    

**Starting the Server**

    npm run start:ios:<master, sandbox, or prod>