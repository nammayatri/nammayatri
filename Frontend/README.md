
# Frontend - NammaYatri

This is the frontend codebase for the Nammayatri app.




## Installation

Our dependencies are managed via nix, to get access to the language servers and tooling just open the frontend development shell.
```bash
   nix develop .#frontend
```
This will give you access to all of the purescript tooling as well as the correct version of node and their corresponding language servers.


## Android

To start the frontend app for Android, follow these steps:

    1. Open android-native in Android Studio
    2. select build variant
    3. Add the google-services.json file to the app folder.
    4. In the terminal, run 'gradlew createJSaFiles'

**Starting the Server**

    nix run <customer, driver>:start:android:master


## IOS

To start the frontend app for IOS, follow these steps:


**Starting the Server**

    npm run start:ios:<master, sandbox, or prod>
