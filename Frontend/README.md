
# Frontend - NammaYatri

This is the frontend codebase for the Nammayatri app.

## Installation

Our dependencies are managed via nix, to get access to the language servers and tooling from the command line open the frontend development shell.
```bash
   nix develop .#frontend
```
or if you have direnv installed use the provided .envrc 
```bash
   ln .envrc.frontend .envrc
   direnv allow .envrc
```
This will give you access to all of the purescript tooling as well as the correct version of node and their corresponding language servers.


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
  bash driverJuspayAssets.sh <driverMerchantDir>
```

```bash
   ex : bash driverJuspayAssets.sh nammaYatri
```

**Starting the Dev Server**

There are a number of configurations for running the webpack devserver, they are all provided as mission control scripts. For example
```bash
  , ui-customer-start-android-master
```
will start a watch process that serves up the bundle from ui-customer compiled for android with the environment set to `master`.

The full set of these scripts is listed in the mission control motd.

## IOS

To start the frontend app for IOS, follow these steps:

**Starting the Server**

TODO: IOS is not currently supported in this repo


# Editing with VSCode 
We have a VSCode workspace that includes settings for the purescript ide plugin(https://marketplace.visualstudio.com/items?itemName=nwolverson.ide-purescript) and includes the three purescript code folders.

If you use the direnv plugin(https://marketplace.visualstudio.com/items?itemName=mkhl.direnv) you should be able to just open the provided `frontend.code-workspace`. 

If you instead prefer to manually enter the develop shell you can.
```bash
cd Frontend
nix develop .#frontend
code frontend.code-workspace
```

You may be prompted to rebuild the project by the plugin using the "Build Project" command from the plugin. 
Alternatively, if you are running one of the watch scripts the output folder will be populated(though it will not autopopulate diagnostics). You may need to use the command "Restart/Reconnect purs IDE server" if you started the watch after opening VSCode.

After pulling the latest changes from git you may need to run a build manually to ensure your output folder is up to date. 
To run a build manually either use the command palette or the hotkey `Command-Shift-B`