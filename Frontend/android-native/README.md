# Atlas-Android

**Project Overview**

This project contains the Android setup for running any atlas microapp(submodules) on Android.

**Steps to run**
1.  Open this project in Android Studio, select build variant
2.  Add GoogleService file
3.  In **terminal**, Run ```gradlew createJSaFiles```
4.  Use the submodule setup and follow the next step 5
OR  
Use the respective repository of the microapps service 
    -   atlas-customer  
    https://bitbucket.juspay.net/projects/BEC/repos/atlas-ui-customer/browse
    -   atlas-driver  
    https://bitbucket.juspay.net/projects/BEC/repos/atlas-ui-driver/browse


5.  Open Driver/Customer directory in **terminal** and do the following
    -   for once you have to do ```npm install```
    -  to start the server do ```npm run start```
    -  or do ```npm run clean``` 

**Submodule setup**
1.  Run below command to initial setup submodules

```bash
git submodule update --init --recursive
```

2.  Run below command to sync with respective remote repo/branches

```bash
git submodule update --recursive --remote
```
