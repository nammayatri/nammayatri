{ stdenv, lib, runtimeShell, writeShellApplication, jdk17 }:
{ androidSdk
, app ? null
, platformVersion ? "31"
, googleApis ? false
, playstore ? false
, abiVersion ? "arm64-v8a"
, enableGPU ? false
, extraAVDFiles ? [ ]
, package ? null
, activity ? null
, androidAvdFlags ? ""
, androidEmulatorFlags ? ""
, androidUserHome ? null
, androidAvdHome ? null
, systemImageType ? (
    if googleApis then
      if playstore then
        "google_apis_playstore"
      else
        "google_apis"
    else
      "default"
  )
}:
let
  androidSdkRoot = "${androidSdk}/share/android-sdk";
in
writeShellApplication {
  name = "run-test-emulator";
  runtimeInputs = [ androidSdk ];
  text = ''
    # We need to specify the location of the Android SDK root folder
    export ANDROID_HOME=${androidSdkRoot}
    export ANDROID_SDK_ROOT=${androidSdkRoot}

    # We need a TMPDIR
    if [ -z "''${TMPDIR:-}" ]
    then
        TMPDIR=/tmp
    fi

    export ANDROID_USER_HOME
    ${if androidUserHome == null then ''
      # Store the virtual devices somewhere else, instead of polluting a user's HOME directory
      ANDROID_USER_HOME=$(mktemp -d "$TMPDIR/nix-android-user-home-XXXX")
    '' else ''
      mkdir -p "${androidUserHome}"
      ANDROID_USER_HOME="${androidUserHome}"
    ''}

    ${if androidAvdHome == null then ''
      export ANDROID_AVD_HOME=$ANDROID_USER_HOME/avd
      mkdir -p "$ANDROID_AVD_HOME"
    '' else ''
      mkdir -p "${androidAvdHome}"
      export ANDROID_AVD_HOME="${androidAvdHome}"
    ''}


    # We have to look for a free TCP port

    echo "Looking for a free TCP port in range 5554-5584" >&2

    for i in $(seq 5554 2 5584)
    do
        if ! adb devices | grep -q "emulator-$i"
        then
            port=$i
            break
        fi
    done

    if [ -z "$port" ]
    then
        echo "Unfortunately, the emulator port space is exhausted!" >&2
        exit 1
    else
        echo "We have a free TCP port: $port" >&2
    fi

    export ANDROID_SERIAL="emulator-$port"

    # Create a virtual android device for testing if it does not exist
    avdmanager list target

    if ! avdmanager list avd | grep -q 'Name: device'
    then
        # Create a virtual android device
        set -o xtrace
        avdmanager create avd --force -n device -k "system-images;android-${platformVersion};${systemImageType};${abiVersion}" -p "$ANDROID_AVD_HOME"

        ${lib.optionalString enableGPU ''
          # Enable GPU acceleration
          echo "hw.gpu.enabled=yes" >> $ANDROID_AVD_HOME/device.avd/config.ini
        ''}

        ${lib.concatMapStrings (extraAVDFile: ''
          ln -sf ${extraAVDFile} $ANDROID_AVD_HOME/device.avd
        '') extraAVDFiles}
    fi

    # Launch the emulator
    printf "\nLaunch the emulator"
    emulator -avd device -no-boot-anim -port "$port" ${lib.optionalString (androidEmulatorFlags != null) androidEmulatorFlags} &

    # Wait until the device has completely booted
    echo "Waiting until the emulator has booted the device and the package manager is ready..." >&2

    adb -s "$ANDROID_SERIAL" wait-for-device

    echo "Device state has been reached" >&2

    while ! adb -s "$ANDROID_SERIAL" shell getprop dev.bootcomplete | grep -q 1
    do
        sleep 5
    done

    echo "dev.bootcomplete property is 1" >&2

    echo "ready" >&2

    ${lib.optionalString (app != null && package != null) ''
      # Install the App through the debugger, if it has not been installed yet

      if [ "$(adb -s "$ANDROID_SERIAL" shell pm list packages | grep package:${package})" = "" ]
      then
          if [ -d "${app}" ]
          then
              appPath="$(echo ${app}/*.apk)"
          else
              appPath="${app}"
          fi

          adb -s "$ANDROID_SERIAL" install "$appPath"
      fi

      # Start the application
      ${lib.optionalString (activity != null) ''
          adb -s "$ANDROID_SERIAL" shell am start -a android.intent.action.MAIN -n ${package}/${activity}
      ''}
    ''}
  '';
}
