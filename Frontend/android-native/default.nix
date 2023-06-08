{ inputs, ... }:
{
  perSystem = { pkgs, system, self', ... }:
    let
      gradle = pkgs.callPackage ../nix/android/gradle.nix { };

      buildMavenRepo = pkgs.callPackage ../nix/android/maven-repo.nix { };

      mavenRepo = buildMavenRepo {
        name = "nix-maven-repo";
        repos = [
          "https://dl.google.com/dl/android/maven2"
          "https://repo1.maven.org/maven2"
          "https://maven.fabric.io/public"
          "https://maven.juspay.in/jp-build-packages/release"
          "https://maven.juspay.in/jp-build-packages/beta"
          "https://maven.juspay.in/jp-build-packages/hyper-sdk"
          "https://maven.getsimpl.com"
          "https://plugins.gradle.org/m2"
          "https://jcenter.bintray.com/"
        ];
        gradle-verification-metadata = ./gradle/verification-metadata.xml;
      };


      androidSdk = inputs.android-nixpkgs.sdk.${system} (sdkPkgs: with sdkPkgs; [
        build-tools-30-0-3
        platform-tools
        platforms-android-31
        emulator
        build-tools-33-0-0 # Get newer aapt2
        cmdline-tools-latest
      ]
      ++ pkgs.lib.optional pkgs.stdenv.isAarch64 system-images-android-31-google-apis-playstore-arm64-v8a
      ++ pkgs.lib.optional pkgs.stdenv.isx86_64 system-images-android-31-google-apis-playstore-x86-64
      ++ pkgs.lib.optional (pkgs.stdenv.isDarwin && pkgs.stdenv.isx86_64) extras-intel-Hardware-Accelerated-Execution-Manager
      );

      buildApp = { variant, env, debug ? true }:
        let buildTask = "assemble${variant}${env}${if debug then "Debug" else "Release"}";
        in pkgs.callPackage ../nix/android/build.nix {
          inherit gradle;
          inherit androidSdk;
          inherit mavenRepo;
          inherit buildTask;
          jdk = pkgs.jdk11;
          src = ./.;
        };

      emulateApp = pkgs.callPackage ../nix/android/emulate-app.nix { };
    in
    {
      apps.gradle-lock = {
        type = "app";
        program = pkgs.writeShellApplication {
          name = "gradle-lock";
          text = ''
            ${gradle}/bin/gradle --write-locks --write-verification-metadata sha256 -q app:dependencies
          '';
        };
      };

      packages.ny-debug = emulateApp {
        inherit androidSdk;
        platformVersion = "31";
        googleApis = true;
        playstore = true;
        abiVersion = if pkgs.stdenv.isAarch64 then "arm64-v8a" else "x86_64";
        app = buildApp {
          variant = "ny_";
          env = "Dev";
          debug = true;
        };
        package = "in.juspay.nammayatri.debug";
        activity = "in.juspay.mobility.MainActivity";
      };

      packages.gradle = gradle;

      apps.adb = {
        type = "app";
        program = "${androidSdk}/share/android-sdk/platform-tools/adb";
      };
    };
}
