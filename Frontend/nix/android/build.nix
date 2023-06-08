{ stdenv
, jdk
, gradle ? gradle_8
, gradle_8
, mavenRepo
, androidSdk
, src
, buildTask ? "build"
}:

stdenv.mkDerivation {
  inherit src;
  pname = "${buildTask}-gradle";
  version = "0.0";

  nativeBuildInputs = [ gradle jdk ];

  JDK_HOME = "${jdk.home}";
  ANDROID_HOME = "${androidSdk}/share/android-sdk";
  ANDROID_SDK_ROOT = "${androidSdk}/share/android-sdk";

  buildPhase = ''
    runHook preBuild
    mkdir .gradle 
    chmod 777 .gradle
    export GRADLE_USER_HOME="$(realpath .gradle)";
    export ANDROID_USER_HOME="$GRADLE_USER_HOME";

    ${gradle}/bin/gradle ${buildTask} \
      --offline --no-daemon --no-build-cache --info --full-stacktrace \
      --warning-mode=all --parallel --console=plain \
      -PnixMavenRepo=${mavenRepo} \
      -Dorg.gradle.project.android.aapt2FromMavenOverride=$ANDROID_SDK_ROOT/build-tools/33.0.0/aapt2
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp -r app/build/outputs/* $out
    runHook postInstall
  '';

  dontStrip = true;
}
