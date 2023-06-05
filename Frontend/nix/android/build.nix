{ stdenv
, jdk
, gradle
, mavenRepo
, androidSdk
, src
, buildTask ? "build"
}:

stdenv.mkDerivation {
  inherit src;
  pname = "${buildTask}-gradle";
  version = "0.0";

  nativeBuildInputs = [ gradle ];

  JDK_HOME = "${jdk.home}";
  ANDROID_SDK_ROOT = "${androidSdk}/share/android-sdk";


  buildPhase = ''
    runHook preBuild
    gradle ${buildTask} \
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
