{ mkDerivation, aeson, base, bytestring, clock, containers
, euler-hs, lib, mobility-core, record-dot-preprocessor
, record-hasfield, servant-server, stm, text, time, time-units, wai
, warp
}:
mkDerivation {
  pname = "mock-fcm";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring clock containers euler-hs mobility-core
    record-dot-preprocessor record-hasfield servant-server stm text
    time time-units wai warp
  ];
  executableHaskellDepends = [
    base euler-hs record-dot-preprocessor record-hasfield
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "mock-fcm-exe";
}
