{ mkDerivation, aeson, base, beckn-services, bytestring, clock
, containers, euler-hs, http-types, lib, mobility-core, openapi3
, record-dot-preprocessor, record-hasfield, servant-client-core
, servant-server, stm, text, time, time-units, wai, wai-extra, warp
}:
mkDerivation {
  pname = "mock-idfy";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beckn-services bytestring clock containers euler-hs
    http-types mobility-core openapi3 record-dot-preprocessor
    record-hasfield servant-client-core servant-server stm text time
    time-units wai wai-extra warp
  ];
  executableHaskellDepends = [
    base euler-hs record-dot-preprocessor record-hasfield
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "mock-idfy-exe";
}
