{ mkDerivation, aeson, base, beckn-spec, bytestring, containers
, euler-hs, lib, mobility-core, record-dot-preprocessor
, record-hasfield, servant-server, text, time, unordered-containers
}:
mkDerivation {
  pname = "mock-rider-platform";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers euler-hs mobility-core
    record-dot-preprocessor record-hasfield servant-server text time
    unordered-containers
  ];
  executableHaskellDepends = [
    aeson base beckn-spec bytestring containers euler-hs mobility-core
    record-dot-preprocessor record-hasfield servant-server text time
    unordered-containers
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "mock-rider-platform-exe";
}
