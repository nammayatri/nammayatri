{ mkDerivation, aeson, base, bytestring, containers, esqueleto
, euler-hs, lib, mobility-core, persistent, persistent-postgresql
, public-transport-rider-platform, record-dot-preprocessor
, record-hasfield, servant-server, stm, template-haskell, text
, time, unordered-containers
}:
mkDerivation {
  pname = "public-transport-search-consumer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers esqueleto euler-hs mobility-core
    persistent persistent-postgresql public-transport-rider-platform
    record-dot-preprocessor record-hasfield servant-server stm
    template-haskell text time unordered-containers
  ];
  executableHaskellDepends = [
    aeson base bytestring containers esqueleto euler-hs mobility-core
    persistent persistent-postgresql public-transport-rider-platform
    record-dot-preprocessor record-hasfield servant-server stm
    template-haskell text time unordered-containers
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "public-transport-search-consumer-exe";
}
