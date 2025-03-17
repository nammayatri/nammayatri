{ mkDerivation, aeson, base, beckn-spec, bytestring, containers
, esqueleto, euler-hs, lib, mobility-core, passetto-client
, persistent, persistent-postgresql, record-dot-preprocessor
, record-hasfield, servant, servant-server, template-haskell, text
, time, unordered-containers
}:
mkDerivation {
  pname = "mock-google";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beckn-spec bytestring containers esqueleto euler-hs
    mobility-core passetto-client persistent persistent-postgresql
    record-dot-preprocessor record-hasfield servant servant-server
    template-haskell text time unordered-containers
  ];
  executableHaskellDepends = [
    aeson base beckn-spec bytestring containers esqueleto euler-hs
    mobility-core passetto-client persistent persistent-postgresql
    record-dot-preprocessor record-hasfield servant servant-server
    template-haskell text time unordered-containers
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "mock-google-exe";
}
