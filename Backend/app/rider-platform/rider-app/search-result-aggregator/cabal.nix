{ mkDerivation, aeson, base, beckn-spec, bytestring, esqueleto
, euler-hs, lib, mobility-core, persistent, persistent-postgresql
, record-dot-preprocessor, record-hasfield, rider-app
, servant-server, stm, template-haskell, text, time
}:
mkDerivation {
  pname = "search-result-aggregator";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring esqueleto euler-hs mobility-core persistent
    persistent-postgresql record-dot-preprocessor record-hasfield
    rider-app servant-server stm template-haskell text time
  ];
  executableHaskellDepends = [
    aeson base beckn-spec bytestring esqueleto euler-hs mobility-core
    persistent persistent-postgresql record-dot-preprocessor
    record-hasfield servant-server stm template-haskell text time
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "search-result-aggregator-exe";
}
