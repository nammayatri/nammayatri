{ mkDerivation, aeson, base, beckn-spec, bytestring, esqueleto, lib
, mobility-core, persistent, persistent-postgresql
, record-dot-preprocessor, record-hasfield, servant-server
, template-haskell, text, time
}:
mkDerivation {
  pname = "example-service";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beckn-spec bytestring esqueleto mobility-core persistent
    persistent-postgresql record-dot-preprocessor record-hasfield
    servant-server template-haskell text time
  ];
  executableHaskellDepends = [
    aeson base beckn-spec bytestring esqueleto mobility-core persistent
    persistent-postgresql record-dot-preprocessor record-hasfield
    servant-server template-haskell text time
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "example-service-exe";
}
