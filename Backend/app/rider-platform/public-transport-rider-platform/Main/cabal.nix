{ mkDerivation, aeson, base, bytestring, containers, esqueleto
, euler-hs, hedis, lib, mobility-core, openapi3, persistent
, persistent-postgresql, prometheus-client, record-dot-preprocessor
, record-hasfield, relude, servant, servant-client
, servant-openapi3, servant-server, template-haskell, text, time
, unordered-containers, wai
}:
mkDerivation {
  pname = "public-transport-rider-platform";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers esqueleto euler-hs hedis
    mobility-core openapi3 persistent persistent-postgresql
    prometheus-client record-dot-preprocessor record-hasfield relude
    servant servant-client servant-openapi3 servant-server
    template-haskell text time unordered-containers wai
  ];
  executableHaskellDepends = [
    aeson base bytestring containers esqueleto euler-hs hedis
    mobility-core openapi3 persistent persistent-postgresql
    prometheus-client record-dot-preprocessor record-hasfield relude
    servant servant-client servant-openapi3 servant-server
    template-haskell text time unordered-containers wai
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "public-transport-rider-platform-exe";
}
