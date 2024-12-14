{ mkDerivation, aeson, base, beam-core, beckn-spec, bytestring
, case-insensitive, containers, dashboard-helper-api, esqueleto
, euler-hs, http-client, http-client-tls, http-types, lens, lib
, lib-dashboard, mobility-core, network-uri, openapi3, persistent
, persistent-postgresql, record-dot-preprocessor, record-hasfield
, sequelize, servant-client, servant-openapi3, servant-server
, singletons-th, string-conversions, template-haskell, text, time
, unordered-containers
}:
mkDerivation {
  pname = "safety-dashboard";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beam-core beckn-spec bytestring case-insensitive
    containers dashboard-helper-api esqueleto euler-hs http-client
    http-client-tls http-types lens lib-dashboard mobility-core
    network-uri openapi3 persistent persistent-postgresql
    record-dot-preprocessor record-hasfield sequelize servant-client
    servant-openapi3 servant-server singletons-th string-conversions
    template-haskell text time unordered-containers
  ];
  executableHaskellDepends = [
    aeson base beam-core beckn-spec bytestring case-insensitive
    containers dashboard-helper-api esqueleto euler-hs http-client
    http-client-tls http-types lens mobility-core network-uri openapi3
    persistent persistent-postgresql record-dot-preprocessor
    record-hasfield sequelize servant-client servant-openapi3
    servant-server singletons-th string-conversions template-haskell
    text time unordered-containers
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "safety-dashboard-exe";
}
