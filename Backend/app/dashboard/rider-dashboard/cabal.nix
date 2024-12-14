{ mkDerivation, aeson, base, beckn-spec, bytestring, containers
, dashboard-helper-api, esqueleto, euler-hs, lib, lib-dashboard
, mobility-core, openapi3, persistent, persistent-postgresql
, record-dot-preprocessor, record-hasfield, rider-app
, servant-client, servant-openapi3, servant-server, shared-services
, special-zone-a, template-haskell, text, time
, unordered-containers, yudhishthira
}:
mkDerivation {
  pname = "rider-dashboard";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beckn-spec bytestring containers dashboard-helper-api
    esqueleto euler-hs lib-dashboard mobility-core openapi3 persistent
    persistent-postgresql record-dot-preprocessor record-hasfield
    rider-app servant-client servant-openapi3 servant-server
    shared-services special-zone-a template-haskell text time
    unordered-containers yudhishthira
  ];
  executableHaskellDepends = [
    aeson base beckn-spec bytestring containers dashboard-helper-api
    esqueleto euler-hs mobility-core openapi3 persistent
    persistent-postgresql record-dot-preprocessor record-hasfield
    servant-client servant-openapi3 servant-server template-haskell
    text time unordered-containers yudhishthira
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "rider-dashboard-exe";
}
