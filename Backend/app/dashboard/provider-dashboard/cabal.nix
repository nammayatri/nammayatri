{ mkDerivation, aeson, base, beckn-services, beckn-spec, bytestring
, containers, dashboard-helper-api, dynamic-offer-driver-app
, esqueleto, euler-hs, http-client, http-client-tls, lib
, lib-dashboard, mobility-core, openapi3, persistent
, persistent-postgresql, record-dot-preprocessor, record-hasfield
, rider-app, servant-client, servant-openapi3, servant-server
, shared-services, special-zone, special-zone-a, template-haskell
, text, time, unordered-containers, utf8-string, yudhishthira
}:
mkDerivation {
  pname = "provider-dashboard";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beckn-services beckn-spec bytestring containers
    dashboard-helper-api dynamic-offer-driver-app esqueleto euler-hs
    http-client http-client-tls lib-dashboard mobility-core openapi3
    persistent persistent-postgresql record-dot-preprocessor
    record-hasfield rider-app servant-client servant-openapi3
    servant-server shared-services special-zone special-zone-a
    template-haskell text time unordered-containers utf8-string
    yudhishthira
  ];
  executableHaskellDepends = [
    aeson base beckn-services beckn-spec bytestring containers
    dashboard-helper-api esqueleto euler-hs http-client http-client-tls
    mobility-core openapi3 persistent persistent-postgresql
    record-dot-preprocessor record-hasfield servant-client
    servant-openapi3 servant-server shared-services special-zone
    special-zone-a template-haskell text time unordered-containers
    utf8-string yudhishthira
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "provider-dashboard-exe";
}
