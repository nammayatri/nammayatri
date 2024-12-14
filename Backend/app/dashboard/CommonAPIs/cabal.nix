{ mkDerivation, aeson, base, beckn-services, beckn-spec, bytestring
, casing, cassava, deriving-aeson, esqueleto, euler-hs, extra, lib
, mobility-core, openapi3, persistent, persistent-postgresql
, record-dot-preprocessor, record-hasfield, servant-client
, servant-multipart, servant-openapi3, servant-server
, shared-services, singletons, singletons-th, special-zone-a
, template-haskell, text, time, vector, wai, yudhishthira
}:
mkDerivation {
  pname = "dashboard-helper-api";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base beckn-services beckn-spec bytestring casing cassava
    deriving-aeson esqueleto euler-hs extra mobility-core openapi3
    persistent persistent-postgresql record-dot-preprocessor
    record-hasfield servant-client servant-multipart servant-openapi3
    servant-server shared-services singletons singletons-th
    special-zone-a template-haskell text time vector wai yudhishthira
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
