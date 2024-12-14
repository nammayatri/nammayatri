{ mkDerivation, aeson, base, beam-core, beam-mysql, beam-postgres
, beckn-services, beckn-spec, bytestring, casing, cassava, cereal
, containers, cryptonite, either, esqueleto, euler-hs, exceptions
, external, extra, generic-lens, hspec, http-client, lens, lib
, location-updates, mobility-core, openapi3, persistent
, postgresql-simple, prometheus-client, record-dot-preprocessor
, record-hasfield, regex-compat, regex-tdfa, resource-pool
, scheduler, sequelize, servant, servant-client
, servant-client-core, servant-multipart, servant-openapi3
, servant-server, singletons, singletons-th, special-zone, stm
, string-conversions, template-haskell, text, text-conversions
, time, unordered-containers, uuid, vector, wai, warp
}:
mkDerivation {
  pname = "shared-services";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base beam-core beam-mysql beam-postgres beckn-services
    beckn-spec bytestring casing cassava cereal containers cryptonite
    either esqueleto euler-hs exceptions external extra generic-lens
    hspec http-client lens location-updates mobility-core openapi3
    persistent postgresql-simple prometheus-client
    record-dot-preprocessor record-hasfield regex-compat regex-tdfa
    resource-pool scheduler sequelize servant servant-client
    servant-client-core servant-multipart servant-openapi3
    servant-server singletons singletons-th special-zone stm
    string-conversions template-haskell text text-conversions time
    unordered-containers uuid vector wai warp
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
