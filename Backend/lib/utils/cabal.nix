{ mkDerivation, aeson, async, base, base64-bytestring, beam-core
, beam-mysql, beam-postgres, beckn-services, beckn-spec, bytestring
, casing, cassava, cereal, containers, cryptonite
, dashboard-helper-api, either, esqueleto, euler-hs, exceptions
, extra, generic-lens, geohash, haskell-cac, hspec, http-client
, http-conduit, http-types, JuicyPixels, JuicyPixels-extra, lens
, lib, location-updates, mobility-core, openapi3, payment
, persistent, postgresql-simple, prometheus-client, random
, record-dot-preprocessor, record-hasfield, resource-pool
, scheduler, sequelize, servant, servant-client
, servant-client-core, servant-multipart, servant-openapi3
, servant-server, sessionizer-metrics, shared-services, singletons
, special-zone-a, stm, string-conversions, template-haskell, text
, text-conversions, time, unordered-containers, uuid, vector, wai
, warp, yudhishthira
}:
mkDerivation {
  pname = "utils";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base base64-bytestring beam-core beam-mysql
    beam-postgres beckn-services beckn-spec bytestring casing cassava
    cereal containers cryptonite dashboard-helper-api either esqueleto
    euler-hs exceptions extra generic-lens geohash haskell-cac hspec
    http-client http-conduit http-types JuicyPixels JuicyPixels-extra
    lens location-updates mobility-core openapi3 payment persistent
    postgresql-simple prometheus-client random record-dot-preprocessor
    record-hasfield resource-pool scheduler sequelize servant
    servant-client servant-client-core servant-multipart
    servant-openapi3 servant-server sessionizer-metrics shared-services
    singletons special-zone-a stm string-conversions template-haskell
    text text-conversions time unordered-containers uuid vector wai
    warp yudhishthira
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
