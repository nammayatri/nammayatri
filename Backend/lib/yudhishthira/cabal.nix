{ mkDerivation, aeson, base, base64-bytestring, beam-core
, beam-mysql, beam-postgres, beckn-services, beckn-spec, bytestring
, cac_client, casing, cassava, cereal, containers, cryptonite
, data-default-class, either, esqueleto, euler-hs, exceptions
, extra, generic-lens, geohash, hashable, haskell-cac, hspec
, http-client, http-client-tls, http-conduit, http-types
, json-logic-hs, JuicyPixels, JuicyPixels-extra, lens, lens-aeson
, lib, mobility-core, openapi3, passetto-client, persistent
, postgresql-simple, prometheus-client, pureMD5, random
, record-dot-preprocessor, record-hasfield, resource-pool
, scheduler, scientific, sequelize, servant, servant-client
, servant-client-core, servant-multipart, servant-openapi3
, servant-server, sessionizer-metrics, shared-services, singletons
, singletons-th, split, stm, string-conversions, template-haskell
, text, text-conversions, time, unordered-containers, uuid, vector
, wai, wai-extra, warp
}:
mkDerivation {
  pname = "yudhishthira";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base64-bytestring beam-core beam-mysql beam-postgres
    beckn-services beckn-spec bytestring casing cassava cereal
    containers cryptonite data-default-class either esqueleto euler-hs
    exceptions extra generic-lens geohash hashable haskell-cac hspec
    http-client http-client-tls http-conduit http-types json-logic-hs
    JuicyPixels JuicyPixels-extra lens lens-aeson mobility-core
    openapi3 passetto-client persistent postgresql-simple
    prometheus-client pureMD5 random record-dot-preprocessor
    record-hasfield resource-pool scheduler scientific sequelize
    servant servant-client servant-client-core servant-multipart
    servant-openapi3 servant-server sessionizer-metrics shared-services
    singletons singletons-th split stm string-conversions
    template-haskell text text-conversions time unordered-containers
    uuid vector wai wai-extra warp
  ];
  librarySystemDepends = [ cac_client ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
