{ mkDerivation, aeson, base, base64-bytestring, beam-core
, beam-mysql, beam-postgres, beckn-services, beckn-spec, bytestring
, cac_client, casing, cassava, cereal, containers, cryptonite
, dashboard-helper-api, data-default-class, either, esqueleto
, euler-hs, exceptions, external, extra, generic-lens, geohash
, hashable, haskell-cac, hspec, http-client, http-client-tls
, http-conduit, http-types, json-logic-hs, JuicyPixels
, JuicyPixels-extra, jwt, lens, lens-aeson, lib, location-updates
, mobility-core, network-uri, openapi3, passetto-client, payment
, persistent, postgresql-simple, prometheus-client, pureMD5, random
, record-dot-preprocessor, record-hasfield, resource-pool
, scheduler, scientific, sequelize, servant, servant-client
, servant-client-core, servant-multipart, servant-openapi3
, servant-server, servant-xml, sessionizer-metrics, shared-services
, singletons, singletons-th, special-zone-a, split, stm
, string-conversions, template-haskell, text, text-conversions
, text-format, time, unordered-containers, utils, uuid, vector, wai
, wai-extra, warp, xml-conduit, xmlbf, yudhishthira
}:
mkDerivation {
  pname = "dynamic-offer-driver-app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring beam-core beam-mysql beam-postgres
    beckn-services beckn-spec bytestring casing cassava cereal
    containers cryptonite dashboard-helper-api data-default-class
    either esqueleto euler-hs exceptions external extra generic-lens
    geohash hashable haskell-cac hspec http-client http-client-tls
    http-conduit http-types json-logic-hs JuicyPixels JuicyPixels-extra
    jwt lens lens-aeson location-updates mobility-core network-uri
    openapi3 passetto-client payment persistent postgresql-simple
    prometheus-client pureMD5 random record-dot-preprocessor
    record-hasfield resource-pool scheduler scientific sequelize
    servant servant-client servant-client-core servant-multipart
    servant-openapi3 servant-server servant-xml sessionizer-metrics
    shared-services singletons singletons-th special-zone-a split stm
    string-conversions template-haskell text text-conversions
    text-format time unordered-containers utils uuid vector wai
    wai-extra warp xml-conduit xmlbf yudhishthira
  ];
  librarySystemDepends = [ cac_client ];
  executableHaskellDepends = [
    aeson base base64-bytestring beam-core beam-mysql beam-postgres
    beckn-services beckn-spec bytestring casing cassava cereal
    containers cryptonite dashboard-helper-api data-default-class
    either esqueleto euler-hs exceptions extra generic-lens geohash
    hashable haskell-cac hspec http-client http-client-tls http-conduit
    http-types json-logic-hs JuicyPixels JuicyPixels-extra jwt lens
    lens-aeson mobility-core network-uri openapi3 passetto-client
    persistent postgresql-simple prometheus-client pureMD5 random
    record-dot-preprocessor record-hasfield resource-pool scheduler
    scientific sequelize servant servant-client servant-client-core
    servant-multipart servant-openapi3 servant-server servant-xml
    shared-services singletons singletons-th split stm
    string-conversions template-haskell text text-conversions
    text-format time unordered-containers uuid vector wai wai-extra
    warp xml-conduit xmlbf yudhishthira
  ];
  executableSystemDepends = [ cac_client ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "dynamic-offer-driver-app-exe";
}
