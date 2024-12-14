{ mkDerivation, aeson, amazonka, amazonka-core, amazonka-ses, base
, base64-bytestring, beam-core, beam-mysql, beam-postgres
, beckn-services, beckn-spec, bytestring, casing, cassava, cereal
, containers, cryptonite, cryptostore, dashboard-helper-api
, data-default-class, directory, euler-hs, external, extra
, filepath, geohash, hashable, haskell-cac, http-api-data
, http-client, http-client-tls, http-media, http-types
, json-logic-hs, lens, lens-aeson, lib, memory, mobility-core
, openapi3, passetto-client, payment, persistent, postgresql-simple
, prometheus-client, prometheus-metrics-ghc, pureMD5, random
, raw-strings-qq, record-dot-preprocessor, record-hasfield
, regex-posix, resource-pool, scheduler, scientific, sequelize
, servant, servant-client, servant-client-core, servant-openapi3
, servant-server, sessionizer-metrics, shared-services, singletons
, singletons-th, special-zone, special-zone-a, split
, template-haskell, text, text-conversions, text-format, time
, unliftio-core, unordered-containers, utils, uuid, vector, wai
, wai-extra, wai-middleware-prometheus, warp, x509, yudhishthira
}:
mkDerivation {
  pname = "rider-app";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson amazonka amazonka-core amazonka-ses base base64-bytestring
    beam-core beam-mysql beam-postgres beckn-services beckn-spec
    bytestring casing cassava cereal containers cryptonite cryptostore
    dashboard-helper-api data-default-class directory euler-hs external
    extra filepath geohash hashable haskell-cac http-api-data
    http-client http-client-tls http-media http-types json-logic-hs
    lens lens-aeson memory mobility-core openapi3 passetto-client
    payment persistent postgresql-simple prometheus-client
    prometheus-metrics-ghc pureMD5 random raw-strings-qq
    record-dot-preprocessor record-hasfield regex-posix resource-pool
    scheduler scientific sequelize servant servant-client
    servant-client-core servant-openapi3 servant-server
    sessionizer-metrics shared-services singletons singletons-th
    special-zone special-zone-a split template-haskell text
    text-conversions text-format time unliftio-core
    unordered-containers utils uuid vector wai wai-extra
    wai-middleware-prometheus warp x509 yudhishthira
  ];
  executableHaskellDepends = [
    aeson amazonka amazonka-core amazonka-ses base base64-bytestring
    beam-core beam-mysql beam-postgres beckn-spec bytestring casing
    cassava cereal containers cryptonite cryptostore
    dashboard-helper-api data-default-class directory euler-hs extra
    filepath geohash hashable http-api-data http-client http-client-tls
    http-media http-types json-logic-hs lens lens-aeson memory
    mobility-core openapi3 passetto-client persistent postgresql-simple
    prometheus-client prometheus-metrics-ghc pureMD5 random
    raw-strings-qq record-dot-preprocessor record-hasfield regex-posix
    resource-pool scheduler scientific sequelize servant servant-client
    servant-client-core servant-openapi3 servant-server singletons
    singletons-th split template-haskell text text-conversions
    text-format time unliftio-core unordered-containers uuid vector wai
    wai-extra wai-middleware-prometheus warp x509 yudhishthira
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  description = "The nammayatri rider application";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "rider-app-exe";
}
