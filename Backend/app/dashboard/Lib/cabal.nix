{ mkDerivation, aeson, base, base32, beam-core, beam-mysql
, beam-postgres, beckn-spec, bytestring, cereal, containers
, cryptonite, dashboard-helper-api, dynamic-offer-driver-app
, esqueleto, euler-hs, http-types, lens, lib, mobility-core
, openapi3, passetto-client, persistent, persistent-postgresql
, postgresql-simple, random, record-dot-preprocessor
, record-hasfield, rider-app, sequelize, servant-client
, servant-openapi3, servant-server, shared-services, singletons
, singletons-th, template-haskell, text, time, unordered-containers
, wai
}:
mkDerivation {
  pname = "lib-dashboard";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base32 beam-core beam-mysql beam-postgres beckn-spec
    bytestring cereal containers cryptonite dashboard-helper-api
    dynamic-offer-driver-app esqueleto euler-hs http-types lens
    mobility-core openapi3 passetto-client persistent
    persistent-postgresql postgresql-simple random
    record-dot-preprocessor record-hasfield rider-app sequelize
    servant-client servant-openapi3 servant-server shared-services
    singletons singletons-th template-haskell text time
    unordered-containers wai
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
