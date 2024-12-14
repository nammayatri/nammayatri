{ mkDerivation, aeson, base, beam-core, beam-mysql, beam-postgres
, bytestring, cereal, containers, dynamic-offer-driver-app, errors
, esqueleto, euler-hs, hedis, hw-kafka-client, lib, mobility-core
, monad-control, openapi3, persistent, persistent-postgresql
, postgresql-simple, record-dot-preprocessor, record-hasfield
, rider-app, sequelize, servant-openapi3, servant-server
, sessionizer-metrics, streamly, template-haskell, text, time
, unordered-containers, warp
}:
mkDerivation {
  pname = "kafka-consumers";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beam-core beam-mysql beam-postgres bytestring cereal
    containers dynamic-offer-driver-app errors esqueleto euler-hs hedis
    hw-kafka-client mobility-core monad-control openapi3 persistent
    persistent-postgresql postgresql-simple record-dot-preprocessor
    record-hasfield rider-app sequelize servant-openapi3 servant-server
    sessionizer-metrics streamly template-haskell text time
    unordered-containers warp
  ];
  executableHaskellDepends = [
    aeson base beam-core beam-mysql beam-postgres bytestring cereal
    containers errors esqueleto euler-hs hedis hw-kafka-client
    mobility-core monad-control openapi3 persistent
    persistent-postgresql postgresql-simple record-dot-preprocessor
    record-hasfield sequelize servant-openapi3 servant-server
    sessionizer-metrics streamly template-haskell text time
    unordered-containers warp
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "kafka-consumers-exe";
}
