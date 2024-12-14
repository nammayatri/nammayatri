{ mkDerivation, aeson, aeson-pretty, async, base, beam-core
, beam-mysql, beam-postgres, bytestring, casing, cereal, connection
, containers, directory, errors, euler-events-hs, euler-hs, extra
, filepath, formatting, hedis, http-client, http-client-tls
, http-types, hw-kafka-client, juspay-extra, lens, lib
, mobility-core, pcg-random, postgresql-simple
, record-dot-preprocessor, record-hasfield, resource-pool
, scientific, sequelize, servant-server, text, time, tinylog
, transformers, unix, unordered-containers, utf8-string, uuid
, vector, wai, wai-extra, wai-middleware-prometheus, warp, warp-tls
}:
mkDerivation {
  pname = "dynamic-offer-driver-drainer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty async base beam-core beam-mysql beam-postgres
    bytestring casing cereal connection containers directory errors
    euler-events-hs euler-hs extra filepath formatting hedis
    http-client http-client-tls http-types hw-kafka-client juspay-extra
    lens mobility-core pcg-random postgresql-simple
    record-dot-preprocessor record-hasfield resource-pool scientific
    sequelize servant-server text time tinylog transformers unix
    unordered-containers utf8-string uuid vector wai wai-extra
    wai-middleware-prometheus warp warp-tls
  ];
  executableHaskellDepends = [
    aeson aeson-pretty async base beam-core beam-mysql beam-postgres
    bytestring casing cereal connection containers directory errors
    euler-events-hs euler-hs extra filepath formatting hedis
    http-client http-client-tls http-types hw-kafka-client juspay-extra
    lens mobility-core pcg-random postgresql-simple
    record-dot-preprocessor record-hasfield resource-pool scientific
    sequelize servant-server text time tinylog transformers unix
    unordered-containers utf8-string uuid vector wai wai-extra
    wai-middleware-prometheus warp warp-tls
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "dynamic-offer-driver-drainer-exe";
}
