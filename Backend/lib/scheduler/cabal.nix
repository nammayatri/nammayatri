{ mkDerivation, aeson, base, beam-core, beam-mysql, beam-postgres
, bytestring, casing, cereal, containers, euler-hs, exceptions, lib
, mobility-core, postgresql-simple, prometheus-client, random
, record-dot-preprocessor, record-hasfield, sequelize
, servant-server, singletons, template-haskell, text, time
, transformers, unliftio, unliftio-core, unordered-containers, uuid
}:
mkDerivation {
  pname = "scheduler";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base beam-core beam-mysql beam-postgres bytestring casing
    cereal containers euler-hs exceptions mobility-core
    postgresql-simple prometheus-client random record-dot-preprocessor
    record-hasfield sequelize servant-server singletons
    template-haskell text time transformers unliftio unliftio-core
    unordered-containers uuid
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
