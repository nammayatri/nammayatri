{ mkDerivation, aeson, base, beam-core, beam-mysql, beam-postgres
, bytestring, casing, cereal, dynamic-offer-driver-app, esqueleto
, euler-hs, hedis, lib, mobility-core, monad-control, persistent
, persistent-postgresql, postgresql-simple, random
, record-dot-preprocessor, record-hasfield, rider-app, scheduler
, sequelize, servant-server, singletons, template-haskell, text
, time, unordered-containers, uuid, yudhishthira
}:
mkDerivation {
  pname = "producer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beam-core beam-mysql beam-postgres bytestring casing
    cereal dynamic-offer-driver-app esqueleto euler-hs hedis
    mobility-core monad-control persistent persistent-postgresql
    postgresql-simple random record-dot-preprocessor record-hasfield
    rider-app scheduler sequelize servant-server singletons
    template-haskell text time unordered-containers uuid yudhishthira
  ];
  executableHaskellDepends = [
    aeson base beam-core beam-mysql beam-postgres bytestring casing
    cereal dynamic-offer-driver-app esqueleto euler-hs hedis
    mobility-core monad-control persistent persistent-postgresql
    postgresql-simple random record-dot-preprocessor record-hasfield
    rider-app scheduler sequelize servant-server singletons
    template-haskell text time unordered-containers uuid yudhishthira
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "producer-exe";
}
