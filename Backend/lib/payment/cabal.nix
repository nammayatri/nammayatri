{ mkDerivation, aeson, base, base16-bytestring, beam-core
, beam-mysql, beam-postgres, bytestring, cereal, containers
, cryptohash, euler-hs, exceptions, http-client, http-types, lib
, mobility-core, postgresql-simple, record-dot-preprocessor
, record-hasfield, sequelize, servant, servant-client
, servant-server, text, time, unordered-containers, uri-encode
}:
mkDerivation {
  pname = "payment";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base16-bytestring beam-core beam-mysql beam-postgres
    bytestring cereal containers cryptohash euler-hs exceptions
    http-client http-types mobility-core postgresql-simple
    record-dot-preprocessor record-hasfield sequelize servant
    servant-client servant-server text time unordered-containers
    uri-encode
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
