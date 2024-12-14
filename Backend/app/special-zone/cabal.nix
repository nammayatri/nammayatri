{ mkDerivation, aeson, base, base16-bytestring, bytestring
, containers, cryptohash, euler-hs, exceptions, http-client
, http-types, lib, mobility-core, record-dot-preprocessor
, record-hasfield, servant, servant-client, servant-server, text
, time, uri-encode
}:
mkDerivation {
  pname = "special-zone";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring containers cryptohash
    euler-hs exceptions http-client http-types mobility-core
    record-dot-preprocessor record-hasfield servant servant-client
    servant-server text time uri-encode
  ];
  executableHaskellDepends = [
    aeson base base16-bytestring bytestring containers cryptohash
    euler-hs exceptions http-client http-types mobility-core
    record-dot-preprocessor record-hasfield servant servant-client
    servant-server text time uri-encode
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "special-zone-exe";
}
