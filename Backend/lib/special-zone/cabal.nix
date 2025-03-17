{ mkDerivation, aeson, base, base16-bytestring, bytestring
, containers, cryptohash, euler-hs, exceptions, http-client
, http-types, lens, lib, mobility-core, openapi3
, record-dot-preprocessor, record-hasfield, servant, servant-client
, servant-openapi3, servant-server, text, time, uri-encode
}:
mkDerivation {
  pname = "special-zone-a";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring containers cryptohash
    euler-hs exceptions http-client http-types lens mobility-core
    openapi3 record-dot-preprocessor record-hasfield servant
    servant-client servant-openapi3 servant-server text time uri-encode
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
