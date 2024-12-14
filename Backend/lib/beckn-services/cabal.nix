{ mkDerivation, aeson, base, base16-bytestring, base64-bytestring
, bytestring, case-insensitive, containers, cryptohash, directory
, euler-hs, filepath, http-client, http-client-tls, http-media
, http-types, lib, mobility-core, openapi3, process
, record-dot-preprocessor, record-hasfield, servant, servant-client
, servant-openapi3, servant-server, string-conversions, text, time
, unix, unordered-containers, uri-encode
}:
mkDerivation {
  pname = "beckn-services";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base16-bytestring base64-bytestring bytestring
    case-insensitive containers cryptohash directory euler-hs filepath
    http-client http-client-tls http-media http-types mobility-core
    openapi3 process record-dot-preprocessor record-hasfield servant
    servant-client servant-openapi3 servant-server string-conversions
    text time unix unordered-containers uri-encode
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
