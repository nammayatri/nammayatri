{ mkDerivation, aeson, base, base16-bytestring, bytestring
, containers, cryptohash, euler-hs, exceptions, http-client
, http-types, lib, mobility-core, prometheus-client
, prometheus-metrics-ghc, record-dot-preprocessor, record-hasfield
, servant, servant-client, servant-server, text, time, uri-encode
, wai-middleware-prometheus
}:
mkDerivation {
  pname = "sessionizer-metrics";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring containers cryptohash
    euler-hs exceptions http-client http-types mobility-core
    prometheus-client prometheus-metrics-ghc record-dot-preprocessor
    record-hasfield servant servant-client servant-server text time
    uri-encode wai-middleware-prometheus
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
