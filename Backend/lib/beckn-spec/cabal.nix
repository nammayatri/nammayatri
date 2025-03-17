{ mkDerivation, aeson, aeson-casing, base, base64-bytestring
, binary, bytestring, case-insensitive, clock, containers
, cryptonite, data-default-class, dhall, double-conversion, either
, esqueleto, euler-hs, exceptions, extra, fast-logger, generic-lens
, geojson, hedis, hex-text, hspec, http-media, hw-kafka-client
, insert-ordered-containers, jwt, kleene, lattices, lens, lib
, memory, mobility-core, monad-logger, network, openapi3, parsec
, passetto-client, persistent, persistent-postgresql
, prometheus-client, prometheus-metrics-ghc, prometheus-proc
, pureMD5, record-dot-preprocessor, record-hasfield
, regex-applicative, regex-posix, relude, resource-pool, resourcet
, safe-exceptions, safe-money, scientific, servant, servant-client
, servant-client-core, servant-multipart, servant-openapi3
, servant-server, stm, string-conversions, syb, tasty, tasty-hunit
, template-haskell, text, time, time-units, transformers, universum
, unix, unliftio, unordered-containers, utf8-string, uuid, vector
, wai, wai-middleware-prometheus, warp
}:
mkDerivation {
  pname = "beckn-spec";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-casing base base64-bytestring binary bytestring
    case-insensitive clock containers cryptonite data-default-class
    dhall double-conversion either esqueleto euler-hs exceptions extra
    fast-logger generic-lens geojson hedis hex-text hspec http-media
    hw-kafka-client insert-ordered-containers jwt kleene lattices lens
    memory mobility-core monad-logger network openapi3 parsec
    passetto-client persistent persistent-postgresql prometheus-client
    prometheus-metrics-ghc prometheus-proc pureMD5
    record-dot-preprocessor record-hasfield regex-applicative
    regex-posix relude resource-pool resourcet safe-exceptions
    safe-money scientific servant servant-client servant-client-core
    servant-multipart servant-openapi3 servant-server stm
    string-conversions syb tasty tasty-hunit template-haskell text time
    time-units transformers universum unix unliftio
    unordered-containers utf8-string uuid vector wai
    wai-middleware-prometheus warp
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
