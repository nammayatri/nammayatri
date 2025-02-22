{ mkDerivation, aeson, base, beckn-services, beckn-spec, bytestring
, cac_client, containers, cryptonite, dynamic-offer-driver-app
, either, euler-hs, exceptions, external, extra, generic-lens
, haskell-cac, hspec, lib, mobility-core, openapi3, passetto-client
, prometheus-client, record-dot-preprocessor, record-hasfield
, resource-pool, safe, scheduler, servant, servant-client
, servant-client-core, servant-openapi3, servant-server
, sessionizer-metrics, singletons, stm, string-conversions, text
, text-conversions, time, unordered-containers, uuid, wai, warp
}:
mkDerivation {
  pname = "driver-offer-allocator";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beckn-services beckn-spec bytestring containers
    cryptonite dynamic-offer-driver-app either euler-hs exceptions
    external extra generic-lens haskell-cac hspec mobility-core
    openapi3 passetto-client prometheus-client record-dot-preprocessor
    record-hasfield resource-pool safe scheduler servant servant-client
    servant-client-core servant-openapi3 servant-server
    sessionizer-metrics singletons stm string-conversions text
    text-conversions time unordered-containers uuid wai warp
  ];
  librarySystemDepends = [ cac_client ];
  executableHaskellDepends = [
    aeson base beckn-services bytestring containers cryptonite either
    euler-hs exceptions external extra generic-lens haskell-cac hspec
    mobility-core openapi3 passetto-client prometheus-client
    record-dot-preprocessor record-hasfield resource-pool safe
    scheduler servant servant-client servant-client-core
    servant-openapi3 servant-server singletons stm string-conversions
    text text-conversions time unordered-containers uuid wai warp
  ];
  executableSystemDepends = [ cac_client ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "driver-offer-allocator-exe";
}
