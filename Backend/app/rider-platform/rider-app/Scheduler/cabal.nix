{ mkDerivation, aeson, base, beckn-spec, bytestring, containers
, cryptonite, either, euler-hs, exceptions, extra, generic-lens
, hspec, lib, mobility-core, openapi3, passetto-client
, prometheus-client, record-dot-preprocessor, record-hasfield
, resource-pool, rider-app, safe, scheduler, servant
, servant-client, servant-client-core, servant-openapi3
, servant-server, sessionizer-metrics, singletons, stm
, string-conversions, text, text-conversions, time
, unordered-containers, uuid, wai, warp
}:
mkDerivation {
  pname = "rider-app-scheduler";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beckn-spec bytestring containers cryptonite either
    euler-hs exceptions extra generic-lens hspec mobility-core openapi3
    passetto-client prometheus-client record-dot-preprocessor
    record-hasfield resource-pool rider-app safe scheduler servant
    servant-client servant-client-core servant-openapi3 servant-server
    sessionizer-metrics singletons stm string-conversions text
    text-conversions time unordered-containers uuid wai warp
  ];
  executableHaskellDepends = [
    aeson base bytestring containers cryptonite either euler-hs
    exceptions extra generic-lens hspec mobility-core openapi3
    passetto-client prometheus-client record-dot-preprocessor
    record-hasfield resource-pool safe scheduler servant servant-client
    servant-client-core servant-openapi3 servant-server singletons stm
    string-conversions text text-conversions time unordered-containers
    uuid wai warp
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "rider-app-scheduler-exe";
}
