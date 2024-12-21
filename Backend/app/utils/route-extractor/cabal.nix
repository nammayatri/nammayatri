{ mkDerivation, aeson, base, beckn-services, bytestring, cassava
, containers, cryptonite, either, euler-hs, exceptions
, generic-lens, hspec, http-client, lens, lib, mobility-core
, openapi3, persistent, prometheus-client, record-dot-preprocessor
, record-hasfield, resource-pool, servant, servant-client
, servant-client-core, servant-openapi3, servant-server, stm
, string-conversions, text, text-conversions, time, uuid, vector
, wai, warp, xml-conduit
}:
mkDerivation {
  pname = "route-extractor";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beckn-services bytestring cassava containers cryptonite
    either euler-hs exceptions generic-lens hspec http-client lens
    mobility-core openapi3 persistent prometheus-client
    record-dot-preprocessor record-hasfield resource-pool servant
    servant-client servant-client-core servant-openapi3 servant-server
    stm string-conversions text text-conversions time uuid vector wai
    warp xml-conduit
  ];
  executableHaskellDepends = [
    aeson base beckn-services bytestring cassava containers cryptonite
    either euler-hs exceptions generic-lens hspec http-client lens
    mobility-core openapi3 persistent prometheus-client
    record-dot-preprocessor record-hasfield resource-pool servant
    servant-client servant-client-core servant-openapi3 servant-server
    stm string-conversions text text-conversions time uuid vector wai
    warp xml-conduit
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "route-extractor-exe";
}
