{ mkDerivation, aeson, base, bytestring, case-insensitive, clock
, containers, exceptions, extra, hedis, http-client, lib
, mobility-core, openapi3, public-transport-rider-platform
, record-dot-preprocessor, record-hasfield, relude, scientific
, servant-client, servant-client-core, servant-server
, string-conversions, text, time, transformers, unliftio, vector
, wai, warp
}:
mkDerivation {
  pname = "mock-public-transport-provider-platform";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring case-insensitive clock containers exceptions
    extra hedis http-client mobility-core openapi3
    public-transport-rider-platform record-dot-preprocessor
    record-hasfield relude scientific servant-client
    servant-client-core servant-server string-conversions text time
    transformers unliftio vector wai warp
  ];
  executableHaskellDepends = [
    aeson base bytestring case-insensitive clock containers exceptions
    extra hedis http-client mobility-core openapi3
    public-transport-rider-platform record-dot-preprocessor
    record-hasfield relude scientific servant-client
    servant-client-core servant-server string-conversions text time
    transformers unliftio vector wai warp
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "mock-public-transport-provider-platform-exe";
}
