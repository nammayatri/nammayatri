{ mkDerivation, aeson, async, base, beckn-gateway, bytestring
, dashboard-helper-api, data-default-class, driver-offer-allocator
, dynamic-offer-driver-app, euler-hs, generic-lens, hspec
, hspec-core, hspec-expectations-lifted, http-client
, http-client-tls, http-types, HUnit, lens, lib, location-updates
, mobility-core, mock-fcm, mock-google
, mock-public-transport-provider-platform, mock-registry, mock-sms
, public-transport-rider-platform, public-transport-search-consumer
, record-dot-preprocessor, record-hasfield, rider-app
, search-result-aggregator, sequelize, servant, servant-client
, servant-server, string-conversions, tasty, tasty-hspec, text
, time, unix, utf8-string, uuid, wai, warp
}:
mkDerivation {
  pname = "beckn-test";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base beckn-gateway bytestring dashboard-helper-api
    data-default-class driver-offer-allocator dynamic-offer-driver-app
    euler-hs generic-lens hspec hspec-core hspec-expectations-lifted
    http-client http-client-tls http-types HUnit lens location-updates
    mobility-core mock-fcm mock-google
    mock-public-transport-provider-platform mock-registry mock-sms
    public-transport-rider-platform public-transport-search-consumer
    record-dot-preprocessor record-hasfield rider-app
    search-result-aggregator sequelize servant servant-client
    servant-server string-conversions tasty tasty-hspec text time unix
    utf8-string uuid wai warp
  ];
  testHaskellDepends = [
    aeson async base beckn-gateway bytestring dashboard-helper-api
    data-default-class driver-offer-allocator dynamic-offer-driver-app
    euler-hs generic-lens hspec hspec-core hspec-expectations-lifted
    http-client http-client-tls http-types HUnit location-updates
    mobility-core mock-fcm mock-google
    mock-public-transport-provider-platform mock-registry mock-sms
    public-transport-rider-platform public-transport-search-consumer
    record-dot-preprocessor record-hasfield rider-app
    search-result-aggregator sequelize servant servant-client
    servant-server string-conversions tasty tasty-hspec text time unix
    utf8-string uuid wai warp
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
