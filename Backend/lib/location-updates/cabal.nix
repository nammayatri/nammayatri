{ mkDerivation, aeson, base, base16-bytestring, beckn-spec
, bytestring, containers, cryptohash, euler-hs, exceptions, hspec
, http-client, http-types, lens, lib, mobility-core, mock-google
, QuickCheck, record-dot-preprocessor, record-hasfield, servant
, servant-client, servant-server, tasty, tasty-hspec, tasty-hunit
, tasty-quickcheck, text, time, unordered-containers, uri-encode
}:
mkDerivation {
  pname = "location-updates";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base16-bytestring bytestring containers cryptohash
    euler-hs exceptions http-client http-types mobility-core
    record-dot-preprocessor record-hasfield servant servant-client
    servant-server text time unordered-containers uri-encode
  ];
  testHaskellDepends = [
    aeson base base16-bytestring beckn-spec bytestring containers
    cryptohash euler-hs exceptions hspec http-client http-types lens
    mobility-core mock-google QuickCheck record-dot-preprocessor
    record-hasfield servant servant-client servant-server tasty
    tasty-hspec tasty-hunit tasty-quickcheck text time
    unordered-containers uri-encode
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
