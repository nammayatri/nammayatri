{ mkDerivation, aeson, base, base64-bytestring, beckn-spec
, bytestring, directory, euler-hs, lens, lib, mobility-core
, optparse-applicative, record-dot-preprocessor, record-hasfield
, text, time
}:
mkDerivation {
  pname = "beckn-cli";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base base64-bytestring beckn-spec bytestring directory
    euler-hs lens mobility-core optparse-applicative
    record-dot-preprocessor record-hasfield text time
  ];
  executableHaskellDepends = [
    aeson base base64-bytestring beckn-spec bytestring directory
    euler-hs mobility-core optparse-applicative record-dot-preprocessor
    record-hasfield text time
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "beckn-cli-exe";
}
