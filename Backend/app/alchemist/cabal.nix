{ mkDerivation, aeson, base, bytestring, casing, directory, extra
, filepath, lib, mobility-core, namma-dsl, record-dot-preprocessor
, record-hasfield, template-haskell, text, time, turtle, yaml
}:
mkDerivation {
  pname = "alchemist";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring casing directory extra filepath mobility-core
    namma-dsl record-dot-preprocessor record-hasfield template-haskell
    text time turtle yaml
  ];
  homepage = "https://github.com/nammayatri/nammayatri#readme";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "alchemist-generator-exe";
}
