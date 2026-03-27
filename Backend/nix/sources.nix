# Pinned source dependencies.
#
# These are fetched directly instead of as flake inputs to avoid the ~1.5s
# per-input verification overhead that nix imposes on every evaluation.
# See: https://github.com/juspay/kolu/pull/152
{
  amazonka-git = builtins.fetchTarball {
    url = "https://github.com/brendanhay/amazonka/archive/e081929d5a81f30397c9c9a219b154da1a922fb8.tar.gz";
    sha256 = "sha256-ml7tGa+hCVbhBWaGDUOVM67Mh+F8R5WU7cLdcMqdVkw=";
  };

  google-cloud-haskell = builtins.fetchTarball {
    url = "https://github.com/tusharad/google-cloud-haskell/archive/cd051e5ab9c20dcfd2d2ffa68d0c6b69703fd459.tar.gz";
    sha256 = "sha256-2y47nFBDKl3iJgkoM+40VUtn1nhIHgX+5Vv7x2bATWk=";
  };

  json-logic-hs = builtins.fetchTarball {
    url = "https://github.com/nammayatri/json-logic-hs/archive/d2d9945c64ffdcb1053dc5592d59c8da10b5fcf2.tar.gz";
    sha256 = "sha256-ahpXlsVQW/b7jTK465Jo/nUijQPshK+83J+htfRqNug=";
  };

  osrm-pbf = builtins.fetchurl {
    url = "https://download.geofabrik.de/asia/india/southern-zone-240101.osm.pbf";
    sha256 = "sha256-cyhgTR+py8e5RBTHM94ZAIxTe5V3fy8peba+4bp/IVQ=";
  };
}
