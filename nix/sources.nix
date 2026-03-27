# Pinned source dependencies — fetched directly to avoid flake input
# verification overhead (~1.5s per input).
# See: https://github.com/juspay/kolu/pull/152
#
# To update a pin: change the rev and set sha256 = "" to get the new hash.
let
  fetchGH = owner: repo: rev: sha256:
    builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
      inherit sha256;
    };
  fetchGL = owner: repo: rev: sha256:
    builtins.fetchTarball {
      url = "https://gitlab.com/${owner}/${repo}/-/archive/${rev}/${repo}-${rev}.tar.gz";
      inherit sha256;
    };
in
{
  # --- Core framework ---
  nixpkgs = fetchGH "nixos" "nixpkgs" "0c7ffbc66e6d78c50c38e717ec91a2a14e0622fb" "sha256-s8TtSYJ1LBpuITXjbPLUPyxzAKw35LhETcajJjCS5f0=";
  flake-parts = fetchGH "hercules-ci" "flake-parts" "567b938d64d4b4112ee253b9274472dc3a346eb6" "sha256-+ebgonl3NbiKD2UD0x4BszCZQ6sTfL4xioaM49o5B3Y=";
  haskell-flake = fetchGH "srid" "haskell-flake" "2ee7904390ce78a81d0f66fcc98bf4c32d128d33" "sha256-R2k/UG5XtyTtmezRs0HZZ9MlvhWXtkyHR+ngEAWrtZI=";
  systems = fetchGH "nix-systems" "default" "da67096a3b9bf56a91d16901293e51ba5b49a27e" "sha256-Vy1rq5AaRuLzOxct8nz4T6wlgyUR7zLU309k9mBC768=";

  # --- Common sub-modules ---
  common = fetchGH "nammayatri" "common" "3c6f4a5f658fcd053c1f7ec5467d482bd0dc7a0c" "sha256-PH9gUSWPHMjp8DK9Ij6weRuUcL0s0kEgM16u48Xp9iU=";
  flake-root = fetchGH "srid" "flake-root" "6000244701c8ae8cf43263564095fd357d89c328" "sha256-VgFdqsu2i2oUcId9n8BFlRRc4GJHFvrmprdamcSZR1o=";
  mission-control = fetchGH "Platonic-Systems" "mission-control" "65d04c4ab9db076eff09824d2936a5c215c21f36" "sha256-1tt43rwHk0N5fwEhbpsHWO4nBVFCQN0w1KM427DNycM=";
  treefmt-nix = fetchGH "numtide" "treefmt-nix" "6d8bea2820630576ad8c3a3bde2c95c38bcc471f" "sha256-dGR2FRxWswpQCHdivejB3uiLZPktnT3DYp6ZkybR/SE=";
  pre-commit-hooks-nix = fetchGH "cachix" "pre-commit-hooks.nix" "fb58866e20af98779017134319b5663b8215d912" "sha256-Hf9XVpqaGqe/4oDGr30W8HlsWvJXtMsEPHDqHZA6dDg=";
  process-compose-flake = fetchGH "Platonic-Systems" "process-compose-flake" "c8942208e7c7122aef4811a606f7c12ad0753a98" "sha256-kSLMsz0tXc1N5Jx8ghEXhLmK4X3Bktt3S4ttXJXCzCo=";
  process-compose = fetchGH "F1bonacc1" "process-compose" "d83409b3216099ee15c5825cf8413eb94c79ffd6" "sha256-+r5l5OOndywrPqr+XRMPHudN+LBIxDsIEY07+vgVlk0=";
  nixpkgs-21_11 = fetchGH "nixos" "nixpkgs" "eabc38219184cc3e04a974fe31857d8e0eac098d" "sha256-hekabNdTdgR/iLsgce5TGWmfIDZ86qjPhxDg/8TlzhE=";
  nixpkgs-140774-workaround = fetchGH "srid" "nixpkgs-140774-workaround" "335c2052970106d8f09f6f7f522f29d6ccaa2416" "sha256-l5BdAeq9ymhUox0sTqeKibx9zkreWbIllwTvCamJLE8=";
  nixpkgs-latest = fetchGH "nixos" "nixpkgs" "7eee17a8a5868ecf596bbb8c8beb527253ea8f4d" "sha256-uNfh3aMyCekMpjtL/PZtl2Hz/YqNuUpCBEzVxt1QYck=";

  # --- Services ---
  services-flake = fetchGH "juspay" "services-flake" "b93a612aa7057fbb395c79a915672f9b6567ffea" "sha256-JDOCKKR8kREeKd7/nMPNPhT2F/bGADxyoy3B7GWWKw8=";
  passetto = fetchGH "nammayatri" "passetto" "a1e0a291115125de01458f8830f781b868bf5eb4" "sha256-l42/tDSf2qe82ah6XKveinKkVgRs11bmyendBEfYX8Y=";
  location-tracking-service = fetchGH "nammayatri" "location-tracking-service" "0e9a41b0ac1b2d58e78e1ef6924309fb2b1e688c" "sha256-zw8fKOzDCy8W5ccF2ArssCL+Otinpn33xDpBTEyQrT8=";

  # --- Haskell library sources ---
  shared-kernel = fetchGH "nammayatri" "shared-kernel" "5cc4e892eb07847329506daed3e312600f2171cb" "sha256-exyKAgOrkHv05Jjqzp4aiAlsVOdAfTZYxvTK8PmFL2s=";
  beckn-gateway = fetchGH "nammayatri" "beckn-gateway" "c03d1f9379cfc370da6cd188960fab160141456d" "sha256-ig+Rx8RYETGmnfLHXqyvtSf0CUB1IPm/BwLrGctdYno=";
  haskell-cac = fetchGH "piyushKumar-1" "haskell_cac_client" "26bcb2cd5c0c42fe179be733bb694fcd0967a3c4" "sha256-kXnm2lZ2dbQ9wPdHqTcgOcrW7aENTC4qvMq1YOP0F4A=";
  namma-dsl = fetchGH "nammayatri" "namma-dsl" "f02fcd337a8a1c37b1ca4d93673ef1fdf30913e2" "sha256-Qgdo5mnYvVInZf7WJY0gn0T8D43pdQJ1P5qLINyViGo=";
  euler-hs = fetchGH "nammayatri" "euler-hs" "bf99af696dca8d9c221e45b2715cf45a827b4fef" "sha256-FU9xn2nlxbJ1CF/cH/7HNjV5/JGvkInMGyOJRzQX9Ck=";
  clickhouse-haskell = fetchGH "nammayatri" "clickhouse-haskell" "5314118b2635724b87f7993da9dc8446602bcd65" "sha256-0Ovbd1CzgI4tSCTBe6z/7p8DpbajKyFlyS39hu4xpNY=";

  # Haskell transitive deps (from euler-hs, shared-kernel)
  euler-events-hs = fetchGH "juspay" "euler-events-hs" "214d436a83113c04b6f937c68488abf53beea4e8" "sha256-YXz1EjpFkgFAC3txAj/9ItuO0SQBKrOwykBmc9w4qXs=";
  juspay-extra = fetchGH "juspay" "euler-haskell-common" "4892071b7af07e5faa085e7917ab12110399f0e5" "sha256-EDdBN4Nfm/u+QRDZRv+vb8RfcJZpoToSr9pSBGc9vBI=";
  sequelize = fetchGH "nammayatri" "haskell-sequelize" "38a8266d1bfa0ade37aa6b3ded2e1435f6839e7e" "sha256-uhv/uKOwu1IfFnYyFFB4k5dMrYcorjs1DCd6ehm9ReA=";
  hedis = fetchGH "nammayatri" "hedis" "089d22be67103f613bd1eecbac8c2e6d6d0576c6" "sha256-4FjUqrP+63Te39UM9PctBS8cuJUiJYkZ3zIJ2IPeTA8=";
  cereal = fetchGH "juspay" "cereal" "ee7fc69f499e86b8274906ba183b60f4f08457a6" "sha256-dDUIny/9kZ0Bx/r/IPa9XRDcNT10XweR1uHqKx1CbAk=";
  servant-mock = fetchGH "arjunkathuria" "servant-mock" "17e90cb831820a30b3215d4f164cf8268607891e" "sha256-J7Lsa3zI1Z05Gtf5m1x4yRE5vRnuhZLWeUShtrhsPbk=";
  tinylog = fetchGL "arjunkathuria" "tinylog" "08d3b6066cd2f883e183b7cd01809d1711092d33" "sha256-o0WEbygbbfIYPonyY6ui1HcbaJcFdngjBhnjWbe65zs=";
  passetto-hs = fetchGH "juspay" "passetto" "bb92cf1dd9699662d2a7bb96cd6a6aed6f20e8ff" "sha256-iNdX1XMryrQ3IYmAgTI1wiBJ6Y+k265Snoluk4aMe7I=";
  prometheus-haskell = fetchGH "juspay" "prometheus-haskell" "f1d996bb317d0a50450ace2b4ae08b5afdf22955" "sha256-KWxG4SyXo39yG+7b1H0ULKzVu8Z1Hj8KCqS0rQKw3qU=";

  # Haskell libraries (beam ecosystem for sequelize)
  beam = fetchGH "arjunkathuria" "beam" "06bcc50997fdcfb87125bed252e888e5dd1e6d9c" "sha256-BIq3ZjZQWQ0w3zWA19zGBggiVVfnOzR5d4b7De0oVZY=";
  beam-mysql = fetchGH "arjunkathuria" "beam-mysql" "2ef13d8ecdcd0959b8604b3106b2013baf2ad272" "sha256-3qyg/Uj3A+MTS494VH2lTBtOz9uptgm+V1M1bPFxTX0=";
  mysql-haskell = fetchGH "arjunkathuria" "mysql-haskell" "2f4861667d19e84474700f32922c97af94f5dfc4" "sha256-UY9HRnqWXv5Ud6pfBa/fPNwjauDYpgzJoLcQb4NuH7I=";
  bytestring-lexing = fetchGH "juspay" "bytestring-lexing" "0a46db1139011736687cb50bbd3877d223bcb737" "sha256-n/5kFb5msE8NPQZf6bsm8MQh0RGDoOx6EJXoji6FPMs=";
  word24 = fetchGH "winterland1989" "word24" "445f791e35ddc8098f05879dbcd07c41b115cb39" "sha256-S37S10sJ45BulvpqJzlhX/J4hY7cW5jLM9nP4xAftac=";

  # --- Non-Haskell sources ---
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
    sha256 = "sha256-X6lL/orc9vJbVgtaKmufBcwFiggmM7C07M+FmadU0D8=";
  };
}
