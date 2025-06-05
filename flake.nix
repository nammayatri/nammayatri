{
  nixConfig = {
    # Workaround https://github.com/nammayatri/nammayatri/pull/9493#issuecomment-2506672419
    max-call-depth = "1000000";
    # Nix cache
    extra-substituters = "https://ny-ci-nixos.betta-gray.ts.net/";
    extra-trusted-public-keys = "ny-ci-nixos.betta-gray.ts.net:tjYdPZNppaGd6L9m7cMGzib4kkch1zAuR660dYp1DiY=";
  };

  inputs = {
    common.url = "github:nammayatri/common";
    nixpkgs.follows = "common/nixpkgs";
    haskell-flake.follows = "common/haskell-flake";

    # Backend inputs
    shared-kernel = {
      url = "github:nammayatri/shared-kernel";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    namma-dsl = {
      url = "github:nammayatri/namma-dsl";
    };

    haskell-cac = {
      url = "github:piyushKumar-1/haskell_cac_client/Testing";
      inputs = {
        common.follows = "common";
        nixpkgs.follows = "common/nixpkgs"; # nix eval is failing in pipeline without giving proper error message #36 for nix update https://github.com/srid/nixci/issues/36
      };
    };

    beckn-gateway = {
      url = "github:nammayatri/beckn-gateway";
      inputs = {
        common.follows = "common";
        haskell-flake.follows = "haskell-flake";
        nixpkgs.follows = "nixpkgs";
        shared-kernel.follows = "shared-kernel";
      };
    };

    location-tracking-service.url = "github:nammayatri/location-tracking-service";

    # https://github.com/nammayatri/passetto/pull/8
    passetto = {
      url = "github:nammayatri/passetto/use-crypton";
      inputs = {
        nixpkgs.follows = "common/nixpkgs";
        flake-parts.follows = "common/flake-parts";
        haskell-flake.follows = "common/haskell-flake";
        process-compose-flake.follows = "common/process-compose-flake";
        services-flake.follows = "services-flake";
      };
    };
    # Question: move this to common?
    services-flake.url = "github:juspay/services-flake";

    # We cannot use southern-zone-latest here, because the sha256 will change
    # over time.  NOTE: This file is not permanent, find the available one at
    # https://download.geofabrik.de/asia/india/
    # NOTE: If you change this, also change `openStreetDataFileName` in osrm.nix
    osrm-pbf.url = "https://download.geofabrik.de/asia/india/southern-zone-240101.osm.pbf";
    osrm-pbf.flake = false;

    easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix/a90bd941297497c83205f0a64f30c5188a2a4fda";
    easy-purescript-nix.flake = false;

    # Amazonka 2.0 tagged release
    # amazonka-2.0 flake seems broken still.
    amazonka-git.url = "github:brendanhay/amazonka?ref=2.0.0";
    amazonka-git.flake = false;

    json-logic-hs.url = "github:nammayatri/json-logic-hs";
    json-logic-hs.flake = false;
  };

  outputs = inputs:
    inputs.common.lib.mkFlake { inherit inputs; } {
      imports = [
        ./Backend/default.nix
        ./Frontend/default.nix
      ];
    };
}
