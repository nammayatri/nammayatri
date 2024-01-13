{
  inputs = {
    common.url = "github:nammayatri/common";
    nixpkgs.follows = "common/nixpkgs";
    haskell-flake.follows = "common/haskell-flake";

    # Backend inputs
    shared-kernel = {
      url = "github:nammayatri/shared-kernel";
      inputs.nixpkgs.follows = "nixpkgs";
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

    location-tracking-service.url = "github:nammayatri/location-tracking-service/86def9d54734c0bfd45375ca97ece0a461254745";
    passetto = {
      url = "github:nammayatri/passetto/postgres-schemas";
      inputs = {
        nixpkgs.follows = "common/nixpkgs";
        flake-parts.follows = "common/flake-parts";
        haskell-flake.follows = "common/haskell-flake";
        process-compose-flake.follows = "common/process-compose-flake";
        services-flake.follows = "services-flake";
      };
    };
    # Question: move this to common?
    services-flake.url = "github:juspay/services-flake/postgres-schemas";

    # We cannot use southern-zone-latest here, because the sha256 will change
    # over time.  NOTE: This file is not permanent, find the available one at
    # https://download.geofabrik.de/asia/india/
    # NOTE: If you change this, also change `openStreetDataFileName` in osrm.nix
    osrm-pbf.url = "https://download.geofabrik.de/asia/india/southern-zone-230101.osm.pbf";
    osrm-pbf.flake = false;

    easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix/a90bd941297497c83205f0a64f30c5188a2a4fda";
    easy-purescript-nix.flake = false;

    # Amazonka 2.0 tagged release
    # amazonka-2.0 flake seems broken still.
    amazonka-git.url = "github:brendanhay/amazonka?ref=2.0.0";
    amazonka-git.flake = false;

  };

  outputs = inputs:
    inputs.common.lib.mkFlake { inherit inputs; } {
      imports = [
        ./Backend/default.nix
        ./Frontend/default.nix
      ];

      flake.nix-health.default = {
        caches.required = [ "https://nammayatri.cachix.org" ];
        direnv.required = true;
        system.min_ram = "24G";
      };
    };
}
