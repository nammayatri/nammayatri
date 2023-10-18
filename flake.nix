{
  inputs = {
    # Note: replace with upstream when GHC 9.2 changes get merged there
    # common.url = "github:nammayatri/common";
    common.url = "github:arjunkathuria/common/Mobility-GHC927-rebased-03";
    nixpkgs.follows = "common/nixpkgs";
    haskell-flake.follows = "common/haskell-flake";

    # Backend inputs
    # Note: replace with upstream when GHC 9.2 changes get merged there
    # shared-kernel.url = "github:nammayatri/shared-kernel";
    shared-kernel = {
      url = "github:arjunkathuria/shared-kernel/Mobility-GHC927-rebased-03";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # beckn-gateway.url = "github:nammayatri/beckn-gateway";
    beckn-gateway = {
      url = "github:arjunkathuria/beckn-gateway/Mobility-GHC927-rebased-03";
      inputs = {
        common.follows = "common";
        haskell-flake.follows = "haskell-flake";
        nixpkgs.follows = "nixpkgs";
        shared-kernel.follows = "shared-kernel";
      };
    };

    # We cannot use southern-zone-latest here, because the sha256 will change
    # over time.  NOTE: This file is not permanent, find the available one at
    # https://download.geofabrik.de/asia/india/
    # NOTE: If you change this, also change `openStreetDataFileName` in osrm.nix
    osrm-pbf.url = "http://download.geofabrik.de/asia/india/southern-zone-230801.osm.pbf";
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
      };
    };
}
