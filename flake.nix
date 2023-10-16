{
  inputs = {
    common.url = "github:nammayatri/common";
    common.inputs.process-compose-flake.follows = "process-compose-flake";

    # Backend inputs
    shared-kernel.url = "github:nammayatri/shared-kernel";
    beckn-gateway.url = "github:nammayatri/beckn-gateway";
    beckn-gateway.inputs.common.follows = "common";
    beckn-gateway.inputs.shared-kernel.follows = "shared-kernel";
    passetto.url = "github:nammayatri/passetto/nixify";
    passetto.inputs.nixpkgs.follows = "common/nixpkgs";
    passetto.inputs.flake-parts.follows = "common/flake-parts";
    passetto.inputs.haskell-flake.follows = "common/haskell-flake";
    # TODO: update the commit in upstream and follow it
    # passetto.inputs.process-compose-flake.follows = "common/process-compose-flake";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    passetto.inputs.services-flake.follows = "services-flake";
    # Question: move this to common?
    services-flake.url = "github:juspay/services-flake";


    # We cannot use southern-zone-latest here, because the sha256 will change
    # over time.  NOTE: This file is not permanent, find the available one at
    # https://download.geofabrik.de/asia/india/
    # NOTE: If you change this, also change `openStreetDataFileName` in osrm.nix
    osrm-pbf.url = "http://download.geofabrik.de/asia/india/southern-zone-230801.osm.pbf";
    osrm-pbf.flake = false;

    easy-purescript-nix.url = "github:justinwoo/easy-purescript-nix/a90bd941297497c83205f0a64f30c5188a2a4fda";
    easy-purescript-nix.flake = false;
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
