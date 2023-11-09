{
  inputs = {
    common.url = "github:nammayatri/common";

    # Backend inputs
    shared-kernel.url = "path:/home/akhilesh/Desktop/projects/shared-kernel";
    beckn-gateway.url = "path:/home/akhilesh/Desktop/projects/beckn-gateway";
    beckn-gateway.inputs.common.follows = "common";
    beckn-gateway.inputs.shared-kernel.follows = "shared-kernel";
    location-tracking-service.url = "github:nammayatri/location-tracking-service";

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
