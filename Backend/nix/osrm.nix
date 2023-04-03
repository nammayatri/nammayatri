{ ... }:

let
  # We cannot use southern-zone-latest here, because the sha256 will change over
  # time.
  openStreetDataFileName = "southern-zone-230401";
  openStreetDataFile = builtins.fetchurl {
    url = "http://download.geofabrik.de/asia/india/${openStreetDataFileName}.osm.pbf";
    sha256 = "sha256:1c1vmcp5b7wfayhxh91b9641d3x7smfms51v9kl7vkk412zhpg5m";
  };
in
{
  perSystem = { config, pkgs, lib, system, ... }: {
    packages =
      rec {
        osrm-data =
          pkgs.runCommandNoCC "osrm"
            { buildInputs = [ pkgs.osrm-backend ]; }
            ''
              mkdir $out && cd $out
              cp ${openStreetDataFile} ${openStreetDataFileName}.osm.pbf
              osrm-extract -p ${pkgs.osrm-backend}/share/osrm/profiles/car.lua ${openStreetDataFileName}.osm.pbf
              osrm-partition ${openStreetDataFileName}.osrm
              osrm-customize ${openStreetDataFileName}.osrm
            '';

        osrm-server = pkgs.writeShellApplication {
          name = "osrm-server";
          runtimeInputs = [ pkgs.osrm-backend ];
          text = ''
            set -x
            osrm-routed --algorithm mld \
               ${osrm-data}/southern-zone-latest.osrm
          '';
        };
      };
  };
}
