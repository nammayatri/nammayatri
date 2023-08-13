_:

let
  # We cannot use southern-zone-latest here, because the sha256 will change over
  # time.
  # NOTE: This file is not permanent, find the available one at https://download.geofabrik.de/asia/india/
  openStreetDataFileName = "southern-zone-230812";
  openStreetDataFile = builtins.fetchurl {
    url = "http://download.geofabrik.de/asia/india/${openStreetDataFileName}.osm.pbf";
    sha256 = "sha256:181y4gnvmr9wbacjqs051j04bdwl4ajbl7j3n9gnp4al8ybpg52b";
  };
in
{
  perSystem = { pkgs, lib, ... }: {
    packages =
      rec {
        osrm-data =
          pkgs.runCommandNoCC "osrm-data"
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
               ${osrm-data}/${openStreetDataFileName}.osrm
          '';
        };
      };
  };
}
