{ inputs, ... }:

let
  openStreetDataFile = inputs.osrm-pbf;
  # NOTE: This *should* match the flake input.
  openStreetDataFileName = "southern-zone-230101";
  # Custom Port for OSRM
  customPort = "5001";
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
               --port ${customPort} \
               ${osrm-data}/${openStreetDataFileName}.osrm
          '';
        };
      };
  };
}
