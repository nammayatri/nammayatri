{ inputs, ... }:
let
  openStreetDataFile = inputs.osrm-pbf;
  openStreetDataFileName = "southern-zone-230101";
  cityName = "bangalore";
  customPort = "5001";
  # Bangalore bounding box - slightly reduced area to test
  bbox = "77.5500,12.9000,77.6500,13.0000"; # Central Bangalore for testing
in
{
  perSystem = { pkgs, lib, ... }: {
    packages =
      rec {
        osrm-data =
          pkgs.runCommandNoCC "osrm-data"
            {
              buildInputs = [ pkgs.osmium-tool pkgs.osrm-backend ];
            }
            ''
              mkdir $out && cd $out
              cp ${openStreetDataFile} input.osm.pbf
              osmium tags-filter input.osm.pbf w/highway -o roads.pbf
              osmium extract --bbox ${bbox} --strategy complete_ways --input-format osm.pbf --output-format osm.pbf roads.pbf -o ${cityName}.osm.pbf
              rm input.osm.pbf roads.pbf
              osrm-extract --threads 1 -p ${pkgs.osrm-backend}/share/osrm/profiles/car.lua ${cityName}.osm.pbf
              osrm-partition ${cityName}.osrm
              osrm-customize ${cityName}.osrm
            '';
        osrm-server = pkgs.writeShellApplication {
          name = "osrm-server";
          runtimeInputs = [ pkgs.osrm-backend ];
          text = ''
            set -x
            osrm-routed \
              --algorithm mld \
              --port ${customPort} \
              ${osrm-data}/${cityName}.osrm
          '';
        };
      };
  };
}
