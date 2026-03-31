{ inputs, ... }:
let
  indiaDataFile = inputs.osrm-pbf;
  finlandDataFile = inputs.osrm-pbf-finland;
  dataName = "combined";
  customPort = "5001";
  # Bounding boxes for routing data extraction
  indiaBbox = "77.5500,12.9000,77.6500,13.0000"; # Central Bangalore
  finlandBbox = "24.0,60.0,25.5,60.5"; # Helsinki metro area
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

              # Extract India roads
              cp ${indiaDataFile} india_input.osm.pbf
              osmium tags-filter india_input.osm.pbf w/highway -o india_roads.pbf
              osmium extract --bbox ${indiaBbox} --strategy complete_ways india_roads.pbf -o india.osm.pbf
              rm india_input.osm.pbf india_roads.pbf

              # Extract Finland roads
              cp ${finlandDataFile} finland_input.osm.pbf
              osmium tags-filter finland_input.osm.pbf w/highway -o finland_roads.pbf
              osmium extract --bbox ${finlandBbox} --strategy complete_ways finland_roads.pbf -o finland.osm.pbf
              rm finland_input.osm.pbf finland_roads.pbf

              # Merge both regions
              osmium merge india.osm.pbf finland.osm.pbf -o ${dataName}.osm.pbf
              rm india.osm.pbf finland.osm.pbf

              # Build OSRM routing data
              osrm-extract --threads 1 -p ${pkgs.osrm-backend}/share/osrm/profiles/car.lua ${dataName}.osm.pbf
              osrm-partition ${dataName}.osrm
              osrm-customize ${dataName}.osrm
            '';
        osrm-server = pkgs.writeShellApplication {
          name = "osrm-server";
          runtimeInputs = [ pkgs.osrm-backend ];
          text = ''
            set -x
            osrm-routed \
              --algorithm mld \
              --port ${customPort} \
              ${osrm-data}/${dataName}.osrm
          '';
        };
      };
  };
}
