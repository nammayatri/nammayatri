{ inputs, lib, flake-parts-lib, ... }:
let
  inherit (flake-parts-lib)
    mkPerSystemOption;
  inherit (lib)
    types;
in
{
  options = {
    perSystem = mkPerSystemOption
      ({ config, self', lib, pkgs, system, ... }:
        let
          nodePackages = inputs.dream2nix.lib.makeFlakeOutputs {
            systems = [ system ];
            source = ./.;
            projects = {
              "atlas-ui" = {
                name = "atlas-ui";
                subsystem = "nodejs";
                translator = "package-lock";
                builder = "strict-builder";
                subsystemInfo = {
                  nodejs = 14;
                };
              };
            };
          };

          nodeDependencies = nodePackages.packages.${system}.atlas-ui.lib;
        in
        {
          options.nammayatri = lib.mkOption {
            type = types.submodule {
              options = {
                nodeDependencies = lib.mkOption {
                  type = types.package;
                };
                nodejs = lib.mkOption {
                  type = types.package;
                };
              };
            };
          };
          config.nammayatri = {
            inherit nodeDependencies;
            nodejs = pkgs.nodejs-14_x;
          };
        });
  };
}
