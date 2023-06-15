{ inputs, ... }:
{
  perSystem = { config, self', lib, pkgs, system, ... }:
    let
      nodePackages = inputs.dream2nix.lib.makeFlakeOutputs {
        systems = [ system ];
        source = ./.;
        projects = {
          "common" = {
            name = "common";
            subsystem = "nodejs";
            translator = "package-lock";
            builder = "strict-builder";
            subsystemInfo = {
              nodejs = 14;
            };
          };
        };
      };

      nodeDependencies = nodePackages.packages.${system}.common.lib;
    in
    {
      packages = {
        inherit nodeDependencies;
        nodejs = pkgs.nodejs-14_x;
      };
      devShells.node = pkgs.mkShell {
        buildInputs = [
          self'.packages.nodejs
        ];
        NODE_PATH = "${nodeDependencies}/node_modules";
      };
    };
}
