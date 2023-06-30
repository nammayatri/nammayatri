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

      # Wrapper for typescript language server to make it work with neovim(and other editors) by default
      # Ensure that typescript is installed and the language server knows where tsserver lives
      # The --tsserver-path argument being hardcoded instead of sent in the initialization
      # both simplifies the process and ensures that everyone is using the same tsserver when
      # the editor is running in the development shell
      typescript-language-server = pkgs.symlinkJoin {
        name = "typescript-language-server";
        nativeBuildInputs = [ pkgs.makeWrapper ];
        paths = [
          pkgs.nodePackages.typescript
          pkgs.nodePackages.typescript-language-server
        ];
        postBuild = ''
          wrapProgram $out/bin/typescript-language-server \
            --add-flags "--tsserver-path $out/bin/tsserver"
        '';
      };
    in
    {
      packages = {
        inherit nodeDependencies;
        nodejs = pkgs.nodejs-14_x;

      };
      devShells.node = pkgs.mkShell {
        buildInputs = [
          self'.packages.nodejs
          typescript-language-server
        ];
        NODE_PATH = "${nodeDependencies}/node_modules";
      };
    };
}
