{ inputs, ... }:
{
  imports = [
    ./node
    ./nix/watch.nix
    ./nix/bundle.nix
  ];

  perSystem = { config, self', lib, system, ... }:
    let
      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.purifix.overlay
        ];
      };

      localPackages = pkgs.purifix {
        src = ./.;
      };

      # Wrapper for typescript language server to make it work with vscode/neovim by default
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
      treefmt.config = {
        # Suppress autoformatting of frontend dhall files.
        settings.formatter.dhall.excludes = [
          "Frontend/packages.dhall"
        ];
      };

      devShells.frontend = pkgs.mkShell {
        name = "ps-dev-shell";
        inputsFrom = [
          config.mission-control.devShell
          config.pre-commit.devShell
          self'.packages.ui-common.develop
        ];
        packages = [
          pkgs.dhall
          config.nammayatri.nodejs
          typescript-language-server
        ];
        shellHook = ''
          export NODE_PATH="${config.nammayatri.nodeDependencies}/node_modules"
        '';
      };

      packages = {
        inherit (localPackages) ui-customer ui-driver ui-common;
        android-customer-bundle = self'.packages.ui-customer-android-prod-production-js;
        android-driver-bundle = self'.packages.ui-driver-android-prod-production-js;
      };
    };
}
