{ inputs, ... }:
{
  imports = [
    ./nix/docker.nix
  ];
  perSystem = { config, self', pkgs, lib, ... }: {
    haskellProjects.default = {
      projectRoot = ./.;
      imports = [
        inputs.beckn-gateway.haskellFlakeProjectModules.output
      ];
      devShell = {
        tools = hp: {
          dhall = pkgs.dhall;
        };
        # TODO: Upstream mkShellArgs as an option in mission-control
        mkShellArgs = {
          nativeBuildInputs = [ config.mission-control.wrapper ];
          shellHook = config.mission-control.banner;
        };
      };

      # Some tests fail under Nix. We shoud probably run them in CI directly.
      overrides = self: super:
        with pkgs.haskell.lib.compose;
        lib.mapAttrs (k: v: lib.pipe super.${k} v) {
          # location-updates-tests: Network.Socket.connect: <socket: 6>: does not exist (Connection refused)
          location-updates = [ dontCheck ];
          # tries to find dhall files from wrong CWD
          beckn-test = [ dontCheck ];
        };
    };

    mission-control.scripts = {
      backend-ghcid = {
        description = "Compile the given local package using ghcid.";
        exec = ''
          set +x
          cd ./Backend # TODO: https://github.com/Platonic-Systems/mission-control/issues/27
          ghcid -c "cabal repl $1"
        '';
        category = "Backend";
      };

      backend-hoogle = {
        description = "Run Hoogle server for Haskell packages.";
        exec = ''
          echo "#### Hoogle running at: http://localhost:8090"
          hoogle serve --local --port 8090
        '';
      };
    };

    packages =
      let
        # Local haskell packages containing only the binaries.
        exes = lib.mapAttrs
          (_: p: pkgs.haskell.lib.justStaticExecutables p)
          (config.haskellProjects.default.outputs.localPackages);
      in
      {
        # The final nammayatri package containing the various executables.
        nammayatri = pkgs.symlinkJoin {
          name = "nammayatri-exes";
          paths = lib.attrValues exes;
        };
      };
  };
}
