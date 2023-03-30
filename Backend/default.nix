{ inputs, ... }:
{
  imports = [
    ./nix/exe.nix
    ./nix/docker.nix
    ./nix/scripts.nix
    ./nix/run-mobility-stack.nix
  ];
  perSystem = { config, self', pkgs, lib, ... }: {
    haskellProjects.default = {
      projectRoot = ./.;
      imports = [
        inputs.beckn-gateway.haskellFlakeProjectModules.output
      ];
      devShell = {
        # TODO: Upstream mkShellArgs as an option in mission-control
        mkShellArgs = {
          nativeBuildInputs = [ config.mission-control.wrapper ];
          shellHook = ''
            ${config.pre-commit.installationScript}
            ${config.mission-control.banner}
          '';
        };
      };

      source-overrides = {
        hspec = "2.7.6";
        hspec-meta = "2.6.0";
        hspec-core = "2.7.6";
        hspec-discover = "2.7.6";
        tasty-hspec = "1.1.6";

        #hex-text = "0.1.0.0";
        #base16-bytestring = "0.1.1.7";
      };

      # Some tests fail under Nix. We shoud probably run them in CI directly.
      overrides = self: super:
        with pkgs.haskell.lib.compose;
        lib.mapAttrs (k: v: lib.pipe super.${k} v) {
          # location-updates-tests: Network.Socket.connect: <socket: 6>: does not exist (Connection refused)
          location-updates = [ dontCheck ];
          # tries to find dhall files from wrong CWD
          beckn-test = [ dontCheck ];

          hex-text = [ dontCheck ];
        };
    };

    packages = {
      # The final nammayatri package containing the various executables.
      nammayatri = pkgs.symlinkJoin {
        name = "nammayatri-exes";
        paths = lib.attrValues config.localPackagesStatic;
      };
    };
  };
}
