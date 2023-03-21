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

    packages =
      let
        # Local haskell packages containing only the binaries.
        exes = lib.mapAttrs
          (_: p: pkgs.haskell.lib.justStaticExecutables p)
          (config.haskellProjects.default.outputs.localPackages);
      in
      {
        nammayatri = pkgs.symlinkJoin {
          name = "nammayatri-exes";
          paths = lib.attrValues exes;
          postBuild = ''
            # Our k8s deployment config is hardcoded to look for exes under
            # /opt/app
            mkdir $out/opt && mv $out/bin $out/opt/app
          '';
        };

        # This is used in docker image (docker.nix)
        # TODO: Remove this after disabling auto wiring via https://github.com/srid/haskell-flake/issues/62
        nammayatri-rider-app = exes.rider-app;
      };
  };
}
