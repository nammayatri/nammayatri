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
      autoWire = false;
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

    packages = {
      # The final nammayatri package containing the various executables.
      nammayatri = pkgs.symlinkJoin {
        name = "nammayatri-exes";
        paths = lib.attrValues config.localPackagesStatic;
      };
    };
  };
}
