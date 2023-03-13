{ self, inputs, ... }:
{
  imports = [
    ./nix/docker.nix
  ];
  perSystem = { config, self', system, pkgs, lib, ... }: {
    haskellProjects.default = {
      projectRoot = ./.;
      imports = [
        inputs.beckn-gateway.haskellFlakeProjectModules.output
      ];
      basePackages = config.haskellProjects.ghc810.outputs.finalPackages;
      devShell = {
        tools = hp: {
          dhall = pkgs.dhall;
        };
      };
      overrides = self: super:
        with pkgs.haskell.lib.compose;
        lib.mapAttrs (k: v: lib.pipe super.${k} v) {
          # FIXME: location-updates-tests: Network.Socket.connect: <socket: 6>: does not exist (Connection refused)
          location-updates = [ dontCheck ];
          # FIXME: tries to find dhall files from wrong CWD
          beckn-test = [ dontCheck ];
        };
    };
  };
}
