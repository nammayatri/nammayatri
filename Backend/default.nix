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

    packages.nammayatri =
      let
        exes = builtins.map pkgs.haskell.lib.justStaticExecutables (with config.haskellProjects.default.outputs.localPackages; [
          rider-app
          dynamic-offer-driver-app
          static-offer-driver-app
          static-offer-driver-app-allocator
          static-offer-driver-app-scheduler
          driver-offer-allocator
          rider-dashboard
          provider-dashboard
          driver-tracking-healthcheck
        ]);
      in
      pkgs.runCommand "nammayatri-exes" { } ''
        mkdir -p $out/bin
        ${lib.concatStringsSep ";" (builtins.map (exe: "cp -rv ${exe}/* $out/") exes)}
        # k8s deployment config is hardcoded to look for exes in /opt/app
        mkdir $out/opt && mv $out/bin $out/opt/app
      '';
  };
}
