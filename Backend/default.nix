{ inputs, ... }:
{
  imports = [
    ./nix/docker.nix
    ./nix/scripts.nix
    ./nix/run-mobility-stack.nix
    ./nix/arion-configuration.nix
    ./nix/osrm.nix
  ];
  perSystem = { config, self', pkgs, lib, ... }: {
    pre-commit.settings.imports = [
      ./nix/pre-commit.nix
    ];

    haskellProjects.default = {
      projectRoot = ./.;
      imports = [
        inputs.beckn-gateway.haskellFlakeProjectModules.output
      ];
      autoWire = [ "packages" "checks" "apps" ];
      devShell.tools = _: {
        inherit (self'.packages)
          arion;
      };
      # Some tests fail under Nix. We shoud probably run them in CI directly.
      overrides = self: super:
        with pkgs.haskell.lib.compose;
        lib.mapAttrs (k: lib.pipe super.${k}) {
          # location-updates-tests: Network.Socket.connect: <socket: 6>: does not exist (Connection refused)
          location-updates = [ dontCheck ];
          # tries to find dhall files from wrong CWD
          beckn-test = [ dontCheck ];
        };
    };

    packages = {
      # The final nammayatri package containing the various executables.
      nammayatri =
        let
          localCabalPackages = builtins.map
            (p: pkgs.haskell.lib.justStaticExecutables p.package)
            (lib.attrValues config.haskellProjects.default.outputs.packages);
        in
        pkgs.symlinkJoin {
          name = "nammayatri-exes";
          paths = localCabalPackages;
        };
      # The nammayatri dist to be deployed in Docker image
      nammayatri-dist = pkgs.symlinkJoin {
        name = "nammayatri-dist";
        paths = [ self'.packages.nammayatri ];
        postBuild = ''
          mkdir $out/opt
          # Rationale: Our k8s deployment config is hardcoded to look for exes
          # under /opt/app.
          mv $out/bin $out/opt/app
          cp -r ${./dhall-configs} $out/opt/app/dhall-configs
          cp -r ${./swagger} $out/opt/app/swagger
        '';
      };
    };

  };
}
