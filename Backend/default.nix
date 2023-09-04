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
      defaults.settings.default = { name, package, config, ... }:
        lib.optionalAttrs (package.local.toDefinedProject or false) {
          # Disabling haddock and profiling is mainly to speed up Nix builds.
          haddock = lib.mkDefault false;
          # Avoid double-compilation.
          libraryProfiling = lib.mkDefault false;
          # The use of -fwhole-archive-hs-libs (see hpack config in common repo)
          # breaks builds on macOS (cyclic references between bin and out); this
          # works around that.
          separateBinOutput = if pkgs.stdenv.isDarwin then false else null;
        };
      autoWire = [ "packages" "checks" "apps" ];
      devShell.tools = _: {
        inherit (self'.packages)
          arion;
      };
      packages = {
        cryptostore.source = "0.2.3.0";
      };
      settings = {
        location-updates.check = false;
        beckn-test.check = false;
      };
    };

    packages = {
      # The final nammayatri package containing the various executables and
      # configuration files.
      nammayatri =
        let
          localCabalPackages = builtins.map
            (p: if p.exes != { } then lib.getBin p.package else null)
            (lib.attrValues config.haskellProjects.default.outputs.packages);
        in
        pkgs.symlinkJoin {
          name = "nammayatri";
          paths = localCabalPackages;
          postBuild = ''
            # Prepare /opt/app layout for Docker image.
            # Rationale: Our k8s deployment config is hardcoded to look for exes
            # under /opt/app.
            mkdir -p $out/opt/app
            for f in `${lib.getExe pkgs.fd} . $out/bin/`; do
              ln -s $f $out/opt/app/
            done
            cp -r ${./dhall-configs} $out/opt/app/dhall-configs
            cp -r ${./swagger} $out/opt/app/swagger
          '';
        };
    };

    devShells.backend = pkgs.mkShell {
      name = "ny-backend";
      # cf. https://haskell.flake.page/devshell#composing-devshells
      inputsFrom = [
        config.mission-control.devShell
        config.pre-commit.devShell
        config.haskellProjects.default.outputs.devShell
        config.flake-root.devShell
      ];
    };
  };
}
