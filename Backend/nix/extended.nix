# Nammayatri extended
{ self, inputs, ... }:

let
  extensions = [
    "ny-example"
  ];
in
{
  flake = {
    lib.mkNammayatriFlake = args: name:
      inputs.common.lib.mkFlake
        args
        (self.flakeModules.extended-nammayatri name);

    flakeModules.extended-nammayatri = name: {
      perSystem = { config, pkgs, self', ... }: {
        # Create a Haskell project that uses nammayatri as a dependency.
        haskellProjects.default = {
          imports = [
            self.haskellFlakeProjectModules.output
          ];
          autoWire = [ "packages" "checks" ];
        };
        packages.default = self'.packages.${name};
        devShells.default = pkgs.mkShell {
          # cf. https://haskell.flake.page/devshell#composing-devshells
          inputsFrom = [
            config.mission-control.devShell
            config.pre-commit.devShell
            config.haskellProjects.default.outputs.devShell
            config.flake-root.devShell
          ];
        };
      };
    };
  };

  perSystem = { config, lib, ... }: {
    packages = lib.genAttrs extensions (name:
      with config.haskellProjects.default.outputs.finalPackages;
      callCabal2nix name inputs.${name} { }
    );
  };
}
