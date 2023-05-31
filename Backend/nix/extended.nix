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

  perSystem = { pkgs, config, lib, ... }:
    let
      build-haskell-package = import "${inputs.common.inputs.haskell-flake}/nix/build-haskell-package.nix" {
        inherit pkgs lib;
        inherit (config.haskellProjects.default) log;
        self = config.haskellProjects.default.outputs.finalPackages;
        super = null;
      };
    in
    {
      # To build all extensions in our CI.
      packages = lib.genAttrs extensions (name:
        build-haskell-package name inputs.${name}
      );
    };
}
