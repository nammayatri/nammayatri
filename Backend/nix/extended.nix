# Nammayatri extended
{ self, inputs, ... }:

let
  extensions = [
    "ny-example"
  ];
in
{
  flake.flakeModules.extended-nammayatri = name: { ... }: {
    systems = import inputs.systems;
    imports = [
      inputs.common.flakeModules.default
    ];

    perSystem = { self', ... }: {
      # Create a Haskell project that uses nammayatri as a dependency.
      haskellProjects.default = {
        imports = [
          self.haskellFlakeProjectModules.output
        ];
      };
      packages.default = self'.packages.${name};
    };
  };

  perSystem = { config, lib, ... }: {
    packages = lib.genAttrs extensions (name:
      with config.haskellProjects.default.outputs.finalPackages;
      callCabal2nix name inputs.${name} { }
    );
  };
}
