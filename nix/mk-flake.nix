# Replicate common.lib.mkFlake without flake inputs.
#
# common.lib.mkFlake is a thin wrapper around flake-parts.lib.mkFlake that
# imports default modules (haskell-flake, treefmt, pre-commit, mission-control,
# process-compose, flake-root). We import these modules directly from fetched sources.
sources:
let
  nixpkgs = import sources.nixpkgs { system = "x86_64-linux"; };
  flake-parts-lib = import (sources.flake-parts + "/lib.nix") { inherit (nixpkgs) lib; };

  # Fake `common` attrset for common's treefmt.nix which takes `common:` as first arg
  # and references common.inputs.treefmt-nix.flakeModule, nixpkgs-21_11.legacyPackages,
  # and nixpkgs-140774-workaround.patch.
  workaroundFlake = (import (sources.nixpkgs-140774-workaround + "/flake.nix")).outputs { self = {}; };
  fakeCommon = {
    inputs = {
      treefmt-nix.flakeModule = sources.treefmt-nix + "/flake-module.nix";
      nixpkgs-140774-workaround = workaroundFlake;
      nixpkgs-21_11 = {
        legacyPackages = builtins.listToAttrs (map (system: {
          name = system;
          value = import sources.nixpkgs-21_11 {
            inherit system;
            config.permittedInsecurePackages = [ "openssl-1.1.1w" ];
          };
        }) (import sources.systems));
      };
      nixpkgs-latest = {
        legacyPackages = builtins.listToAttrs (map (system: {
          name = system;
          value = import sources.nixpkgs-latest { inherit system; };
        }) (import sources.systems));
      };
    };
  };
in
{
  # Equivalent to common.lib.mkFlake { inherit inputs; } mod
  mkFlake = { self }: mod:
    flake-parts-lib.mkFlake { inputs = { inherit self; }; } {
      systems = import sources.systems;
      imports = [
        # Core modules (from common.flakeModules.default)
        (sources.flake-root + "/flake-module.nix")
        (sources.haskell-flake + "/nix/modules")
        (import (sources.common + "/nix/treefmt.nix") fakeCommon)
        (sources.common + "/nix/haskell")
        (sources.common + "/nix/ghc927.nix")
        (sources.common + "/nix/pre-commit.nix")
        (sources.common + "/nix/arion.nix")
        (sources.mission-control + "/nix/flake-module.nix")
        (sources.process-compose-flake + "/nix/flake-module.nix")
        (sources.pre-commit-hooks-nix + "/flake-module.nix")

        # The user module
        mod
      ];
      # Replicate common's pkgs setup (from common/flake-module.nix)
      perSystem = { system, ... }: {
        _module.args.pkgs = import sources.nixpkgs {
          inherit system;
          config.allowUnfree = true;
          config.allowBroken = true;
          config.permittedInsecurePackages = [ "nodejs-16.20.2" "openssl-1.1.1w" ];
        };
      };
    };
}
