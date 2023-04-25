# TODO: Upstream this via https://github.com/srid/haskell-flake/issues/101#issuecomment-1526276493
{ pkgs, lib, config, ... }:

with pkgs.haskell.lib.compose;
let
  overrideLocalPackage = name: info:
    [
      # Haddocks are not used, so disable them to speed up builds.
      dontHaddock
      # Library profiling is enabled by default in nixpkgs. This makes
      # cabal compile the modules twice. Disable it to speed up nix
      # builds.
      disableLibraryProfiling
    ] ++ (lib.optionals (info.exes != { }) [
      # Separate about binaries from library assets, so the closure size
      # of `.#nammayatri` will be smaller.
      enableSeparateBinOutput
    ]);
  defaultOverrides =
    lib.mapAttrs overrideLocalPackage config.haskellProjects.default.outputs.packages;
in
defaultOverrides
