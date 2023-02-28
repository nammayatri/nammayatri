{ self, inputs, ... }:

{
  config = {
    perSystem = { self', config, pkgs, lib, ... }: {
      treefmt.config = {
        inherit (config.flake-root) projectRootFile;
        package = pkgs.treefmt;

        programs.nixpkgs-fmt.enable = true;
        # programs.hlint.enable = true;
        programs.ormolu.enable = true;

        programs.ormolu.package = (inputs.nixpkgs-140774-workaround.packages { inherit pkgs; }).ormolu;
        settings.formatter.ormolu = {
          options = [
            "--ghc-opt"
            "-XTypeApplications"
            "--ghc-opt"
            "-fplugin=RecordDotPreprocessor"
          ];
        };
      };
    };
  };
}
