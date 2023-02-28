{ self, inputs, ... }:

{
  config = {
    perSystem = { self', config, pkgs, lib, system, ... }: {
      treefmt.config = {
        inherit (config.flake-root) projectRootFile;
        package = pkgs.treefmt;

        programs.nixpkgs-fmt.enable = true;
        # FIXME: Disabled until https://github.com/nammayatri/nammayatri/issues/31
        # programs.hlint.enable = true;
        programs.ormolu.enable = true;

        programs.ormolu.package =
          let pkgs-21_11 = inputs.nixpkgs-21_11.legacyPackages.${system};
          in inputs.nixpkgs-140774-workaround.patch pkgs-21_11 pkgs-21_11.haskellPackages.ormolu;
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
