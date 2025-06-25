{
  perSystem = { pkgs, lib, config, ... }: {
    packages.joinNammayatriMetadata = pkgs.symlinkJoin {
      name = "joinNammayatriMetadata";
      paths = lib.pipe config.haskellProjects.default.packages [
        (lib.mapAttrs (n: _: config.haskellProjects.default.outputs.finalPackages."${n}"))
        (lib.filterAttrs (_: v: v?nammayatriMetadata))
        (lib.mapAttrs (_: v: v.nammayatriMetadata))
        lib.attrValues
      ];
      postBuild = ''
        # Create symlinks for each package's nammayatriMetadata
        for pkg in $paths; do
          if [ -d "$pkg" ]; then
            pkg_name=$(basename "$pkg")
            ln -s "$pkg" "$out/$pkg_name"
          fi
        done
      '';
      meta.description = "Join the `nammayatriMetadata` output of packages defined in `haskellProjects.default.packages` (An option of `haskell-flake`)";
    };
  };
}

