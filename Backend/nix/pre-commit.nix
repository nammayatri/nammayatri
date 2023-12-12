# https://pre-commit.com/ hooks defined in Nix
# cf. https://github.com/cachix/pre-commit-hooks.nix
{ self', ... }:
{ pkgs, lib, ... }:

{
  hooks = {
    trailing-ws.files = "Backend/.*$";

    overlapping-migrations = {
      enable = true;
      name = "overlapping-migrations";
      description = "Check for overlapping migration indices";
      types = [ "file" ];
      pass_filenames = true;
      files = "Backend/.*\\.sql$";
      entry = lib.getExe (pkgs.writeShellApplication {
        name = "overlapping-migrations";
        text = ''
          if [[ ''$# -gt 0 ]]; then
            echo "Checking for overlapping migrations"
            check () {
               if [ "$(find "''$1" -type f -exec basename {} \; | cut -c-4 | sort | uniq | wc -l)" != "$(find "''$1" -type f -exec basename {} \; | wc -l)" ]; then
                echo "There are overlapping migration indices in ''${1}."
                exit 2
              fi
            }
            DIRS=$(find Backend/dev/migrations -type d -not -path Backend/dev/migrations)
            for DIR in $DIRS
            do
              check "''$DIR"
            done
          fi
        '';
      });
    };
    check-src-read-only-diff-check = {
      enable = true;
      name = "check-src-read-only-diff-check";
      description = "Check that src-read-only folder is read-only";
      types = [ "file" ];
      pass_filenames = true;
      files = ".*\\/((rider-platform\\/rider-app)|(provider-platform\\/dynamic-offer-driver-app))\\/Main\\/src-read-only\\/.*\\.hs$";
      entry = lib.getExe self'.packages.alchemist;
    };

  };
}
