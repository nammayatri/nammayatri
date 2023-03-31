# https://pre-commit.com/ defined in Nix, via https://github.com/cachix/pre-commit-hooks.nix
{ pkgs, lib, ... }:

{
  hooks = {
    trailing-ws = {
      enable = true;
      name = "trailing-ws";
      description = "Remove trailing spaces";
      types = [ "file" ];
      files = "Backend/.*$";
      pass_filenames = true;
      entry = lib.getExe (pkgs.writeShellApplication {
        name = "trailing-ws";
        text = ''
          if [[ ''$# -gt 0 ]]; then
            echo "Checking for trailing spaces in ''$# files"
            FILES_WITH_TRAILING_WS=$(grep -r -l '  *$' "''$@" || true)
            if [ -z "$FILES_WITH_TRAILING_WS" ]; then
              echo "No trailing spaces found"
            else
              echo "Trailing spaces found in:"
              echo "''$FILES_WITH_TRAILING_WS"

              echo "Removing trailing spaces, please check and git add the changes"
              if [[ "''$OSTYPE" == 'darwin'* ]]; then
                BACKUP_EXTENSION=pqmc98hxvymotiyb4rz34
                sed -i".''${BACKUP_EXTENSION}" -e 's/  *$//' "''$FILES_WITH_TRAILING_WS"
                find ./ -name "*.''${BACKUP_EXTENSION}" -exec rm {} +
              else
                sed -i -e 's/  *$//' "''$FILES_WITH_TRAILING_WS"
              fi
            fi
          fi
        '';
      });
    };

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
  };
}
