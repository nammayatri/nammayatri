{ pkgs, lib, ... }:

{
  hooks = {
    trailing-ws = {
      enable = true;
      name = "trailing-ws";
      description = "Remove trailing spaces";
      types = [ "file" ];
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
  };
}
