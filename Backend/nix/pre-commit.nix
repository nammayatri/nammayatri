# https://pre-commit.com/ hooks defined in Nix
# cf. https://github.com/cachix/pre-commit-hooks.nix
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

    yaml-constraint-tags = {
      enable = true;
      name = "yaml-constraint-tags";
      description = "Reject unquoted YAML tags (e.g. !SecondaryKey) in Storage spec constraints — they are silently dropped by the NammaDSL parser";
      types = [ "file" ];
      pass_filenames = true;
      files = "Backend/.*/spec/Storage/.*\\.yaml$";
      entry = lib.getExe (pkgs.writeShellApplication {
        name = "yaml-constraint-tags";
        text = ''
          fail=0
          for f in "''$@"; do
            if grep -nE '^[[:space:]]+[a-zA-Z_][a-zA-Z0-9_]*:[[:space:]]+![A-Za-z]+[[:space:]]*$' "''$f"; then
              echo "ERROR: ''$f contains unquoted YAML tag(s) in constraints (shown above)."
              echo "       Wrap the value in quotes, e.g. fieldName: \"!SecondaryKey\""
              echo "       Unquoted !Tag values are parsed as YAML tags and silently dropped by the NammaDSL parser,"
              echo "       producing an empty secondary-key list in the generated Beam file."
              fail=1
            fi
          done
          exit "''$fail"
        '';
      });
    };
  };
}
