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

    yaml-validation = {
      enable = true;
      name = "dsl-yaml";
      description = "Validate and format DSL YAML files";
      types = [ "file" ];
      pass_filenames = true;
      files = "Backend/.*/spec/.*\\.ya?ml$";
      entry = lib.getExe (pkgs.writeShellApplication {
        name = "yaml-validation";
        text = ''
          if [[ ''$# -gt 0 ]]; then
            echo "Validate and format DSL YAML files"
            for FILE in "''$@"; do
              yamlfmt -formatter retain_line_breaks=true,trim_trailing_whitespace=true "$FILE" || exit 1
            done
          fi
        '';
      });
    };
  };
}
