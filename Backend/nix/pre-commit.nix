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

    hotfix-branch-validation = {
      enable = true;
      name = "hotfix-branch-validation";
      description = "Validate prodHotPush branch changes don't cross BAP/BPP boundaries";
      types = [ "file" ];
      pass_filenames = true;
      files = "Backend/.*$";
      entry = lib.getExe (pkgs.writeShellApplication {
        name = "hotfix-branch-validation";
        runtimeInputs = [ pkgs.git ];
        text = ''
          BRANCH=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "")

          if [[ "$BRANCH" == "prodHotPush-BAP" ]]; then
            FORBIDDEN=""
            for file in "''$@"; do
              if [[ "$file" == Backend/app/provider-platform/* ]]; then
                FORBIDDEN="''${FORBIDDEN}"$'\n'"  $file"
              fi
            done
            if [[ -n "$FORBIDDEN" ]]; then
              echo "ERROR: prodHotPush-BAP branch must NOT contain changes in Backend/app/provider-platform/."
              echo "The following files are not allowed:''${FORBIDDEN}"
              exit 1
            fi
          fi

          if [[ "$BRANCH" == "prodHotPush-BPP" ]]; then
            FORBIDDEN=""
            for file in "''$@"; do
              if [[ "$file" == Backend/app/rider-platform/* ]]; then
                FORBIDDEN="''${FORBIDDEN}"$'\n'"  $file"
              fi
            done
            if [[ -n "$FORBIDDEN" ]]; then
              echo "ERROR: prodHotPush-BPP branch must NOT contain changes in Backend/app/rider-platform/."
              echo "The following files are not allowed:''${FORBIDDEN}"
              exit 1
            fi
          fi
        '';
      });
    };
  };
}
