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
      # Run the hook ONCE per commit, not per-file. The check walks the whole
      # ddl/feature-migrations trees regardless, so passing the staged file
      # list is meaningless and forking 1500× causes macOS to run out of
      # processes (`find: fork: Resource temporarily unavailable`), leading
      # to a half-walked tree and false-positive "overlapping" reports.
      pass_filenames = false;
      files = "Backend/dev/(ddl|feature)-migrations/.*\\.sql$";
      entry = lib.getExe (pkgs.writeShellApplication {
        name = "overlapping-migrations";
        text = ''
          {
            echo "Checking for overlapping migrations"
            check () {
               # Only consider .sql files at the top level of $1 (don't recurse — feature-migrations
               # is a flat dir, ddl-migrations subdirs are flat per-service).
               if [ "$(find "''$1" -maxdepth 1 -type f -name '*.sql' -exec basename {} \; | cut -c-4 | sort | uniq | wc -l)" != "$(find "''$1" -maxdepth 1 -type f -name '*.sql' | wc -l)" ]; then
                echo "There are overlapping migration indices in ''${1}."
                find "''$1" -maxdepth 1 -type f -name '*.sql' -exec basename {} \; | sort | awk '{p=substr($0,1,4); if (p==prev) print prev_full, "<-> ", $0; prev=p; prev_full=$0}'
                exit 2
              fi
            }
            # Per-service ddl-migrations subdirs (each subdir is independent).
            # Read into an array so the loop body runs in the parent shell —
            # so `exit 2` inside check() actually fails the hook (not just a
            # subshell). Using mapfile avoids shellcheck SC2044 (find in for).
            mapfile -t DIRS < <(find Backend/dev/ddl-migrations -mindepth 1 -maxdepth 1 -type d)
            for DIR in "''${DIRS[@]}"; do
              check "''$DIR"
            done
            # feature-migrations is a single flat directory.
            if [ -d Backend/dev/feature-migrations ]; then
              check Backend/dev/feature-migrations
            fi
          }
        '';
      });
    };

    ddl-migrations-no-dml = {
      enable = true;
      name = "ddl-migrations-no-dml";
      description = "Reject DML (INSERT/UPDATE/DELETE) in Backend/dev/ddl-migrations/ — that directory must hold pure DDL only. Move DML to dev/feature-migrations/ or dev/local-testing-data/.";
      types = [ "file" ];
      pass_filenames = false;
      files = "Backend/dev/ddl-migrations/.*\\.sql$";
      entry = lib.getExe (pkgs.writeShellApplication {
        name = "ddl-migrations-no-dml";
        text = ''
          fail=0
          # Pattern: INSERT INTO …, DELETE FROM …, UPDATE <ident>… at the
          # start of a line (after optional whitespace). Lines that begin
          # with `--` (comments) cannot match because the comment marker
          # comes before the keyword, so commented-out DML is skipped
          # automatically. Inside DO $$…$$ blocks the indented INSERT/UPDATE
          # lines still match, which is intentional — we want to catch
          # those too.
          # Allowlist: DML targeting `system_configs` is permitted because that
          # table holds NammaDSL kv_configs / runtime config that is schema-
          # coupled to the migration that adds it (and not in scope of
          # config-sync). Any DML matching the regex below that ALSO mentions
          # system_configs in the same line is filtered out before failing.
          mapfile -t FILES < <(find Backend/dev/ddl-migrations -type f -name '*.sql')
          for f in "''${FILES[@]}"; do
            hits=$(
              grep -nE '^[[:space:]]*(INSERT[[:space:]]+INTO[[:space:]]+|DELETE[[:space:]]+FROM[[:space:]]+|UPDATE[[:space:]]+[a-zA-Z_])' "''$f" \
                | grep -vE 'system_configs?\b' \
                || true
            )
            if [ -n "''$hits" ]; then
              echo "ERROR: DML in ''$f (Backend/dev/ddl-migrations/ must be pure DDL)"
              # Indent each hit line with 4 spaces using bash parameter
              # expansion (shellcheck SC2001 — avoid `sed` for substitution).
              echo "    ''${hits//$'\n'/$'\n    '}"
              fail=1
            fi
          done
          if [ "''$fail" -ne 0 ]; then
            echo
            echo "Move INSERT/UPDATE/DELETE statements out of dev/ddl-migrations/."
            echo "  • Schema-coupled bootstrap data → dev/seed-migrations/<svc>/"
            echo "  • Local-only test seed         → dev/local-testing-data/<svc>.sql"
            echo "  • Feature toggles / config     → dev/feature-migrations/"
            exit 1
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
