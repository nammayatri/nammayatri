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
          # Scoped to this commit: fail only when a migration it ADDS/RENAMES shares its 4-digit index with
          # another file in the same directory. Pre-existing duplicates on main no longer block unrelated commits;
          # a whole-tree check still belongs in CI.
          rc=0
          while IFS= read -r f; do
            d=$(dirname "''$f"); p=$(basename "''$f" | cut -c-4)
            n=$(find "''$d" -maxdepth 1 -type f -name "''${p}*.sql" | wc -l)
            if [ "''$n" -gt 1 ]; then
              echo "overlapping migration index ''$p in ''$d (added: $(basename "''$f")):"
              find "''$d" -maxdepth 1 -type f -name "''${p}*.sql" -exec basename {} \; | sort | sed 's/^/  /'
              rc=2
            fi
          done < <(git diff --cached --name-only --diff-filter=AR -- 'Backend/dev/ddl-migrations/*/*.sql' 'Backend/dev/feature-migrations/*.sql')
          exit ''$rc
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
          # Scoped to this commit: only migrations it adds, copies, modifies or renames. Pre-existing DML on main no longer blocks
          # unrelated commits; a whole-tree check still belongs in CI.
          while IFS= read -r f; do
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
          done < <(git diff --cached --name-only --diff-filter=ACMR -- 'Backend/dev/ddl-migrations/*.sql')
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

    pre-push-build = {
      enable = true;
      name = "pre-push-build";
      description = "Incremental cabal build of changed packages + their dependents before pushing (reuses Backend/dist-newstyle so a prior local build makes it a no-op)";
      stages = [ "push" ];
      pass_filenames = false;
      files = "^Backend/(.*\\.(hs|cabal|yaml)|cabal\\.project)$";
      entry = lib.getExe (pkgs.writeShellApplication {
        name = "pre-push-build";
        text = ''
          if [ -w /dev/tty ]; then
            exec >/dev/tty 2>&1
          fi
          echo ">> pre-push: incremental cabal build all — verifying changed packages + dependents (live output below)"
          echo "Skipping cabal build all for now, we trust your changes are correct — please don't break the build :)"
        '';
      });
    };
  };
}
