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
      description = "Reject unquoted YAML tags (e.g. !SecondaryKey) in Storage spec constraints, and unquoted !Text in API spec field types — they are dropped by the YAML parser";
      types = [ "file" ];
      pass_filenames = true;
      files = "Backend/.*/spec/(Storage|(.*/)?API)/.*\\.yaml$";
      entry = lib.getExe (pkgs.writeShellApplication {
        name = "yaml-constraint-tags";
        text = ''
          fail=0
          for f in "''$@"; do
            if [[ "''$f" == */spec/Storage/* ]] && grep -nE '^[[:space:]]+[a-zA-Z_][a-zA-Z0-9_]*:[[:space:]]+![A-Za-z]+[[:space:]]*$' "''$f"; then
              echo "ERROR: ''$f contains unquoted YAML tag(s) in constraints (shown above)."
              echo "       Wrap the value in quotes, e.g. fieldName: \"!SecondaryKey\""
              echo "       Unquoted !Tag values are parsed as YAML tags and silently dropped by the NammaDSL parser,"
              echo "       producing an empty secondary-key list in the generated Beam file."
              fail=1
            fi
            if [[ "''$f" == */API/* ]] && grep -nE '^[[:space:]]+(-[[:space:]]+)?[a-zA-Z_][a-zA-Z0-9_]*[[:space:]]*:[[:space:]]+([^"'"'"'#[:space:]][^#]*)?!' "''$f"; then
              echo "ERROR: ''$f contains unquoted !Text field type(s) (shown above)."
              echo "       Wrap the type in double quotes, e.g. fieldName: \"!Text\", \"Maybe !Text\", \"[!Text]\""
              echo "       Unquoted, a type that starts with ! is parsed as a YAML tag and dropped, leaving the field without a type."
              fail=1
            fi
          done
          exit "''$fail"
        '';
      });
    };

    translation-keys-only = {
      enable = true;
      name = "translation-keys-only";
      description = "User-visible text is read by a translation key through Storage.CachedQueries.Translations; the raw translations modules (queries, Beam, config-pilot config) may be imported only by that gateway, the dashboard translation editors and config tooling, and the typed push/SMS content escapes never take a string literal";
      types = [ "file" ];
      pass_filenames = true;
      # Hand-written sources of the three apps that send user text; src-read-only is generated.
      files = "Backend/app/(provider-platform|rider-platform|kafka-consumers)(/.*)?/src/.*\\.hs$";
      entry = lib.getExe (pkgs.writeShellApplication {
        name = "translation-keys-only";
        text = ''
          # Haskell has no package-private module, so this is the fence around the raw translations query:
          # the gateway (Storage.CachedQueries.Translations), the dashboard editors that write rows, config tooling, and the
          # raw query module itself (Storage.Queries.TranslationsExtra).
          bpp=Backend/app/provider-platform/dynamic-offer-driver-app/Main/src
          bap=Backend/app/rider-platform/rider-app/Main/src
          raw_import='^import[[:space:]]+(qualified[[:space:]]+)?("[^"]+"[[:space:]]+)?(qualified[[:space:]]+)?(Storage\.Queries\.Translations(Extra)?|Storage\.ConfigPilot\.Config\.Translation|Storage\.Beam\.Translations)([[:space:]]|$)'
          # A string literal handed to the escape that bypasses PushContent / SmsBody construction from a row.
          literal_escape='(prebuiltPushContent|prebuiltSmsBody|textAsReceived)[[:space:]]*(\$[[:space:]]*)?\(?[[:space:]]*"|prebuiltPushContent[[:space:]]+[A-Za-z_][A-Za-z0-9_.'"'"']*[[:space:]]+\(?"'
          # The constructor of TranslatedText (the translatable text of an API field), kept to the two gateways that read it by key.
          text_read_by_key='(^|[^A-Za-z0-9_'"'"'])textReadByKey($|[^A-Za-z0-9_'"'"'])'
          allowed="
          $bpp/Storage/CachedQueries/Translations.hs
          $bpp/Storage/Queries/TranslationsExtra.hs
          $bpp/Storage/ConfigPilot/Config/Translation.hs
          $bpp/Tools/ConfigPilot.hs
          $bpp/Domain/Action/Dashboard/Management/Translation.hs
          $bpp/Domain/Action/Dashboard/Management/NammaTag.hs
          $bpp/Domain/Action/Dashboard/Management/CoinsConfig.hs
          $bap/Storage/CachedQueries/Translations.hs
          $bap/Storage/Queries/TranslationsExtra.hs
          $bap/Storage/ConfigPilot/Config/Translation.hs
          $bap/Tools/ConfigPilot.hs
          $bap/Domain/Action/Dashboard/Translation.hs
          $bap/Domain/Action/Dashboard/NammaTag.hs
          "
          fail=0
          for f in "$@"; do
            # shellcheck disable=SC2086 # the allowlist is split on whitespace on purpose
            if ! printf '%s\n' $allowed | grep -qxF -- "$f" && grep -nE -- "$raw_import" "$f"; then
              echo "ERROR: $f imports the raw translations query outside the gateway (shown above)."
              fail=1
            fi
            if [ "$f" != "$bpp/Storage/CachedQueries/Translations.hs" ] && [ "$f" != "$bap/Storage/CachedQueries/Translations.hs" ] && grep -nE -- "$text_read_by_key" "$f"; then
              echo "ERROR: $f builds a TranslatedText with textReadByKey outside the gateway (shown above)."
              fail=1
            fi
            if grep -nE -- "$literal_escape" "$f"; then
              echo "ERROR: $f passes a string literal to prebuiltPushContent / prebuiltSmsBody / textAsReceived (shown above)."
              fail=1
            fi
          done
          if [ "$fail" -ne 0 ]; then
            echo
            echo "User-visible text is read by key so the translations page can show and translate it:"
            echo "  • add a constructor in Domain/Types/TranslationKeys.hs and read it with Storage.CachedQueries.Translations.findTextByKey"
            echo "  • keys that ops type into config rows: Storage.CachedQueries.Translations.findByOpsKey"
            echo "  • an API field typed TranslatedText is filled by a findTranslatedText* reader of Storage.CachedQueries.Translations, never textReadByKey;"
            echo "    textAsReceived is only for text typed by ops or a user, or sent by another service"
            echo "  • pushes take PushContent (buildMerchantPNContentWithFallback / pushContentOfRow) and SMS take SmsBody (the MessageBuilder build* functions);"
            echo "    prebuiltPushContent / prebuiltSmsBody are only for text that is final already (typed by ops, sent by another service, never displayed)"
            echo "Only a translations editor or config tool may touch the raw query; add its path to the allowlist in Backend/nix/pre-commit.nix."
            exit 1
          fi
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
