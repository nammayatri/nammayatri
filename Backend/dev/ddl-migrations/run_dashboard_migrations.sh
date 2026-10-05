#!/usr/bin/env bash
# Apply the unified dashboard (atlas_dashboard) migrations, connecting as the
# schema owner atlas_dashboard_user — the same way the retired
# provider-dashboard-exe applied them at startup.
#
# These dirs are deliberately NOT in any app server's migrationPath: the
# in-app migration runner keys schema_migrations by file basename, so a
# dashboard file sharing a name with an app file (e.g.
# fleet_member_association.sql, which exists for both atlas_driver_offer_bpp
# and atlas_dashboard) makes the runner report a checksum mismatch and abort
# the server. Running them here, idempotently, avoids that entirely.
#
# Idempotency: psql runs with ON_ERROR_STOP=0, so statements that were already
# applied (CREATE TABLE / ADD COLUMN on existing objects) error and are
# skipped, while newly appended statements apply — the same model
# check_migrations.sh uses. Use check_migrations.sh for strict validation.
#
# Env overrides: DB_HOST (localhost), DB_PRIMARY_PORT (5434), DB_NAME
# (atlas_dev), DASHBOARD_DB_USER (atlas_dashboard_user), DASHBOARD_DB_PASSWORD
# (atlas).

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BACKEND_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

DB_HOST="${DB_HOST:-localhost}"
DB_PORT="${DB_PRIMARY_PORT:-5434}"
DB_NAME="${DB_NAME:-atlas_dev}"
DB_USER="${DASHBOARD_DB_USER:-atlas_dashboard_user}"
export PGPASSWORD="${DASHBOARD_DB_PASSWORD:-atlas}"

DIRS=(
  "$BACKEND_DIR/dev/ddl-migrations/dashboard"
  "$BACKEND_DIR/dev/seed-migrations/dashboard"
  "$BACKEND_DIR/dev/migrations-read-only/dashboard"
)

# Fail loudly if we cannot reach the database at all.
if ! psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" -c "SELECT 1" >/dev/null 2>&1; then
  echo "ERROR: cannot connect to $DB_NAME at $DB_HOST:$DB_PORT as $DB_USER" >&2
  exit 1
fi

for dir in "${DIRS[@]}"; do
  [ -d "$dir" ] || { echo "skip (missing): $dir"; continue; }
  while IFS= read -r f; do
    if psql -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" -d "$DB_NAME" \
        -v ON_ERROR_STOP=0 -q -f "$f" 2>&1 | grep -q "ERROR"; then
      echo "Applied (with skipped statements): ${f#"$BACKEND_DIR"/}"
    else
      echo "Applied: ${f#"$BACKEND_DIR"/}"
    fi
  done < <(ls "$dir"/*.sql 2>/dev/null | sort)
done

echo "dashboard migrations: done"
