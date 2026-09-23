#!/usr/bin/env bash
#
# Tell the person who validates that somebody is waiting.
#
# ── Why this exists ─────────────────────────────────────────────────────────
# Drivers enrol themselves (see the auth guard's DRIVER_SIGNUP), and the
# console shows them the moment they do — but a page gives nobody a reason to
# open it. Audited on 2026-09-23: the queue held two registrations, six and two
# days old, and the last validation decision on the whole system was three
# weeks before that. Nothing was broken; nobody had been told.
#
# A driver who signs up and hears nothing for a day has already formed his
# opinion of us, so this closes the gap the week self-registration meets real
# people.
#
# ── Why Telegram, of all things ─────────────────────────────────────────────
# The obvious channel cannot work: **Moorsyl only delivers to +222**, and the
# person validating has an Algerian number. That is the same constraint the
# guard's SMS_BYPASS list exists for. There is no SMTP on this box either.
# Telegram needs one HTTP call, costs nothing, reaches any country, and the
# credential is revocable from a phone.
#
# ── What it will not do ─────────────────────────────────────────────────────
# It never announces the same driver twice. The ids it has already sent live in
# STATE, so a driver is announced once when he appears and once more if he is
# still waiting after PATIENCE_HOURS — and then left alone. A notifier that
# repeats itself is one people mute, and a muted notifier is worse than none
# because everyone believes it is working.
#
# Inert until configured: with no token it exits 0 quietly, so it can be
# installed and enabled before the bot exists.
#
#   TELEGRAM_BOT_TOKEN=...  from @BotFather
#   TELEGRAM_CHAT_ID=...    your own chat with the bot
#
# Both live in /opt/ny/local-stack/.env, which is not in git and is in the
# backup set.
set -uo pipefail

cd "$(dirname "$0")" || exit 1
[ -f .env ] && set -a && . ./.env && set +a

TOKEN="${TELEGRAM_BOT_TOKEN:-}"
CHAT="${TELEGRAM_CHAT_ID:-}"
STATE="${REGISTRATION_STATE:-/opt/ny/local-stack/registration-notify.state}"
PATIENCE_HOURS="${PATIENCE_HOURS:-4}"
CONSOLE="${ADMIN_CONSOLE_URL:-https://admin.movinapp.net}"

if [ -z "$TOKEN" ] || [ -z "$CHAT" ]; then
  # Not an error: the unit is installed before the bot is made.
  echo "registration-notify: no TELEGRAM_BOT_TOKEN/TELEGRAM_CHAT_ID, nothing to do"
  exit 0
fi

touch "$STATE" 2>/dev/null || { echo "cannot write $STATE"; exit 1; }

send() {
  # --data-urlencode so an apostrophe in a name cannot break the request.
  curl -s -m 15 -o /dev/null -w '%{http_code}' \
    -X POST "https://api.telegram.org/bot${TOKEN}/sendMessage" \
    --data-urlencode "chat_id=${CHAT}" \
    --data-urlencode "text=$1" \
    --data-urlencode "disable_web_page_preview=true"
}

# One row per pending driver. Tab-separated, and `psql -At` so there is no
# header or padding to strip. The fields are exactly what somebody needs in
# order to decide whether to open the console *now*.
ROWS=$(docker exec ny-postgres psql -U postgres -d atlas_dev -At -F$'\t' -c "
  SELECT p.id,
         coalesce(nullif(trim(coalesce(p.first_name,'') || ' ' || coalesce(p.last_name,'')), ''),
                  'Nom non renseigné'),
         p.unencrypted_mobile_number,
         round(extract(epoch from (now() - p.created_at)) / 3600)::int,
         (SELECT count(*) FROM movin.driver_document d WHERE d.driver_id = p.id),
         (SELECT count(*) FROM movin.driver_declaration dd WHERE dd.driver_id = p.id)
    FROM atlas_driver_offer_bpp.person p
    JOIN atlas_driver_offer_bpp.driver_information di ON di.driver_id = p.id
   WHERE p.merchant_id = 'favorit0-0000-0000-0000-00000favorit'
     AND NOT di.enabled AND NOT di.blocked
   ORDER BY p.created_at;" 2>/dev/null)

# An empty queue is the normal case and still has work to do: the state file
# has to forget whoever has just been validated, or it grows for ever. Exiting
# here was a real bug, caught by tests/registration-notify.test.sh.
if [ -z "$ROWS" ]; then
  : > "$STATE"
  exit 0
fi

while IFS=$'\t' read -r ID NAME NUMBER HOURS DOCS DECL; do
  [ -z "${ID:-}" ] && continue

  # Which announcement is due for this driver, if any.
  if ! grep -q "^${ID} new$" "$STATE" 2>/dev/null; then
    STAGE="new"
  elif [ "${HOURS:-0}" -ge "$PATIENCE_HOURS" ] && ! grep -q "^${ID} waited$" "$STATE" 2>/dev/null; then
    STAGE="waited"
  else
    continue
  fi

  PAPERS="aucun papier reçu"
  [ "${DOCS:-0}" -gt 0 ] && PAPERS="${DOCS} papier(s) reçu(s)"
  [ "${DECL:-0}" -gt 0 ] && PAPERS="${PAPERS}, véhicule déclaré"

  if [ "$STAGE" = "new" ]; then
    HEAD="Movin · nouvelle inscription chauffeur"
    WHEN="À l'instant"
  else
    HEAD="Movin · chauffeur toujours en attente"
    WHEN="En attente depuis ${HOURS} h"
  fi

  CODE=$(send "${HEAD}

${NAME}
${NUMBER}
${PAPERS}
${WHEN}

${CONSOLE}")

  if [ "$CODE" = "200" ]; then
    echo "${ID} ${STAGE}" >> "$STATE"
    echo "registration-notify: told about ${ID} (${STAGE})"
  else
    # Left out of STATE deliberately, so the next run tries again rather than
    # silently deciding this driver was already announced.
    echo "registration-notify: Telegram refused for ${ID} (HTTP ${CODE})" >&2
  fi
done <<< "$ROWS"

# Forget drivers who have left the queue, so the file cannot grow for ever and
# a number re-registering months later is announced properly.
if [ -s "$STATE" ]; then
  LIVE=$(printf '%s\n' "$ROWS" | cut -f1)
  awk -v live="$LIVE" 'BEGIN { split(live, a, "\n"); for (i in a) keep[a[i]] = 1 }
                       $1 in keep { print }' "$STATE" > "${STATE}.tmp" \
    && mv "${STATE}.tmp" "$STATE"
fi
