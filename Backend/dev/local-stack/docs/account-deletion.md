# Account deletion

How a person asks for their account to be deleted, and why nothing here deletes anything.

> Moved here from the local-stack README on 2026-10-08 (phase 7), word for word: dates and measurements are as they were taken. Commands written `./x.sh` run from `stack/`. Back to the [README](../README.md).

## Account deletion — `account-deletion.sql`, `maps-shim/deletion.js`

Google Play rejects an app that lets people create an account and gives them no
way inside it to ask for that account to be deleted. A web page is not enough;
it wants both, and it checks every kind of account the app offers.

**The deployed backend cannot delete an account.** Not "does it badly" — the
route does not exist. Checked *inside the container*, against both running
binaries: `deleteAccount`, `deleteProfile` and `account/delete` all return zero.
So the request is **recorded** and a person carries it out from the admin site,
which is what the client asked for. Every screen in the app says *demande
enregistrée*, never *compte supprimé*, and `verify-apk.py` fails a build that
contains the latter — the account still signs in while that screen is on the
display, and saying otherwise would be a lie we chose rather than a bug we
missed.

    docker cp account-deletion.sql ny-postgres:/tmp/
    docker exec ny-postgres psql -U postgres -d atlas_dev -f /tmp/account-deletion.sql
    scp maps-shim/deletion.js maps-shim/server.js ny:/opt/ny/local-stack/maps-shim/
    ssh ny 'cd /opt/ny/local-stack && docker compose restart maps-shim'

One route, three methods, and it takes **no id at all**:

    GET    /account/deletion-request   what screen 21 draws when it opens
    POST   /account/deletion-request   record it
    DELETE /account/deletion-request   withdraw it

The caller sends a token and nothing else; the shim asks the backend whose it
is. There is no request shape here that could delete somebody else's account,
because there is nowhere to put their id. Same rule as the wallet and the
avatar fix.

**Three decisions worth keeping.**

*The in-ride check is written backwards.* It asks whether a ride is **not**
`COMPLETED` or `CANCELLED`, rather than listing the in-flight statuses. Only
terminal statuses exist in our data, so enumerating the active ones is
guesswork — and a guess that misses one produces a check that never fires,
which is worse than no check because it looks like one. Written this way an
unknown status refuses the deletion, which is the safe direction. A check that
throws returns `true` as well, rather than silently allowing what it guards.

*One open request per account is a partial unique index*, so two taps on a slow
connection cannot both insert and the app's "already requested" state is a fact
rather than a race.

*A withdrawal marks the row `withdrawn`, it does not delete it.* The office
should be able to see that somebody asked and changed their mind.

`movin.deletion_queue` is the view the admin site reads: the open requests,
oldest first, with `days_left` and an `overdue` flag already computed.

nginx gets an **exact-path** location rather than an `/account/` prefix, on the
`auth` rate-limit zone. Nobody does this dozens of times a minute, and a flood
here would be somebody guessing a token rather than an app behaving badly.

Proved end to end by `./probe-account-deletion.py`, 17/17 on 2026-08-30: none →
request → pending → 409 on a second → withdraw → none → and she can ask again,
with the row kept as history and the date not moving between reads.
