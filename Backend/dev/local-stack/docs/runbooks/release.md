# Runbook — release a change to the server

The whole procedure, start to finish. What the release script does inside is
in [releasing.md](../releasing.md); why it works this way is in its header
(`ops/release-remote.py`). Run everything from the laptop, in WSL, anywhere in
the repository.

You need SSH access to the server as root under the alias `ny` (an entry in
`~/.ssh/config`; `HOST=` overrides it). Access is the owner's to grant, and the
address is not written in this public repository. The firewall refuses a sixth
connection within 30 seconds, so do not loop `ssh` while a release runs.

## 1. Before — the change is ready

1. Everything the server runs is under `Backend/dev/local-stack/stack/`. A
   change anywhere else is not released by this.
2. Run the tests — the same set CI runs:

       cd Backend/dev/local-stack
       (cd tests && npm ci) && bash tests/run-all.sh

   A shim change that is **meant** to alter an answer re-records its golden
   file in the same commit (`node tests/maps-shim-routes.test.js --record`, or
   the auth-guard one) and says so in the message. A refactor must pass them
   untouched.
3. Commit, then `git push`. The release refuses a dirty tree or an unpushed
   commit — and an untracked file counts. Park the next change *outside* the
   repository, not just unstaged: a new `db/*.sql` is applied by whichever
   release carries it.
4. Wait for both workflows on that commit to be green: `algeria: node tests`
   and `algeria: ride regression` ([testing.md](../testing.md)).

## 2. Look before writing

    Backend/dev/local-stack/ops/deploy.sh --dry-run

It prints the files that are new, changed and to be removed, the SQL it would
apply, and what it would restart. Nothing is written. Read it against what you
meant to change: an unexpected file or restart is a reason to stop.

Who notices what:

| Restart | Who notices |
|---|---|
| `ny-maps-shim` | a few seconds: routes, place search, wallet screens fail once |
| `ny-auth-guard` | everyone mid-sign-in loses their code; the SMS budget in memory resets |
| `reload ny-edge` | nobody (nginx reloads in place; a failed `nginx -t` aborts) |
| a systemd unit (`movin-fleet`, `movin-bot`) | the simulated cars or the bot, briefly |
| SQL | depends on the file — read it |

Get the owner's OK for **this** release, with the dry run's summary and the
time. One OK covers one release.

## 3. Release

    Backend/dev/local-stack/ops/deploy.sh

It saves what it replaces in `/opt/ny/local-stack.prev`, writes the files in
place, applies new SQL, restarts what changed, runs its checks (containers,
both healthz, `nginx -t`, every hash), writes `.shipped`, and then verifies the
server from outside against the commit, from git.

**Read the last lines.** `released` and `all N files … byte for byte the
commit` mean done. A `BAD` line means it did not finish cleanly — go to the
[rollback runbook](rollback.md) before anything else.

If a file was edited on the server by hand since the last release, it stops and
names it. Bring that edit into git and release again; `--force` overwrites it.

## 4. After — prove it, from outside

1. The release ran `verify`. To repeat it later: `ops/deploy.sh verify`.
2. The service, not just the files — on the server:

       curl -s http://127.0.0.1:8030/healthz     # maps-shim: payments per country, push
       curl -s http://127.0.0.1:8031/healthz     # auth-guard: channels, SMS

   and the routes the change touched, in **both countries**.
3. A whole ride in each country, while the simulated fleet is installed:

       ssh ny "python3 - both" < Backend/dev/local-stack/ops/checks/probe-two-country-rides.py

   It signs in the two test passengers, books, rides with a simulated car to
   COMPLETED, and checks the wallet charge. Both lines must say PASS.
4. Record the release in the phase record or the launch log: commit, time,
   what restarted, what was checked.

## If Claude Code's safety check blocks the release

The auto-mode classifier has refused `deploy.sh` when a release removes files on
the server. Do not work around it: the owner runs the same command in a
terminal, and the checks in step 4 follow as usual.
