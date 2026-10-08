# Releasing — `ops/deploy.sh`

How the server is changed: one command, what it checks, what it restarts. The step-by-step versions are the runbooks in `runbooks/`.

> Moved here from the local-stack README on 2026-10-08 (phase 7), word for word: dates and measurements are as they were taken. Commands written `./x.sh` run from `stack/`. Back to the [README](../README.md).

## Releasing — `ops/deploy.sh`

Since 2026-10-06 (phase 3 of the backend restructuring plan) a release is one
command, run from the laptop in WSL, anywhere in the repository:

```bash
Backend/dev/local-stack/ops/deploy.sh --dry-run   # what would change; writes nothing
Backend/dev/local-stack/ops/deploy.sh             # release the commit you are on
Backend/dev/local-stack/ops/deploy.sh status      # what is deployed; any hand edits since?
Backend/dev/local-stack/ops/deploy.sh verify      # hash the server against the commit it claims
Backend/dev/local-stack/ops/deploy.sh rollback    # put back what the last release replaced
Backend/dev/local-stack/ops/deploy.sh tidy        # archive old .bak copies (root only)
```

On the server, `cat /opt/ny/local-stack/.shipped` answers *what is running*:
the commit, when, by whom, and whether the checks passed. `.shipped.files`
holds the sha256 of every file shipped.

What it does, and what it will not do:

- **Refuses** a dirty working tree or an unpushed commit, and takes `stack/`
  from the commit (`git archive`), never from the folder.
- **One scp and one ssh**, then one of each for `verify` — the firewall
  refuses the sixth SSH connection in 30 seconds.
- Writes **only files git ships**, **in place** (a bind-mounted file keeps its
  inode). Never `.env`, the certificates, `edge-web/`, `bin/` or the data.
- **Stops** if a file it would overwrite was edited by hand on the server since
  the last release, and names it. Bring the edit into git, or `--force`.
- Removes a file it no longer ships only if it is still exactly as shipped.
- Applies `db/*.sql` **only when its content is new to the server** — re-running
  a tariff would undo every fare changed since.
- **Restarts only what changed**: `auth-guard/` → that container, `maps-shim/` →
  that container, `edge/nginx.conf` → `nginx -t` then reload (a failure rolls
  back on the spot), `docker-compose.yml` → only the services whose resolved
  config changed, `simulate-driver.py` / `movin-bot.py` → their systemd unit,
  `systemd/` → installed into `/etc/systemd/system`, `daemon-reload`, timers
  enabled (the replaced units are kept for `rollback`). `backup.sh` needs
  nothing: the nightly unit runs the shipped file.
- **Checks**: every container that ran still runs, both healthz answer 200,
  `nginx -t`, every shipped file and installed unit matches its hash. Then
  writes `.shipped`.
- **Then verifies from outside** (`ops/deploy.sh verify`, also run on its own):
  the server only *measures* — the commit in `.shipped` and the sha256 of every
  file — and the laptop recomputes what they should be **from that commit in
  git** (`ops/release-verify.py`). The release's own check proves the copy;
  this proves the claim: a `.shipped` naming the wrong commit, or a hand edit
  hidden by rewriting `.shipped.files`, both fail it.
- Keeps the **image** it replaces (a maps-shim rebuild, a recreated service) as
  `<container>:previous`, by its tag — `docker tag ny-maps-shim:previous
  ny-maps-shim:local` and `docker compose up -d --no-deps maps-shim` is the
  instant way back (since phase 5).
- Keeps what it replaced or removed in **`/opt/ny/local-stack.prev`** — one
  named directory, instead of `.bak` files beside the live ones.

`tests/release.test.sh` rehearses all of it — a release, a refused hand edit,
the outside check (including a forged record), the units, a rollback, a tidy —
against a copy of the server's layout with docker and systemd stubbed:
`bash tests/release.test.sh` (37 checks).

**The website no longer writes our files.** Three of its scripts —
`deploy-console.sh`, `deploy-site.sh` and `ops/server/edge-gzip.sh` — used to
insert their nginx blocks, the custom 404, the gzip directives and the
`edge-web` mount into this stack's files. Since 2026-10-06 (website `d7f4374`,
`4fb9c7e`) they only check, and stop naming the file if something is missing.
`edge/nginx.conf` and `docker-compose.yml` are released from here, by
`ops/deploy.sh`. On the server those scripts change at the website's next
release; until then they take their "already present" path.
