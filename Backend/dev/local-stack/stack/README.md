# /opt/ny/local-stack — what this folder is

Everything here that git ships comes from **`Backend/dev/local-stack/stack/`**
of the Movin backend repository (`algeria/osrm-routing`), released by
`ops/deploy.sh` on a laptop. Do not edit those files here: a hand edit is
found by the next release, which stops and names it.

    cat .shipped                      which commit is deployed, when, by whom
    .shipped.files                    the sha256 of every file it shipped
    /opt/ny/local-stack.prev          what the last release replaced (rollback)
    systemd/                          units the release installs in /etc/systemd/system
                                      (movin-backup: the nightly backup runs ./backup.sh)

Everything else in this folder is the server's own and no release touches it:
`.env` and the secrets, the certificates, `edge-web/` (the website's build),
`bin/` and `2023/`, the map data, the bot's state, the drivers' codes.

The full documentation is the repository's `Backend/dev/local-stack/README.md`;
how to run the stack is `setup.sh`'s header.
