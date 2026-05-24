# passetto-server binaries

`config_transfer.py` picks one of these by host architecture (`platform.machine()`):

| Arch suffix | Used on                              |
|-------------|--------------------------------------|
| `aarch64`   | macOS Apple Silicon, Linux ARM64     |
| `x86_64`    | Linux x86_64 (k8s pods, CI runners)  |

The current `passetto-server-aarch64` is a macOS arm64 Mach-O — fine for local
dev on Apple Silicon. To run inside an x86_64 Linux container you need a Linux
ELF built for that arch. The container image build should COPY one in at
`Backend/dev/config-sync/bin/passetto-server-x86_64` (e.g. produced by
`nix build .#packages.x86_64-linux.passetto-service` against the upstream
flake, or whatever your existing build pipeline emits).

Files in this directory are gitignored — each environment populates its own.
