"""SSH transport for git operations. Two responsibilities:

1. ``configure_git_ssh()`` — sets ``GIT_SSH_COMMAND=/usr/bin/ssh`` so git
   uses Apple's OpenSSH (which understands ``UseKeychain``) instead of
   the nix-provided OpenSSH that aborts with ``Bad configuration option:
   usekeychain``. Idempotent; respects an already-set ``GIT_SSH_COMMAND``.

2. ``ssh_diagnostics()`` — prints a 5-step recovery hint when a clone or
   submodule init fails, including a live ``ssh -T git@github.com`` probe."""
from __future__ import annotations

import os
from pathlib import Path

from . import log, sh

SYSTEM_SSH = Path("/usr/bin/ssh")


def configure_git_ssh() -> None:
    if not SYSTEM_SSH.is_file() or not os.access(SYSTEM_SSH, os.X_OK):
        return
    if os.environ.get("GIT_SSH_COMMAND"):
        return
    os.environ["GIT_SSH_COMMAND"] = str(SYSTEM_SSH)
    log.info(
        "using system /usr/bin/ssh for git "
        "(works around nix-ssh + macOS UseKeychain)"
    )


def ssh_diagnostics() -> None:
    print("")
    log.info("SSH-to-GitHub auth check:")
    if SYSTEM_SSH.is_file():
        rc, out, err = sh.capture(
            [
                str(SYSTEM_SSH), "-T",
                "-o", "BatchMode=yes",
                "-o", "StrictHostKeyChecking=accept-new",
                "git@github.com",
            ],
            timeout=10,
        )
        for line in (out + err).splitlines():
            print(f"  {line}")
    print("")
    print("  If you saw 'Permission denied (publickey)' above, configure an SSH key:")
    print('    1. ls ~/.ssh/id_ed25519.pub  (or id_rsa.pub) — does a key exist?')
    print('       If not: ssh-keygen -t ed25519 -C "your_email@example.com"')
    print('    2. Start the agent + add the key:')
    print('       eval "$(ssh-agent -s)"')
    print("       ssh-add --apple-use-keychain ~/.ssh/id_ed25519   # macOS")
    print("       ssh-add ~/.ssh/id_ed25519                         # other")
    print('    3. Copy the public key and add it on GitHub:')
    print("       pbcopy < ~/.ssh/id_ed25519.pub                    # macOS")
    print("       open https://github.com/settings/ssh/new")
    print('    4. Verify: ssh -T git@github.com   (expect: \'Hi <user>! …\')')
    print("    5. Re-click 'Launch Native App' to retry.")
    print("")
    print("  If you saw a 'Bad configuration option' error, your ~/.ssh/config has")
    print("  a directive nix-ssh doesn't understand (commonly UseKeychain). The")
    print("  script already routes through /usr/bin/ssh — if that still failed,")
    print("  comment that line out and retry, or set NY_RN_PATH to a working checkout.")
