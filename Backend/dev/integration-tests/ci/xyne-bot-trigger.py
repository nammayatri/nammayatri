#!/usr/bin/env python3
"""
CI: post an alert to a Xyne channel through a Xyne bot app (Slack-compatible chat.postMessage).

Usage:
    xyne-bot-trigger.py --channel <channel-id> --title "🔴 main build FAILED" --message-file msg.txt
    xyne-bot-trigger.py --channel <channel-id> --title "..." --message "line 1\\nline 2"
    ... --dry-run    print the request that would be sent (token masked) and exit without sending

The token never goes on the command line or into the log; it is read from the environment:
    XYNE_BOT_TOKEN     the bot app's JWT (a GitHub secret)
    XYNE_API_BASE_URL  optional, default https://spaces.xyne.juspay.net/api/apps/slack
The script POSTs to <base>/chat.postMessage with a Slack-shaped body:
    {"channel", "text", "attachments": [{"color", "blocks": [title, body]}]}
Like Slack, the API answers HTTP 200 even on logical failures ({"ok": false, "error": ...}), so only
a "ts" (message id) in the reply counts as delivered.
Exit code: 0 = delivered, 1 = not delivered (Xyne's error printed), 2 = token missing / bad arguments.
"""

import argparse, json, os, sys, urllib.error, urllib.request

DEFAULT_BASE = "https://spaces.xyne.juspay.net/api/apps/slack"
RED = "#dc2626"   # the "failed/aborted" colour the release bot uses
MAX_BODY = 2900   # Slack-style section text allows 3000 characters


def mask(token):
    return token[:4] + "…" + token[-4:] if len(token) > 12 else "****"


def build_payload(channel, title, body, color):
    if len(body) > MAX_BODY:
        body = body[:MAX_BODY] + "\n… (truncated — open the run link for everything)"
    blocks = [{"type": "section", "text": {"type": "mrkdwn", "text": f"*{title}*"}}]
    if body:
        blocks.append({"type": "section", "text": {"type": "mrkdwn", "text": body}})
    return {"channel": channel, "text": title, "attachments": [{"color": color, "blocks": blocks}]}


def main():
    p = argparse.ArgumentParser(description="Post an alert to a Xyne channel via a Xyne bot app")
    p.add_argument("--channel", required=True, help="Xyne channel id (the bot must be a member of it)")
    p.add_argument("--title", required=True, help="first line: shown bold and used as the notification text")
    src = p.add_mutually_exclusive_group()
    src.add_argument("--message", help="message body (\\n for new lines)")
    src.add_argument("--message-file", help="read the message body from this file")
    p.add_argument("--color", default=RED, help=f"side-bar colour (default {RED})")
    p.add_argument("--timeout", type=float, default=10, help="seconds to wait for Xyne (default 10)")
    p.add_argument("--dry-run", action="store_true", help="print the request instead of sending it")
    a = p.parse_args()

    if a.message_file:
        try:
            with open(a.message_file, encoding="utf-8", errors="replace") as f:
                body = f.read()
        except OSError as e:
            print(f"xyne-bot-trigger: cannot read {a.message_file}: {e}", file=sys.stderr)
            return 2
    else:
        body = (a.message or "").replace("\\n", "\n")
    payload = build_payload(a.channel, a.title, body.strip(), a.color)

    token = os.environ.get("XYNE_BOT_TOKEN", "")
    url = (os.environ.get("XYNE_API_BASE_URL") or DEFAULT_BASE).rstrip("/") + "/chat.postMessage"

    if a.dry_run:
        print(f"POST {url}")
        print(f"Authorization: Bearer {mask(token) if token else '<XYNE_BOT_TOKEN not set>'}")
        print(json.dumps(payload, indent=2, ensure_ascii=False))
        return 0
    if not token:
        print("xyne-bot-trigger: skipped — XYNE_BOT_TOKEN is not set (is the GitHub secret configured?)",
              file=sys.stderr)
        return 2

    req = urllib.request.Request(
        url, data=json.dumps(payload).encode("utf-8"), method="POST",
        headers={"Authorization": f"Bearer {token}", "Content-Type": "application/json; charset=utf-8"})
    try:
        with urllib.request.urlopen(req, timeout=a.timeout) as resp:
            raw = resp.read().decode("utf-8", errors="replace")
    except urllib.error.HTTPError as e:
        print(f"xyne-bot-trigger: HTTP {e.code} from Xyne: {e.read().decode('utf-8', 'replace')[:300]}",
              file=sys.stderr)
        return 1
    except (urllib.error.URLError, TimeoutError, OSError) as e:
        print(f"xyne-bot-trigger: could not reach Xyne ({url}): {getattr(e, 'reason', e)}", file=sys.stderr)
        return 1

    try:
        reply = json.loads(raw)
    except ValueError:
        reply = {}
    if isinstance(reply, dict) and reply.get("ts"):
        print(f"xyne-bot-trigger: posted to {a.channel} (ts={reply['ts']})")
        return 0
    err = reply.get("error") or reply.get("message") or "unknown" if isinstance(reply, dict) else "unknown"
    print(f"xyne-bot-trigger: NOT posted (error={err}): {raw[:300]}", file=sys.stderr)
    return 1


if __name__ == "__main__":
    sys.exit(main())
