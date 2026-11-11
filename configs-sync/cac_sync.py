#!/usr/bin/env python3
"""
cac_sync.py — sync Superposition (CAC) context overrides from one context
"family" to another, reviewed and applied from a TUI.

    pip install textual
    python3 cac_sync.py --source '{"app":"lynx"}' --target '{"app":"bridge"}' [--cookie '<cookie>']
    python3 cac_sync.py        # pick source/target dimensions and values interactively

How it works
------------
1. GET /config to load every context, override and default config.
2. Every context whose condition CONTAINS the source condition is a source
   context, e.g. {"app":"lynx"} matches {"app":"lynx","city":"helsinki"}.
3. Each one is mapped to a target condition by swapping the source keys for the
   target keys: {"app":"lynx","city":"helsinki"} -> {"app":"bridge","city":"helsinki"}.
4. Each source key that is missing (ADD) or different (UPDATE) in the target
   context is listed and ticked (✓) by default.
5. Apply:
     target context exists  -> PATCH /context/overrides  (full merged override)
     target context missing -> PUT   /context            (creates it)

Keys:  space tick/untick · c tick/untick context · a tick/untick all
       e edit value · k update cookie · p apply · q quit
"""
from __future__ import annotations

import argparse
import json
import os
import readline
import ssl
import urllib.error
import urllib.request
from dataclasses import dataclass, field
from typing import Any

UA = ("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 "
      "(KHTML, like Gecko) Chrome/130.0 Safari/537.36")

# --------------------------------------------------------------------------- #
# API client
# --------------------------------------------------------------------------- #


class AuthExpired(Exception):
    pass


AUTH_MSG = ("Not logged in / session expired: Superposition redirected to SSO. "
            "Paste a fresh Cookie header (DevTools → Network → any superposition "
            "request → Request Headers → cookie).")


def normalize_cookie(cookie: str) -> str:
    cookie = cookie.strip()
    if cookie.lower().startswith("cookie:"):
        cookie = cookie[7:].strip()
    return cookie


class SPClient:
    def __init__(self, host: str, org: str, workspace: str,
                 cookie: str | None = None,
                 insecure: bool = False):
        self.host = host.rstrip("/")
        self.headers = {
            "Content-Type": "application/json",
            "Accept": "application/json",
            "User-Agent": UA,
            "x-org-id": org,
            "x-workspace": workspace,
            "x-tenant": workspace,
        }
        if cookie:
            self.set_cookie(cookie)
        self.ctx = ssl._create_unverified_context() if insecure else None

    def set_cookie(self, cookie: str) -> None:
        self.headers["Cookie"] = normalize_cookie(cookie)

    def _req(self, method: str, path: str, body: Any = None) -> Any:
        data = json.dumps(body).encode() if body is not None else None
        req = urllib.request.Request(self.host + path, data=data,
                                     method=method, headers=self.headers)
        try:
            with urllib.request.urlopen(req, context=self.ctx, timeout=60) as r:
                if "openid-connect" in r.geturl():
                    raise AuthExpired(AUTH_MSG)
                raw = r.read().decode(errors="replace")
                if not raw:
                    return None
                try:
                    return json.loads(raw)
                except json.JSONDecodeError:
                    raise RuntimeError(
                        f"{method} {path} -> not JSON:\n{raw[:600]}") from None
        except urllib.error.HTTPError as e:
            if e.code in (301, 302, 303, 307, 308, 401):
                raise AuthExpired(AUTH_MSG) from None
            raise RuntimeError(
                f"{method} {path} -> {e.code}: {e.read().decode()[:500]}") from None

    def get_config(self) -> dict:
        return self._req("GET", "/config")

    def create_context(self, condition: dict, override: dict,
                       description: str, change_reason: str) -> Any:
        return self._req("PUT", "/context", {
            "context": condition, "override": override,
            "description": description, "change_reason": change_reason,
        })

    def replace_overrides(self, condition: dict, override: dict,
                          change_reason: str) -> Any:
        return self._req("PATCH", "/context/overrides", {
            "context": {"context": condition}, "override": override,
            "change_reason": change_reason,
        })


# --------------------------------------------------------------------------- #
# Diff model
# --------------------------------------------------------------------------- #

def canon(v: Any) -> str:
    return json.dumps(v, sort_keys=True, separators=(",", ":"))


@dataclass
class Item:
    key: str
    kind: str                 # ADD | UPDATE
    source_val: Any
    target_val: Any           # current value in target ctx (None if absent)
    default_val: Any
    value: Any                # value that will be written (editable)
    checked: bool = True
    edited: bool = False

    @property
    def equals_default(self) -> bool:
        return canon(self.value) == canon(self.default_val)


@dataclass
class CtxPlan:
    source_cond: dict
    target_cond: dict
    target_exists: bool
    target_override: dict
    items: list[Item] = field(default_factory=list)

    def selected(self) -> list[Item]:
        return [i for i in self.items if i.checked]

    def merged_override(self) -> dict:
        out = dict(self.target_override)
        for i in self.selected():
            out[i.key] = i.value
        return out


def cond_label(c: dict) -> str:
    return ", ".join(f"{k}={v}" for k, v in c.items())


def build_plans(cfg: dict, source: dict, target: dict) -> list[CtxPlan]:
    overrides = cfg.get("overrides", {})
    defaults = cfg.get("default_configs", {})

    def ctx_override(ctx: dict) -> dict:
        out: dict = {}
        for k in ctx.get("override_with_keys", []):
            out.update(overrides.get(k, {}))
        return out

    by_cond = {canon(c["condition"]): c for c in cfg.get("contexts", [])}
    plans: list[CtxPlan] = []
    for ctx in cfg.get("contexts", []):
        cond = ctx["condition"]
        if not isinstance(cond, dict):
            continue
        if not all(k in cond and canon(cond[k]) == canon(v) for k, v in source.items()):
            continue
        tcond = {k: v for k, v in cond.items() if k not in source}
        tcond.update(target)
        tcond = {k: tcond[k] for k in list(cond) + list(target) if k in tcond}
        tctx = by_cond.get(canon(tcond))
        t_over = ctx_override(tctx) if tctx else {}
        plan = CtxPlan(cond, tcond, tctx is not None, t_over)
        for key, sval in sorted(ctx_override(ctx).items()):
            if key not in t_over:
                kind = "ADD"
            elif canon(t_over[key]) != canon(sval):
                kind = "UPDATE"
            else:
                continue
            plan.items.append(
                Item(key, kind, sval, t_over.get(key), defaults.get(key), sval))
        if plan.items:
            plans.append(plan)
    plans.sort(key=lambda p: (len(p.source_cond), cond_label(p.source_cond)))
    return plans


def apply_plan(client: SPClient, p: CtxPlan, reason: str) -> str:
    if p.target_exists:
        client.replace_overrides(p.target_cond, p.merged_override(), reason)
        return "updated"
    client.create_context(p.target_cond, p.merged_override(),
                          f"Synced from {cond_label(p.source_cond)}", reason)
    return "created"


def commit_plan(p: CtxPlan) -> None:
    p.target_override = p.merged_override()
    p.target_exists = True
    for i in p.items:
        i.checked = False


# --------------------------------------------------------------------------- #
# TUI
# --------------------------------------------------------------------------- #

def short(v: Any, n: int = 48) -> str:
    s = "∅" if v is None else canon(v)
    return s if len(s) <= n else s[: n - 1] + "…"


def run_tui(plans: list[CtxPlan], client: SPClient | None, reason: str, dry_run: bool) -> None:
    from rich.text import Text
    from textual.app import App, ComposeResult
    from textual.binding import Binding
    from textual.containers import Horizontal, Vertical
    from textual.screen import ModalScreen
    from textual.widgets import (Button, DataTable, Footer, Header, Input,
                                 Label, RichLog, Static, TextArea)

    rows: list[tuple[CtxPlan, Item]] = [(p, i) for p in plans for i in p.items]

    class EditScreen(ModalScreen[Any]):
        DEFAULT_CSS = """
        EditScreen { align: center middle; }
        #box { width: 90%; height: 80%; border: thick $accent; background: $surface; padding: 1; }
        #ta { height: 1fr; }
        #err { color: $error; height: 1; }
        """
        BINDINGS = [Binding("ctrl+s", "save", "Save"),
                    Binding("escape", "cancel", "Cancel")]

        def __init__(self, item: Item):
            super().__init__()
            self.item = item

        def compose(self) -> ComposeResult:
            with Vertical(id="box"):
                yield Label(Text(f"Edit value for {self.item.key} (JSON) · ctrl+s save · esc cancel"))
                yield TextArea(json.dumps(self.item.value, indent=2), language="json", id="ta")
                yield Static("", id="err", markup=False)

        def action_save(self) -> None:
            try:
                val = json.loads(self.query_one("#ta", TextArea).text)
            except json.JSONDecodeError as e:
                self.query_one("#err", Static).update(f"Invalid JSON: {e}")
                return
            self.dismiss(val)

        def action_cancel(self) -> None:
            self.dismiss(None)

    class CookieScreen(ModalScreen[Any]):
        DEFAULT_CSS = """
        CookieScreen { align: center middle; }
        #box { width: 90%; height: auto; border: thick $error; background: $surface; padding: 1; }
        """
        BINDINGS = [Binding("escape", "cancel", "Cancel")]

        def __init__(self, msg: str):
            super().__init__()
            self.msg = msg

        def compose(self) -> ComposeResult:
            with Vertical(id="box"):
                yield Static(self.msg, markup=False)
                yield Label("Paste Cookie header · enter save · esc cancel")
                yield Input(password=True, id="cookie")

        def on_input_submitted(self, ev: Input.Submitted) -> None:
            cookie = normalize_cookie(ev.value)
            if cookie:
                self.dismiss(cookie)

        def action_cancel(self) -> None:
            self.dismiss(None)

    class ConfirmScreen(ModalScreen[bool]):
        DEFAULT_CSS = """
        ConfirmScreen { align: center middle; }
        #box { width: 90%; height: 80%; border: thick $warning; background: $surface; padding: 1; }
        #summary { height: 1fr; overflow-y: auto; }
        #btns { height: 3; align: center middle; }
        """

        def __init__(self, text: str):
            super().__init__()
            self.text = text

        def compose(self) -> ComposeResult:
            with Vertical(id="box"):
                yield Static(self.text, id="summary", markup=False)
                with Horizontal(id="btns"):
                    yield Button("Apply (dry run)" if dry_run else "Apply", variant="warning", id="yes")
                    yield Button("Cancel", id="no")

        def on_button_pressed(self, ev: Button.Pressed) -> None:
            self.dismiss(ev.button.id == "yes")

    class SyncApp(App):
        TITLE = "CAC context sync"
        CSS = """
        #table { height: 2fr; }
        #detail { height: 1fr; border-top: solid $accent; padding: 0 1; overflow-y: auto; }
        #log { height: 8; border-top: solid $accent; display: none; }
        """
        BINDINGS = [
            Binding("space", "toggle", "Tick"),
            Binding("c", "toggle_ctx", "Tick context"),
            Binding("a", "toggle_all", "Tick all"),
            Binding("e", "edit", "Edit value"),
            Binding("k", "cookie", "Update cookie"),
            Binding("p", "apply", "Apply"),
            Binding("q", "quit", "Quit"),
        ]

        def compose(self) -> ComposeResult:
            yield Header()
            yield DataTable(id="table", cursor_type="row", zebra_stripes=True)
            yield Static("", id="detail", markup=False)
            yield RichLog(id="log", markup=False)
            yield Footer()

        def on_mount(self) -> None:
            t = self.query_one(DataTable)
            t.add_columns(" ", "target context", "status", "kind", "key",
                          "value to write", "target now", "default")
            for idx, (p, i) in enumerate(rows):
                t.add_row(*self._cells(p, i), key=str(idx))
            self._update_title()
            if rows:
                self._show_detail(0)

        def _cells(self, p: CtxPlan, i: Item):
            tick = Text("✓", style="bold green") if i.checked else Text(" ")
            status = "update" if p.target_exists else "NEW"
            kind = i.kind + ("*" if i.edited else "")
            return (tick, Text(cond_label(p.target_cond)), status, kind, Text(i.key),
                    Text(short(i.value)), Text(short(i.target_val, 30)),
                    Text(short(i.default_val, 30)))

        def _refresh_row(self, idx: int) -> None:
            t = self.query_one(DataTable)
            p, i = rows[idx]
            for col, val in zip(t.columns.keys(), self._cells(p, i)):
                t.update_cell(str(idx), col, val)

        def _update_title(self) -> None:
            n = sum(i.checked for _, i in rows)
            new = sum(1 for p in plans if not p.target_exists and p.selected())
            upd = sum(1 for p in plans if p.target_exists and p.selected())
            self.sub_title = (f"{n}/{len(rows)} ticked · {new} contexts to create · "
                              f"{upd} to update{' · DRY RUN' if dry_run else ''}")

        def _cur(self) -> int | None:
            return self.query_one(DataTable).cursor_row if rows else None

        def _show_detail(self, idx: int) -> None:
            p, i = rows[idx]
            self.query_one("#detail", Static).update(
                f"source ctx : {cond_label(p.source_cond)}\n"
                f"target ctx : {cond_label(p.target_cond)} "
                f"({'exists' if p.target_exists else 'will be CREATED'})\n"
                f"key        : {i.key}   [{i.kind}{', edited' if i.edited else ''}]\n"
                f"write      : {canon(i.value)}\n"
                f"target now : {canon(i.target_val) if i.target_val is not None else '∅ (falls back to default)'}\n"
                f"default    : {canon(i.default_val)}")

        def on_data_table_row_highlighted(self, ev: DataTable.RowHighlighted) -> None:
            if rows and ev.cursor_row is not None and ev.cursor_row < len(rows):
                self._show_detail(ev.cursor_row)

        def _set(self, idxs, value: bool | None = None) -> None:
            for idx in idxs:
                it = rows[idx][1]
                it.checked = (not it.checked) if value is None else value
                self._refresh_row(idx)
            self._update_title()

        def action_toggle(self) -> None:
            idx = self._cur()
            if idx is not None:
                self._set([idx])

        def action_toggle_ctx(self) -> None:
            idx = self._cur()
            if idx is None:
                return
            plan = rows[idx][0]
            idxs = [n for n, (p, _) in enumerate(rows) if p is plan]
            self._set(idxs, not all(rows[n][1].checked for n in idxs))

        def action_toggle_all(self) -> None:
            self._set(range(len(rows)), not all(i.checked for _, i in rows))

        def action_edit(self) -> None:
            idx = self._cur()
            if idx is None:
                return
            item = rows[idx][1]

            def done(val: Any) -> None:
                if val is None:
                    return
                item.value = val
                item.edited = canon(val) != canon(item.source_val)
                item.checked = True
                self._refresh_row(idx)
                self._show_detail(idx)
                self._update_title()

            self.push_screen(EditScreen(item), done)

        def action_cookie(self) -> None:
            def done(cookie: Any) -> None:
                if cookie and client is not None:
                    client.set_cookie(cookie)
                    self.notify("Cookie updated")

            self.push_screen(CookieScreen("Update Superposition cookie."), done)

        def _resume(self, cookie: Any) -> None:
            if not cookie:
                self.notify("Apply paused — press k to set cookie, then p to continue",
                            severity="warning")
                return
            client.set_cookie(cookie)
            self.notify("Cookie updated — resuming apply")
            self._do_apply(True)

        def _refresh_all(self) -> None:
            for n in range(len(rows)):
                self._refresh_row(n)
            self._update_title()
            cur = self._cur()
            if cur is not None:
                self._show_detail(cur)

        def action_apply(self) -> None:
            todo = [p for p in plans if p.selected()]
            if not todo:
                self.notify("Nothing ticked", severity="warning")
                return
            lines = [f"change_reason: {reason}", ""]
            for p in todo:
                lines.append(
                    f"{'UPDATE' if p.target_exists else 'CREATE'} {{{cond_label(p.target_cond)}}}")
                for i in p.selected():
                    lines.append(
                        f"    {'+' if i.kind == 'ADD' else '~'} {i.key} = {short(i.value, 90)}")
                lines.append("")
            self.push_screen(ConfirmScreen("\n".join(lines)), self._do_apply)

        def _do_apply(self, ok: bool) -> None:
            if not ok:
                return
            log = self.query_one("#log", RichLog)
            log.display = True
            if dry_run or client is None:
                for p in plans:
                    if p.selected():
                        method = "PATCH /context/overrides" if p.target_exists else "PUT /context"
                        log.write(f"[dry-run] {method} {cond_label(p.target_cond)}: "
                                  f"{canon(p.merged_override())[:300]}")
                return
            fails = 0
            for p in [p for p in plans if p.selected()]:
                label = cond_label(p.target_cond)
                n = len(p.selected())
                try:
                    verb = apply_plan(client, p, reason)
                except AuthExpired as e:
                    log.write(f"[auth] session expired at {label}; remaining ticked items kept")
                    self._refresh_all()
                    self.push_screen(CookieScreen(str(e)), self._resume)
                    return
                except Exception as e:  # noqa: BLE001
                    fails += 1
                    log.write(f"[FAIL] {label}: {e}")
                    continue
                commit_plan(p)
                log.write(f"[ok] {verb}  {label}  ({n} keys)")
            self._refresh_all()
            if fails:
                self.notify(f"{fails} context(s) failed — see log; still ticked, p to retry",
                            severity="error")
                return
            self.notify("All ticked updates applied. Re-run to verify.")

    SyncApp().run()


# --------------------------------------------------------------------------- #
# CLI
# --------------------------------------------------------------------------- #

def prompt_cookie() -> str:
    try:
        cookie = normalize_cookie(input("Paste Superposition Cookie header: "))
    except (EOFError, KeyboardInterrupt):
        raise SystemExit("\nNo cookie provided.")
    if not cookie:
        raise SystemExit("No cookie provided.")
    return cookie


def ask(prompt: str) -> str:
    try:
        return input(prompt).strip()
    except (EOFError, KeyboardInterrupt):
        raise SystemExit("\nAborted.")


def pick_many(title: str, options: list[str], default: list[str] | None = None) -> list[str]:
    for n, o in enumerate(options, 1):
        print(f"  {n:>3}) {o}")
    while True:
        raw = ask(f"{title} (numbers or names, comma-separated): ")
        if not raw and default:
            return default
        out: list[str] = []
        for tok in filter(None, (t.strip() for t in raw.split(","))):
            if tok.isdigit() and 1 <= int(tok) <= len(options):
                out.append(options[int(tok) - 1])
            elif tok in options:
                out.append(tok)
            else:
                print(f"  unknown: {tok}")
                break
        else:
            if out:
                return list(dict.fromkeys(out))


def pick_value(key: str, values: list[Any]) -> Any:
    for n, v in enumerate(values, 1):
        print(f"  {n:>3}) {canon(v)}")
    while True:
        raw = ask(f"{key} = (number, or type a new value): ")
        if raw.isdigit() and values:
            if 1 <= int(raw) <= len(values):
                return values[int(raw) - 1]
            print(f"  pick 1-{len(values)}")
            continue
        if raw:
            try:
                return json.loads(raw)
            except json.JSONDecodeError:
                return raw


def dict_conditions(cfg: dict) -> list[dict]:
    return [c["condition"] for c in cfg.get("contexts", []) if isinstance(c.get("condition"), dict)]


def dim_values(cfg: dict, key: str, within: dict) -> list[Any]:
    seen: dict[str, Any] = {}
    for c in dict_conditions(cfg):
        if key in c and all(k in c and canon(c[k]) == canon(v) for k, v in within.items()):
            seen.setdefault(canon(c[key]), c[key])
    return sorted(seen.values(), key=canon)


def prompt_condition(cfg: dict, label: str, scoped: bool,
                     default_keys: list[str] | None = None) -> dict:
    keys = sorted({k for c in dict_conditions(cfg) for k in c})
    print(f"\n== {label} context: choose dimensions ==")
    if default_keys:
        print(f"  enter = same as source ({', '.join(default_keys)})")
    cond: dict = {}
    for k in pick_many("dimensions", keys, default_keys):
        print(f"\n-- {label} {k} --")
        cond[k] = pick_value(k, dim_values(cfg, k, cond if scoped else {}))
    print(f"{label}: {json.dumps(cond)}")
    return cond


def main() -> None:
    ap = argparse.ArgumentParser(
        description="Sync CAC overrides between context families")
    ap.add_argument("--source", help='e.g. \'{"app":"lynx"}\' (prompted if omitted)')
    ap.add_argument("--target",
                    help='e.g. \'{"app":"bridge"}\' (prompted if omitted)')
    ap.add_argument("--host", default="https://superposition.juspay.io")
    ap.add_argument("--org", default="movingtech")
    ap.add_argument("--workspace", default="driverapp")
    ap.add_argument("--cookie", default=os.getenv("SP_COOKIE"))
    ap.add_argument("--reason", default=None,
                    help="change_reason sent with every write")
    ap.add_argument("--config-file",
                    help="read /config JSON from a file instead of the API")
    ap.add_argument("--dry-run", action="store_true",
                    help="never write; show payloads")
    ap.add_argument("--insecure", action="store_true",
                    help="skip TLS verification")
    a = ap.parse_args()

    source = json.loads(a.source) if a.source else None
    target = json.loads(a.target) if a.target else None
    cookie = a.cookie
    if not cookie and not (a.config_file and a.dry_run):
        cookie = prompt_cookie()
    client = SPClient(a.host, a.org, a.workspace, cookie, a.insecure)
    if a.config_file:
        with open(a.config_file) as f:
            cfg = json.load(f)
    else:
        while True:
            try:
                cfg = client.get_config()
                break
            except AuthExpired as e:
                print(e)
                client.set_cookie(prompt_cookie())

    if source is None:
        source = prompt_condition(cfg, "source", scoped=True)
    if target is None:
        target = prompt_condition(cfg, "target", scoped=False, default_keys=list(source))
    if canon(source) == canon(target):
        raise SystemExit("Source and target are the same.")
    reason = a.reason or f"Sync overrides from {cond_label(source)} to {cond_label(target)}"

    plans = build_plans(cfg, source, target)
    if not plans:
        print("Nothing to sync — target already matches source.")
        return
    run_tui(plans, client, reason, a.dry_run)


if __name__ == "__main__":
    main()
