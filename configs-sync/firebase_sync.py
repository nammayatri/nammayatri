#!/usr/bin/env python3
"""
firebase_sync.py — sync Firebase Remote Config values from one project/city to
another (same or different project), reviewed and applied from a TUI.
Talks to the same Control Center API the /config/firebase page uses.

    pip install textual
    python3 firebase_sync.py                                   # pick everything interactively
    python3 firebase_sync.py --source-project movingtech-international --source-city helsinki \
                             --target-project movingtech-international --target-city amsterdam

How it works
------------
1. GET  /firebase/projects, GET /firebase/config/<project>  -> {template, etag}
2. Parameters whose value is JSON keyed by city ({"helsinki": {...}, ...}) are
   scanned in the default value and in every conditional value (condition slot).
3. For each (parameter, slot) where the source city exists, the source city's
   keys are diffed against the target city in the same parameter/slot of the
   target project:
       ADD     key missing in target city      ticked ✓ by default
       UPDATE  key differs                     ticked ✓ by default
   A non-object city value is compared as a whole (key shown as "*").
   Condition slots that don't exist in the target project are skipped.
4. Apply (one publish for all ticked items):
       POST /firebase/config/<target>/validate  {template}
       POST /firebase/config/<target>           {template, etag}

Auth: the Control Center "token" header. In the Control Center tab open DevTools
console and run:  localStorage.getItem('dashboard_token')
Pass it with --token, SP env FB_TOKEN, or paste it when asked.

Keys:  space tick/untick · c tick/untick parameter · a tick/untick all
       e edit value · k update token · p apply · q quit
"""
from __future__ import annotations

import argparse
import copy
import json
import os
import ssl
import urllib.error
import urllib.request
from dataclasses import dataclass, field
from typing import Any

UA = ("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 "
      "(KHTML, like Gecko) Chrome/130.0 Safari/537.36")
WHOLE = "*"
DEFAULT_SLOT = "(default)"

# --------------------------------------------------------------------------- #
# API client
# --------------------------------------------------------------------------- #


class AuthExpired(Exception):
    pass


AUTH_MSG = ("Not logged in / token expired. In the Control Center tab run in the "
            "DevTools console:  localStorage.getItem('dashboard_token')  and paste the value.")


def normalize_token(tok: str) -> str:
    tok = tok.strip()
    if tok.lower().startswith("token:"):
        tok = tok[6:].strip()
    return tok.strip("'\"")


class CCClient:
    def __init__(self, host: str, token: str | None = None, insecure: bool = False):
        self.host = host.rstrip("/")
        self.headers = {"Content-Type": "application/json",
                        "Accept": "application/json", "User-Agent": UA}
        if token:
            self.set_token(token)
        self.ctx = ssl._create_unverified_context() if insecure else None

    def set_token(self, token: str) -> None:
        self.headers["token"] = normalize_token(token)

    def _req(self, method: str, path: str, body: Any = None) -> Any:
        data = json.dumps(body).encode() if body is not None else None
        req = urllib.request.Request(self.host + path, data=data,
                                     method=method, headers=self.headers)
        try:
            with urllib.request.urlopen(req, context=self.ctx, timeout=90) as r:
                raw = r.read().decode(errors="replace")
                if not raw:
                    return None
                try:
                    return json.loads(raw)
                except json.JSONDecodeError:
                    raise RuntimeError(
                        f"{method} {path} -> not JSON:\n{raw[:600]}") from None
        except urllib.error.HTTPError as e:
            if e.code in (401, 403):
                raise AuthExpired(AUTH_MSG) from None
            raise RuntimeError(
                f"{method} {path} -> {e.code}: {e.read().decode()[:800]}") from None

    def projects(self) -> list[str]:
        data = self._req("GET", "/firebase/projects") or []
        return [p.get("id") or p.get("name") for p in data]

    def get_config(self, project: str) -> dict:
        return self._req("GET", f"/firebase/config/{project}")

    def validate(self, project: str, template: dict) -> dict:
        return self._req("POST", f"/firebase/config/{project}/validate", {"template": template})

    def publish(self, project: str, template: dict, etag: str) -> Any:
        return self._req("POST", f"/firebase/config/{project}", {"template": template, "etag": etag})


# --------------------------------------------------------------------------- #
# Template helpers
# --------------------------------------------------------------------------- #

def canon(v: Any) -> str:
    return json.dumps(v, sort_keys=True, separators=(",", ":"))


def parse_json(v: Any) -> Any:
    if not isinstance(v, str):
        return None
    try:
        return json.loads(v)
    except json.JSONDecodeError:
        return None


def slots(param: dict) -> list[tuple[str, dict]]:
    """(slot name, value holder) pairs: default value + each conditional value."""
    out = []
    if isinstance(param.get("defaultValue"), dict):
        out.append((DEFAULT_SLOT, param["defaultValue"]))
    for cname, holder in (param.get("conditionalValues") or {}).items():
        if isinstance(holder, dict):
            out.append((cname, holder))
    return out


def slot_obj(param: dict | None, slot: str) -> dict | None:
    """Parsed JSON object stored in a parameter slot (None if absent / not an object)."""
    if not param:
        return None
    holder = param.get("defaultValue") if slot == DEFAULT_SLOT else \
        (param.get("conditionalValues") or {}).get(slot)
    obj = parse_json((holder or {}).get("value"))
    return obj if isinstance(obj, dict) else None


def all_params(template: dict) -> dict[str, tuple[dict, str | None]]:
    """Every parameter by name, top-level and inside parameterGroups, with its group name."""
    out = {name: (p, None) for name, p in (template.get("parameters") or {}).items()}
    for gname, group in (template.get("parameterGroups") or {}).items():
        for name, p in (group.get("parameters") or {}).items():
            out[name] = (p, gname)
    return out


def city_counts(template: dict) -> dict[str, int]:
    counts: dict[str, int] = {}
    for p, _ in all_params(template).values():
        for _, holder in slots(p):
            obj = parse_json(holder.get("value"))
            if isinstance(obj, dict):
                for k in obj:
                    counts[k] = counts.get(k, 0) + 1
    return dict(sorted(counts.items(), key=lambda kv: (-kv[1], kv[0])))


# --------------------------------------------------------------------------- #
# Diff model
# --------------------------------------------------------------------------- #

@dataclass
class Item:
    key: str                  # key inside the city object, or "*" for whole value
    kind: str                 # ADD | UPDATE
    source_val: Any
    target_val: Any
    value: Any                # value that will be written (editable)
    checked: bool = True
    edited: bool = False


@dataclass
class Group:
    param: str
    slot: str
    param_exists: bool
    city_exists: bool
    items: list[Item] = field(default_factory=list)

    def selected(self) -> list[Item]:
        return [i for i in self.items if i.checked]

    @property
    def status(self) -> str:
        if not self.param_exists:
            return "NEW param"
        if not self.city_exists:
            return "NEW city"
        return "update"


def build_groups(src_tpl: dict, src_city: str, tgt_tpl: dict, tgt_city: str) -> tuple[list[Group], list[str]]:
    sparams = all_params(src_tpl)
    tparams = all_params(tgt_tpl)
    tconds = {c.get("name") for c in (tgt_tpl.get("conditions") or [])}
    groups: list[Group] = []
    skipped: list[str] = []
    for pname in sorted(sparams):
        sparam = sparams[pname][0]
        tparam = tparams[pname][0] if pname in tparams else None
        for slot, _ in slots(sparam):
            sobj = slot_obj(sparam, slot)
            if not sobj or src_city not in sobj:
                continue
            if slot != DEFAULT_SLOT and slot not in tconds:
                skipped.append(
                    f"{pname} [{slot}] — condition missing in target project")
                continue
            if slot != DEFAULT_SLOT and tparam is not None and \
                    slot not in (tparam.get("conditionalValues") or {}):
                skipped.append(
                    f"{pname} [{slot}] — target parameter has no value for this condition; "
                    "adding one would override the default for every other city under it")
                continue
            if tparam is not None and slot_obj(tparam, slot) is None and \
                    (slot == DEFAULT_SLOT or slot in (tparam.get("conditionalValues") or {})):
                skipped.append(
                    f"{pname} [{slot}] — target value is not a JSON object")
                continue
            tobj = slot_obj(tparam, slot) or {}
            sval, tval = sobj[src_city], tobj.get(tgt_city)
            g = Group(pname, slot, tparam is not None, tgt_city in tobj)
            if isinstance(sval, dict) and (tval is None or isinstance(tval, dict)):
                tval = tval or {}
                for k in sval:
                    if k not in tval:
                        g.items.append(Item(k, "ADD", sval[k], None, sval[k]))
                    elif canon(tval[k]) != canon(sval[k]):
                        g.items.append(
                            Item(k, "UPDATE", sval[k], tval[k], sval[k]))
            elif tgt_city not in tobj:
                g.items.append(Item(WHOLE, "ADD", sval, None, sval))
            elif canon(tval) != canon(sval):
                g.items.append(Item(WHOLE, "UPDATE", sval, tval, sval))
            if g.items:
                groups.append(g)
    return groups, skipped


def build_target_template(src_tpl: dict, tgt_tpl: dict, groups: list[Group], tgt_city: str) -> dict:
    """Copy of the target template with every ticked item written into it."""
    out = copy.deepcopy(tgt_tpl)
    tparams = all_params(out)
    sparams = all_params(src_tpl)
    for g in groups:
        sel = g.selected()
        if not sel:
            continue
        if g.param not in tparams:
            sp, sgroup = sparams[g.param]
            container = out.setdefault("parameters", {})
            if sgroup and sgroup in (out.get("parameterGroups") or {}):
                container = out["parameterGroups"][sgroup].setdefault("parameters", {})
            p = container[g.param] = {k: copy.deepcopy(sp[k]) for k in (
                "valueType", "description") if k in sp}
            p["defaultValue"] = {"value": "{}"}
            tparams[g.param] = (p, sgroup)
        p = tparams[g.param][0]
        if g.slot == DEFAULT_SLOT:
            holder = p.setdefault("defaultValue", {"value": "{}"})
        else:
            holder = p.setdefault("conditionalValues", {}).setdefault(
                g.slot, {"value": "{}"})
        obj = parse_json(holder.get("value"))
        obj = obj if isinstance(obj, dict) else {}
        for i in sel:
            if i.key == WHOLE:
                obj[tgt_city] = i.value
            else:
                city = obj.get(tgt_city)
                if not isinstance(city, dict):
                    city = obj[tgt_city] = {}
                city[i.key] = i.value
        holder.pop("useInAppDefault", None)
        holder["value"] = json.dumps(
            obj, ensure_ascii=False, separators=(",", ":"))
    return out


def validate_ok(resp: Any) -> tuple[bool, str]:
    if isinstance(resp, dict) and resp.get("valid") is False:
        return False, resp.get("message") or canon(resp)
    msg = resp.get("message", "") if isinstance(resp, dict) else ""
    return True, msg


# --------------------------------------------------------------------------- #
# TUI
# --------------------------------------------------------------------------- #

def short(v: Any, n: int = 48) -> str:
    s = "∅" if v is None else canon(v)
    return s if len(s) <= n else s[: n - 1] + "…"


def run_tui(state: dict, client: CCClient, dry_run: bool) -> None:
    from rich.text import Text
    from textual.app import App, ComposeResult
    from textual.binding import Binding
    from textual.containers import Horizontal, Vertical
    from textual.screen import ModalScreen
    from textual.widgets import (Button, DataTable, Footer, Header, Input,
                                 Label, RichLog, Static, TextArea)

    groups: list[Group] = state["groups"]
    rows: list[tuple[Group, Item]] = [(g, i) for g in groups for i in g.items]
    sp, sc, tp, tc = state["sp"], state["sc"], state["tp"], state["tc"]

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
                yield TextArea(json.dumps(self.item.value, indent=2, ensure_ascii=False),
                               language="json", id="ta")
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

    class TokenScreen(ModalScreen[Any]):
        DEFAULT_CSS = """
        TokenScreen { align: center middle; }
        #box { width: 90%; height: auto; border: thick $error; background: $surface; padding: 1; }
        """
        BINDINGS = [Binding("escape", "cancel", "Cancel")]

        def __init__(self, msg: str):
            super().__init__()
            self.msg = msg

        def compose(self) -> ComposeResult:
            with Vertical(id="box"):
                yield Static(self.msg, markup=False)
                yield Label("Paste token · enter save · esc cancel")
                yield Input(password=True, id="token")

        def on_input_submitted(self, ev: Input.Submitted) -> None:
            tok = normalize_token(ev.value)
            if tok:
                self.dismiss(tok)

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
                    yield Button("Validate only (dry run)" if dry_run else "Validate & publish",
                                 variant="warning", id="yes")
                    yield Button("Cancel", id="no")

        def on_button_pressed(self, ev: Button.Pressed) -> None:
            self.dismiss(ev.button.id == "yes")

    class SyncApp(App):
        TITLE = f"Firebase RC sync  {sp}/{sc}  →  {tp}/{tc}"
        CSS = """
        #table { height: 2fr; }
        #detail { height: 1fr; border-top: solid $accent; padding: 0 1; overflow-y: auto; }
        #log { height: 8; border-top: solid $accent; display: none; }
        """
        BINDINGS = [
            Binding("space", "toggle", "Tick"),
            Binding("c", "toggle_group", "Tick parameter"),
            Binding("a", "toggle_all", "Tick all"),
            Binding("e", "edit", "Edit value"),
            Binding("k", "token", "Update token"),
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
            t.add_columns(" ", "parameter", "slot", "status", "kind", "key",
                          "value to write", "target now")
            for idx, (g, i) in enumerate(rows):
                t.add_row(*self._cells(g, i), key=str(idx))
            self._update_title()
            if rows:
                self._show_detail(0)
            if state["skipped"]:
                log = self.query_one("#log", RichLog)
                log.display = True
                for s in state["skipped"]:
                    log.write(f"[skip] {s}")

        def _cells(self, g: Group, i: Item):
            tick = Text("✓", style="bold green") if i.checked else Text(" ")
            kind = i.kind + ("*" if i.edited else "")
            return (tick, Text(g.param), Text(g.slot), g.status, kind, Text(i.key),
                    Text(short(i.value)), Text(short(i.target_val, 30)))

        def _refresh_row(self, idx: int) -> None:
            t = self.query_one(DataTable)
            g, i = rows[idx]
            for col, val in zip(t.columns.keys(), self._cells(g, i)):
                t.update_cell(str(idx), col, val)

        def _refresh_all(self) -> None:
            for n in range(len(rows)):
                self._refresh_row(n)
            self._update_title()
            cur = self._cur()
            if cur is not None:
                self._show_detail(cur)

        def _update_title(self) -> None:
            n = sum(i.checked for _, i in rows)
            ps = len({g.param for g in groups if g.selected()})
            self.sub_title = (f"{n}/{len(rows)} ticked · {ps} parameters"
                              f"{' · DRY RUN' if dry_run else ''}")

        def _cur(self) -> int | None:
            return self.query_one(DataTable).cursor_row if rows else None

        def _show_detail(self, idx: int) -> None:
            g, i = rows[idx]
            self.query_one("#detail", Static).update(
                f"parameter  : {g.param}   slot: {g.slot}   ({g.status})\n"
                f"key        : {tc}.{i.key}   [{i.kind}{', edited' if i.edited else ''}]\n"
                f"source     : {sp}/{sc} = {canon(i.source_val)}\n"
                f"write      : {canon(i.value)}\n"
                f"target now : {canon(i.target_val) if i.target_val is not None else '∅ (missing)'}")

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

        def action_toggle_group(self) -> None:
            idx = self._cur()
            if idx is None:
                return
            param = rows[idx][0].param
            idxs = [n for n, (g, _) in enumerate(rows) if g.param == param]
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

        def action_token(self) -> None:
            def done(tok: Any) -> None:
                if tok:
                    client.set_token(tok)
                    self.notify("Token updated")

            self.push_screen(TokenScreen("Update Control Center token."), done)

        def _resume(self, tok: Any) -> None:
            if not tok:
                self.notify(
                    "Apply paused — press k to set token, then p", severity="warning")
                return
            client.set_token(tok)
            self.notify("Token updated — resuming apply")
            self._do_apply(True)

        def action_apply(self) -> None:
            todo = [g for g in groups if g.selected()]
            if not todo:
                self.notify("Nothing ticked", severity="warning")
                return
            lines = [
                f"Publish to project {tp}, city {tc}  (from {sp}/{sc})", ""]
            for g in todo:
                lines.append(f"{g.param} [{g.slot}]  ({g.status})")
                for i in g.selected():
                    lines.append(
                        f"    {'+' if i.kind == 'ADD' else '~'} {i.key} = {short(i.value, 90)}")
                lines.append("")
            self.push_screen(ConfirmScreen("\n".join(lines)), self._do_apply)

        def _do_apply(self, ok: bool) -> None:
            if not ok:
                return
            log = self.query_one("#log", RichLog)
            log.display = True
            new_tpl = build_target_template(
                state["src_tpl"], state["tgt_tpl"], groups, tc)
            try:
                valid, msg = validate_ok(client.validate(tp, new_tpl))
                if not valid:
                    log.write(f"[FAIL] validate: {msg}")
                    self.notify("Validation failed — see log",
                                severity="error")
                    return
                log.write(f"[ok] validate {msg}")
                if dry_run:
                    for g in groups:
                        if g.selected():
                            log.write(f"[dry-run] would write {g.param} [{g.slot}] "
                                      f"{tc}: {len(g.selected())} keys")
                    return
                client.publish(tp, new_tpl, state["etag"])
            except AuthExpired as e:
                log.write(
                    "[auth] token expired; nothing published, ticks kept")
                self.push_screen(TokenScreen(str(e)), self._resume)
                return
            except Exception as e:  # noqa: BLE001
                log.write(f"[FAIL] {e}")
                if "412" in str(e) or "409" in str(e) or "etag" in str(e).lower():
                    log.write(
                        "The target config changed since it was loaded — restart the script.")
                self.notify(
                    "Publish failed — see log; ticks kept, p to retry", severity="error")
                return
            log.write(
                f"[ok] published {tp} ({sum(len(g.selected()) for g in groups)} keys)")
            try:  # reload so a second apply uses the fresh template/etag
                fresh = client.get_config(tp)
                state["tgt_tpl"], state["etag"] = fresh["template"], fresh["etag"]
                if sp == tp:
                    state["src_tpl"] = fresh["template"]
            except Exception as e:  # noqa: BLE001
                log.write(
                    f"[warn] reload failed, restart before applying again: {e}")
            for g in groups:
                for i in g.selected():
                    i.target_val, i.checked = i.value, False
                g.param_exists = g.city_exists = True
            self._refresh_all()
            self.notify("Published. Re-run to verify.")

    SyncApp().run()


# --------------------------------------------------------------------------- #
# CLI
# --------------------------------------------------------------------------- #

def ask(prompt: str) -> str:
    try:
        return input(prompt).strip()
    except (EOFError, KeyboardInterrupt):
        raise SystemExit("\nAborted.")


def prompt_token() -> str:
    tok = normalize_token(ask("Paste Control Center token "
                              "(DevTools console: localStorage.getItem('dashboard_token')): "))
    if not tok:
        raise SystemExit("No token provided.")
    return tok


def pick(title: str, options: list[str], labels: list[str] | None = None,
         allow_new: bool = False) -> str:
    print(f"\n{title}")
    for n, o in enumerate(options, 1):
        print(f"  {n:>2}. {labels[n - 1] if labels else o}")
    hint = "number" + (" or a new name" if allow_new else "")
    while True:
        v = ask(f"Choose ({hint}): ")
        if v.isdigit() and 1 <= int(v) <= len(options):
            return options[int(v) - 1]
        if v in options or (allow_new and v):
            return v
        print("  invalid choice")


def with_auth(client: CCClient, fn, *args):
    while True:
        try:
            return fn(*args)
        except AuthExpired as e:
            print(e)
            client.set_token(prompt_token())


def main() -> None:
    ap = argparse.ArgumentParser(
        description="Sync Firebase Remote Config between project/city")
    ap.add_argument(
        "--host", default="https://control-center.c2.moving.tech/analytics-data/api")
    ap.add_argument("--token", default=os.getenv("FB_TOKEN"))
    ap.add_argument("--source-project")
    ap.add_argument("--source-city")
    ap.add_argument("--target-project")
    ap.add_argument("--target-city")
    ap.add_argument("--dry-run", action="store_true",
                    help="validate only, never publish")
    ap.add_argument("--insecure", action="store_true",
                    help="skip TLS verification")
    a = ap.parse_args()

    client = CCClient(a.host, a.token or prompt_token(), a.insecure)
    projects = None
    if not (a.source_project and a.target_project):
        projects = with_auth(client, client.projects)

    sp = a.source_project or pick("Source project", projects)
    src = with_auth(client, client.get_config, sp)
    scounts = city_counts(src["template"])
    sc = a.source_city or pick(f"Source city in {sp}", list(scounts),
                               [f"{k}  ({v} values)" for k, v in scounts.items()])

    tp = a.target_project or pick("Target project", projects)
    tgt = src if tp == sp else with_auth(client, client.get_config, tp)
    tcounts = city_counts(tgt["template"])
    tc = a.target_city or pick(f"Target city in {tp}", list(tcounts),
                               [f"{k}  ({v} values)" for k, v in tcounts.items()], allow_new=True)
    if (sp, sc) == (tp, tc):
        raise SystemExit("Source and target are the same.")

    groups, skipped = build_groups(src["template"], sc, tgt["template"], tc)
    for s in skipped:
        print(f"[skip] {s}")
    if not groups:
        print("Nothing to sync — target already matches source.")
        return
    state = {"groups": groups, "skipped": skipped, "sp": sp, "sc": sc, "tp": tp, "tc": tc,
             "src_tpl": src["template"], "tgt_tpl": tgt["template"], "etag": tgt["etag"]}
    run_tui(state, client, a.dry_run)


if __name__ == "__main__":
    main()
