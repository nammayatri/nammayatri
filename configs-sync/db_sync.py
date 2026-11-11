#!/usr/bin/env python3
from __future__ import annotations

import argparse
import datetime as dt
import json
import os
import re
import readline  # noqa: F401
import ssl
import threading
import time
import urllib.error
import urllib.request
import uuid
from dataclasses import dataclass, field
from decimal import Decimal, InvalidOperation
from pathlib import Path
from typing import Any, Callable

UA = "db-sync/1.0"
DEFAULT_CONFIG = Path(__file__).with_name("db_sync_config.json")
POLL_SECONDS = 0.4
POLL_TIMEOUT_SECONDS = 180
TERMINAL_STATUSES = ("completed", "failed", "cancelled")
TOTAL_COLUMN = "__db_sync_total"
ADD, UPDATE, DELETE, DUP = "ADD", "UPDATE", "DELETE", "DUP"
DISPLAY_ORDER = {ADD: 0, UPDATE: 1, DELETE: 2, DUP: 3}
APPLY_ORDER = {DELETE: 0, UPDATE: 1, ADD: 2}
ZONES = (("green", "GREEN   safe to add / update", "bold green"),
         ("yellow", "YELLOW  tick rows by hand after checking flagged columns", "bold yellow"),
         ("red", "RED     do not sync across environments", "bold red"))
TICK_BY_ZONE = {"green": (ADD, UPDATE), "yellow": (), "red": ()}
AUTH_MSG = ("Not logged in / cookie expired. Paste a fresh Cookie header for {host} "
            "(DevTools → Network → any db-manager request → Request Headers → cookie).")


class AuthExpired(Exception):
    pass


class RoleDenied(Exception):
    def __init__(self, message: str, can_request: bool):
        super().__init__(message)
        self.can_request = can_request


class PasswordRequired(Exception):
    pass


class DBError(Exception):
    pass


class ConnectionLost(DBError):
    pass


def normalize_cookie(cookie: str) -> str:
    cookie = cookie.strip()
    if cookie.lower().startswith("cookie:"):
        cookie = cookie[7:].strip()
    return cookie.strip("'\"")


class DBClient:
    def __init__(self, host: str, mode: str, read_mode: str | None = None,
                 cookie: str | None = None, insecure: bool = False):
        self.host = host.rstrip("/")
        self.mode = mode
        self.read_mode = read_mode or mode
        self.cookie = cookie
        self.ctx = ssl._create_unverified_context() if insecure else None

    def set_cookie(self, cookie: str) -> None:
        self.cookie = normalize_cookie(cookie)

    def auth_message(self) -> str:
        return AUTH_MSG.format(host=self.host)

    def _req(self, method: str, path: str, body: Any = None) -> Any:
        headers = {"Content-Type": "application/json", "Accept": "application/json",
                   "User-Agent": UA, "Origin": self.host, "Referer": self.host + "/",
                   "Cookie": self.cookie or ""}
        data = json.dumps(body).encode() if body is not None else None
        req = urllib.request.Request(self.host + path, data=data, method=method, headers=headers)
        try:
            with urllib.request.urlopen(req, context=self.ctx, timeout=90) as r:
                raw = r.read().decode(errors="replace")
        except urllib.error.HTTPError as e:
            self._raise_http(e)
        except urllib.error.URLError as e:
            raise ConnectionLost(f"Cannot reach {self.host}: {e.reason}") from None
        except OSError as e:
            raise ConnectionLost(f"Connection to {self.host} failed: {e}") from None
        try:
            return json.loads(raw)
        except json.JSONDecodeError:
            raise AuthExpired(self.auth_message()) from None

    def _raise_http(self, e: urllib.error.HTTPError) -> None:
        raw = e.read().decode(errors="replace")
        try:
            payload = json.loads(raw)
        except json.JSONDecodeError:
            payload = None
        if e.code == 401 or (300 <= e.code < 400) or payload is None and e.code == 403:
            raise AuthExpired(self.auth_message()) from None
        if not isinstance(payload, dict):
            raise DBError(f"HTTP {e.code}: {raw[:500]}") from None
        message = payload.get("message") or payload.get("error") or raw[:500]
        if payload.get("code") == "ROLE_NOT_PERMITTED":
            raise RoleDenied(message, bool(payload.get("canRequestApproval"))) from None
        if "password" in str(message).lower():
            raise PasswordRequired(message) from None
        raise DBError(f"HTTP {e.code}: {message}") from None

    def validate(self, query: str) -> None:
        resp = self._req("POST", "/api/query/validate", {"query": query})
        if not resp.get("valid"):
            raise DBError(f"Invalid query: {resp.get('error') or resp}")

    def submit(self, query: str, database: str, schema: str, mode: str,
               password: str | None = None) -> str:
        body = {"query": query, "database": database, "mode": mode,
                "pgSchema": schema, "continueOnError": False}
        if password:
            body["password"] = password
        return self._req("POST", "/api/query/execute", body)["executionId"]

    def wait(self, execution_id: str) -> dict:
        deadline = time.monotonic() + POLL_TIMEOUT_SECONDS
        while True:
            status = self._req("GET", f"/api/query/status/{execution_id}")
            if status.get("status") in TERMINAL_STATUSES:
                return status
            if time.monotonic() > deadline:
                raise ConnectionLost(f"Timed out waiting for execution {execution_id}")
            time.sleep(POLL_SECONDS)

    def create_request(self, reason: str, items: list[dict]) -> str:
        return self._req("POST", "/api/query-requests",
                         {"reason": reason, "items": items})["groupId"]

    def request_link(self, group_id: str) -> str:
        return f"{self.host}/?request={group_id}"


def cloud_results(status: dict) -> dict[str, dict]:
    result = status.get("result") or {}
    return {k: v for k, v in result.items() if isinstance(v, dict) and "success" in v}


def status_error(status: dict) -> str:
    errors = [status.get("error")] if status.get("error") else []
    for cloud, res in cloud_results(status).items():
        if not res.get("success"):
            errors.append(f"{cloud}: {res.get('error') or res}")
    return "; ".join(errors) or f"execution {status.get('status')}"


def statement_result(cloud: dict) -> dict:
    if "result" in cloud:
        return cloud["result"]
    return cloud["results"][-1]["result"]


def read_rows(client: DBClient, auth: Callable, sql: str, database: str, schema: str) -> list[dict]:
    auth(client, client.validate, sql)
    execution_id = auth(client, client.submit, sql, database, schema, client.read_mode)
    status = auth(client, client.wait, execution_id)
    if status.get("status") != "completed":
        raise DBError(status_error(status))
    cloud = cloud_results(status).get(client.read_mode)
    if not cloud or not cloud.get("success"):
        raise DBError(status_error(status))
    return statement_result(cloud)["rows"]


def ensure_success(status: dict) -> None:
    clouds = cloud_results(status)
    if status.get("status") != "completed" or not clouds or not all(c.get("success") for c in clouds.values()):
        raise DBError(status_error(status))


def q_ident(name: str) -> str:
    return '"' + name.replace('"', '""') + '"'


def q_table(schema: str, table: str) -> str:
    return f"{q_ident(schema)}.{q_ident(table)}"


def q_text(value: str) -> str:
    return "'" + value.replace("'", "''") + "'"


def atomic_block(statements: list[tuple[str, str]]) -> str:
    tag = f"$db_sync_{uuid.uuid4().hex[:12]}$"
    body = []
    for n, (label, sql) in enumerate(statements, 1):
        name = f"statement {n} ({label})".replace("'", "''").replace("%", "%%")
        body.append(f"  {sql}\n  GET DIAGNOSTICS affected = ROW_COUNT;\n"
                    f"  IF affected <> 1 THEN RAISE EXCEPTION 'db_sync {name} affected % rows', affected; END IF;")
    joined = "\n".join(body)
    return f"DO {tag}\nDECLARE affected bigint;\nBEGIN\n{joined}\nEND\n{tag};"


@dataclass
class Column:
    name: str
    type: str
    not_null: bool
    has_default: bool
    generated: bool


META_SQL = """select a.attname as name, format_type(a.atttypid, a.atttypmod) as type,
a.attnotnull::text as not_null, (d.adrelid is not null)::text as has_default,
(a.attgenerated <> '')::text as generated
from pg_attribute a
join pg_class c on c.oid = a.attrelid
join pg_namespace n on n.oid = c.relnamespace
left join pg_attrdef d on d.adrelid = a.attrelid and d.adnum = a.attnum
where n.nspname = {schema} and c.relname = {table} and a.attnum > 0 and not a.attisdropped
order by a.attnum limit 5000"""


def fetch_meta(client: DBClient, auth: Callable, database: str, schema: str, table: str) -> dict[str, Column]:
    rows = read_rows(client, auth, META_SQL.format(schema=q_text(schema), table=q_text(table)), database, schema)
    if not rows:
        raise DBError(f"Table {schema}.{table} does not exist on {client.host}")
    return {r["name"]: Column(r["name"], r["type"], r["not_null"] == "true",
                              r["has_default"] == "true", r["generated"] == "true") for r in rows}


def select_sql(schema: str, table: str, meta: dict[str, Column], scope_col: str | None,
               scope_val: str | None, where: str | None, max_rows: int) -> str:
    cols = ", ".join(f"t.{q_ident(c)}::text as {q_ident(c)}" for c in meta)
    cols += f", (count(*) over ())::text as {q_ident(TOTAL_COLUMN)}"
    filters = []
    if scope_col:
        filters.append(f"t.{q_ident(scope_col)}::text = {q_text(scope_val or '')}")
    if where:
        filters.append(f"({where})")
    clause = f" where {' and '.join(filters)}" if filters else ""
    return f"select {cols} from {q_table(schema, table)} t{clause} limit {max_rows + 1}"


def fetch_rows(client: DBClient, auth: Callable, database: str, schema: str, table: str,
               meta: dict[str, Column], scope_col: str | None, scope_val: str | None,
               where: str | None, max_rows: int) -> list[dict]:
    rows = read_rows(client, auth, select_sql(schema, table, meta, scope_col, scope_val, where, max_rows),
                     database, schema)
    total = int(rows[0][TOTAL_COLUMN]) if rows else 0
    if total > max_rows:
        raise DBError(f"{table} has {total} matching rows on {client.host}, over maxRows {max_rows}; "
                      "raise maxRows or add a where filter for this table")
    if len(rows) != total:
        raise DBError(f"{table}: db-manager returned {len(rows)} of {total} rows on {client.host}; "
                      "refusing to diff a partial result")
    for row in rows:
        del row[TOTAL_COLUMN]
    return rows


def norm(value: str | None, col_type: str) -> Any:
    if value is None:
        return None
    if col_type.startswith("json"):
        try:
            return json.dumps(json.loads(value), sort_keys=True, separators=(",", ":"))
        except json.JSONDecodeError:
            return value
    if col_type.startswith(("numeric", "double precision", "real")):
        try:
            return str(Decimal(value).normalize())
        except InvalidOperation:
            return value
    return value


def literal(value: str | None, col_type: str) -> str:
    return "NULL" if value is None else f"{q_text(value)}::{col_type}"


@dataclass
class Cell:
    col: str
    src: str | None
    tgt: str | None
    value: str | None
    write: bool
    kind: str
    warn: bool = False
    edited: bool = False
    locked: bool = False


@dataclass
class Row:
    op: str
    key: dict[str, str | None]
    src: dict | None
    tgt: dict | None
    cells: list[Cell] = field(default_factory=list)
    checked: bool = False

    def label(self) -> str:
        return ", ".join(f"{k}={v}" for k, v in self.key.items()) or "(single row)"

    def changed(self) -> list[str]:
        return [c.col for c in self.cells if c.kind in ("changed", "keep") and self.op == UPDATE]

    def written(self) -> list[Cell]:
        return [c for c in self.cells if c.write]

    def can_tick(self) -> bool:
        return self.op != DUP


@dataclass
class ScopeContext:
    column: str | None
    src_value: str | None
    tgt_value: str | None
    rewrite: dict[str, str | None]
    env_differs: bool

    @property
    def differs(self) -> bool:
        return self.src_value != self.tgt_value


@dataclass
class TableRules:
    dimensions: list[str]
    ignore: set[str]
    keep: set[str]
    keep_patterns: list[re.Pattern]

    def is_keep(self, col: str) -> bool:
        return col in self.keep or any(p.search(col) for p in self.keep_patterns)


def utc_now_literal() -> str:
    return dt.datetime.now(dt.timezone.utc).strftime("%Y-%m-%d %H:%M:%S+00")


def is_ref(col: str) -> bool:
    return col.endswith("_id") or col.endswith("_ids")


def row_key(row: dict, dims: list[str], meta: dict[str, Column]) -> tuple:
    return tuple(norm(row.get(d), meta[d].type) for d in dims)


def update_cells(src: dict, tgt: dict, smeta: dict[str, Column], tmeta: dict[str, Column],
                 rules: TableRules, scope: ScopeContext) -> list[Cell]:
    cells = []
    for name, col in tmeta.items():
        if col.generated:
            continue
        sv = src.get(name) if name in smeta else None
        tv = tgt.get(name)
        if name == scope.column or name in scope.rewrite:
            cells.append(Cell(name, sv, tv, tv, False, "target-owned", locked=True))
        elif name in rules.dimensions:
            cells.append(Cell(name, sv, tv, tv, False, "dimension", locked=True))
        elif name not in smeta:
            cells.append(Cell(name, None, tv, tv, False, "target-only", locked=True))
        elif name == "id" or name in rules.ignore:
            cells.append(Cell(name, sv, tv, sv, False, "ignored"))
        elif norm(sv, smeta[name].type) == norm(tv, col.type):
            cells.append(Cell(name, sv, tv, sv, False, "same"))
        elif rules.is_keep(name):
            cells.append(Cell(name, sv, tv, sv, False, "keep"))
        elif is_ref(name) and (scope.differs or scope.env_differs):
            cells.append(Cell(name, sv, tv, sv, False, "changed", warn=True))
        else:
            cells.append(Cell(name, sv, tv, sv, True, "changed"))
    return cells


def add_cells(src: dict, smeta: dict[str, Column], tmeta: dict[str, Column],
              rules: TableRules, scope: ScopeContext) -> list[Cell]:
    cells = []
    now = utc_now_literal()
    for name, col in tmeta.items():
        if col.generated:
            continue
        sv = src.get(name) if name in smeta else None
        if name == scope.column:
            cells.append(Cell(name, sv, None, scope.tgt_value, True, "target-owned"))
        elif name in scope.rewrite:
            cells.append(Cell(name, sv, None, scope.rewrite[name], True, "target-owned"))
        elif name == "id" and scope.differs and not col.has_default:
            cells.append(Cell(name, sv, None, str(uuid.uuid4()), True, "new id"))
        elif name == "id" and scope.differs:
            cells.append(Cell(name, sv, None, None, False, "db default"))
        elif name in rules.ignore and "timestamp" in col.type:
            cells.append(Cell(name, sv, None, now, True, "now"))
        elif name not in smeta:
            cells.append(Cell(name, None, None, None, False, "target-only"))
        elif rules.is_keep(name) and name not in rules.dimensions:
            required = col.not_null and not col.has_default
            cells.append(Cell(name, sv, None, sv, required, "keep", warn=required))
        else:
            warn = is_ref(name) and (scope.differs or scope.env_differs) and sv is not None
            cells.append(Cell(name, sv, None, sv, True, "added", warn=warn))
    return cells


def delete_cells(tgt: dict, tmeta: dict[str, Column]) -> list[Cell]:
    return [Cell(n, None, tgt.get(n), None, False, "removed", locked=True)
            for n, c in tmeta.items() if not c.generated]


def duplicate_keys(rows: list[dict], dims: list[str], meta: dict[str, Column]) -> tuple[dict, set]:
    index: dict[tuple, dict] = {}
    dups = set()
    for row in rows:
        key = row_key(row, dims, meta)
        if key in index:
            dups.add(key)
        index[key] = row
    return index, dups


def compute_diff(srows: list[dict], trows: list[dict], smeta: dict[str, Column], tmeta: dict[str, Column],
                 rules: TableRules, scope: ScopeContext) -> list[Row]:
    dims = rules.dimensions
    sindex, sdups = duplicate_keys(srows, dims, smeta)
    tindex, tdups = duplicate_keys(trows, dims, tmeta)
    rows: list[Row] = []
    for key in sorted(set(sindex) | set(tindex), key=lambda k: tuple(str(x) for x in k)):
        src, tgt = sindex.get(key), tindex.get(key)
        label = dict(zip(dims, ((src or tgt).get(d) for d in dims)))
        if key in sdups or key in tdups:
            rows.append(Row(DUP, label, src, tgt))
        elif src and not tgt:
            cells = add_cells(src, smeta, tmeta, rules, scope)
            rows.append(Row(ADD, label, src, None, cells, True))
        elif tgt and not src:
            rows.append(Row(DELETE, label, None, tgt, delete_cells(tgt, tmeta), False))
        else:
            cells = update_cells(src, tgt, smeta, tmeta, rules, scope)
            if any(c.kind in ("changed", "keep") for c in cells):
                rows.append(Row(UPDATE, label, src, tgt, cells, any(c.write for c in cells)))
    rows.sort(key=lambda r: DISPLAY_ORDER[r.op])
    return rows


def row_where(row: Row, tmeta: dict[str, Column], rules: TableRules, scope: ScopeContext) -> str:
    tgt = row.tgt or {}
    if "id" in tmeta and tgt.get("id") is not None:
        parts = [f"{q_ident('id')} = {literal(tgt['id'], tmeta['id'].type)}"]
    else:
        parts = [f"{q_ident(d)} IS NOT DISTINCT FROM {literal(tgt.get(d), tmeta[d].type)}" for d in rules.dimensions]
    if scope.column:
        parts.append(f"{q_ident(scope.column)}::text = {q_text(scope.tgt_value or '')}")
    if not parts:
        raise DBError(f"Refusing to write {row.label()}: no id, dimensions or scope to target one row")
    return " AND ".join(parts)


def statement_for(row: Row, schema: str, table: str, tmeta: dict[str, Column],
                  rules: TableRules, scope: ScopeContext) -> str | None:
    target = q_table(schema, table)
    if row.op == DELETE:
        return f"DELETE FROM {target} WHERE {row_where(row, tmeta, rules, scope)};"
    written = row.written()
    if row.op == ADD:
        cols = ", ".join(q_ident(c.col) for c in written)
        vals = ", ".join(literal(c.value, tmeta[c.col].type) for c in written)
        return f"INSERT INTO {target} ({cols}) VALUES ({vals});"
    if row.op != UPDATE or not written:
        return None
    sets = [f"{q_ident(c.col)} = {literal(c.value, tmeta[c.col].type)}" for c in written]
    if "updated_at" in tmeta and not any(c.col == "updated_at" for c in written):
        sets.append(f"{q_ident('updated_at')} = {literal(utc_now_literal(), tmeta['updated_at'].type)}")
    return f"UPDATE {target} SET {', '.join(sets)} WHERE {row_where(row, tmeta, rules, scope)};"


@dataclass
class Side:
    env_name: str
    client: DBClient
    database: str
    schema: str
    option: dict


@dataclass
class TableDiff:
    name: str
    rules: TableRules
    scope: ScopeContext
    smeta: dict[str, Column]
    tmeta: dict[str, Column]
    rows: list[Row]
    notes: list[str]
    max_rows: int
    where: str | None

    def checked(self) -> list[Row]:
        return [r for r in self.rows if r.checked]

    def summary(self) -> str:
        count = lambda op: sum(r.op == op for r in self.rows)
        dup = f" !{count(DUP)}" if count(DUP) else ""
        return f"+{count(ADD)} ~{count(UPDATE)} -{count(DELETE)}{dup} ✓{len(self.checked())}"


@dataclass
class Context:
    config: dict
    config_path: Path
    app_name: str
    source: Side
    target: Side

    @property
    def defaults(self) -> dict:
        return self.config.get("defaults", {})

    @property
    def tables(self) -> dict:
        return self.config["apps"][self.app_name]["tables"]

    def zone(self, table: str) -> str:
        return self.tables[table].get("zone", "yellow")

    def save_dimensions(self, table: str, dims: list[str]) -> None:
        self.tables[table]["dimensions"] = dims
        self.config_path.write_text(json.dumps(self.config, indent=2) + "\n")


def scope_context(ctx: Context, conf: dict, smeta: dict[str, Column], tmeta: dict[str, Column]) -> ScopeContext:
    column = conf.get("scope")
    env_differs = ctx.source.env_name != ctx.target.env_name
    if not column:
        return ScopeContext(None, None, None, {}, env_differs)
    if column not in smeta or column not in tmeta:
        raise DBError(f"scope column {column} is missing on source or target")
    field_name = ctx.config["scope"]["filters"][column]
    rewrite = {}
    for col, opt_field in ctx.defaults.get("rewrite", {}).items():
        applies = column == "merchant_operating_city_id" or opt_field.startswith("merchant")
        if (col != column and col in tmeta and applies
                and ctx.source.option.get(opt_field) != ctx.target.option.get(opt_field)):
            rewrite[col] = ctx.target.option.get(opt_field)
    return ScopeContext(column, ctx.source.option.get(field_name), ctx.target.option.get(field_name),
                        rewrite, env_differs)


def load_table(ctx: Context, name: str, auth: Callable) -> TableDiff:
    conf = ctx.tables[name]
    src, tgt = ctx.source, ctx.target
    smeta = fetch_meta(src.client, auth, src.database, src.schema, name)
    tmeta = fetch_meta(tgt.client, auth, tgt.database, tgt.schema, name)
    scope = scope_context(ctx, conf, smeta, tmeta)
    notes = []
    dims = conf.get("dimensions")
    if dims is None:
        dims = ["id"] if "id" in smeta and "id" in tmeta else []
        if scope.differs:
            notes.append("No dimensions configured: matching on id, so rows will not pair across "
                         "cities. Press d to set the columns that identify a row.")
    missing = [d for d in dims if d not in smeta or d not in tmeta]
    if missing:
        raise DBError(f"dimension columns missing on source or target: {', '.join(missing)}")
    defaults = ctx.defaults
    rules = TableRules(list(dims), set(defaults.get("ignore", [])) | set(conf.get("ignore", [])),
                       set(conf.get("keep", [])),
                       [re.compile(p) for p in defaults.get("keepPatterns", [])])
    only_src = sorted(set(smeta) - set(tmeta))
    if only_src:
        notes.append(f"Source-only columns (skipped): {', '.join(only_src)}")
    max_rows = int(conf.get("maxRows", defaults.get("maxRows", 5000)))
    where = conf.get("where")
    if not scope.env_differs and not scope.differs:
        notes.append("Source and target are the same rows for this table (same environment and scope).")
        return TableDiff(name, rules, scope, smeta, tmeta, [], notes, max_rows, where)
    srows = fetch_rows(src.client, auth, src.database, src.schema, name, smeta,
                       scope.column, scope.src_value, where, max_rows)
    trows = fetch_rows(tgt.client, auth, tgt.database, tgt.schema, name, tmeta,
                       scope.column, scope.tgt_value, where, max_rows)
    rows = compute_diff(srows, trows, smeta, tmeta, rules, scope)
    allowed = TICK_BY_ZONE.get(ctx.zone(name), ())
    for row in rows:
        row.checked = row.checked and row.op in allowed
    if any(r.op == DUP for r in rows):
        notes.append("Rows marked DUP share dimension values and cannot be applied. "
                     "Press d to add columns that make them unique.")
    return TableDiff(name, rules, scope, smeta, tmeta, rows, notes, max_rows, where)


def stale_rows(ctx: Context, diff: TableDiff, auth: Callable) -> list[str]:
    tgt = ctx.target
    current = fetch_rows(tgt.client, auth, tgt.database, tgt.schema, diff.name, diff.tmeta,
                         diff.scope.column, diff.scope.tgt_value, diff.where, diff.max_rows)
    index, _ = duplicate_keys(current, diff.rules.dimensions, diff.tmeta)
    stale = []
    for row in diff.checked():
        key = tuple(norm(row.key[d], diff.tmeta[d].type) for d in diff.rules.dimensions)
        if index.get(key) != row.tgt:
            stale.append(row.label())
    return stale


def short(value: Any, limit: int = 48) -> str:
    text = "∅" if value is None else str(value).replace("\n", " ")
    return text if len(text) <= limit else text[: limit - 1] + "…"


def run_tui(ctx: Context, dry_run: bool) -> None:
    from rich.text import Text
    from textual.app import App, ComposeResult
    from textual.binding import Binding
    from textual.containers import Horizontal, Vertical, VerticalScroll
    from textual.screen import ModalScreen
    from textual.widgets import (Button, DataTable, Footer, Header, Input, Label,
                                 OptionList, RichLog, Static, TextArea)
    from textual.widgets.option_list import Option

    src, tgt = ctx.source, ctx.target
    scope_label = ctx.config["scope"].get("labelField", "city")
    src_name = f"{src.env_name}/{src.option.get(scope_label)}"
    tgt_name = f"{tgt.env_name}/{tgt.option.get(scope_label)}"
    protected = bool(ctx.config["environments"][tgt.env_name].get("protected"))

    class PromptScreen(ModalScreen[Any]):
        DEFAULT_CSS = """
        PromptScreen { align: center middle; }
        #box { width: 90%; height: auto; border: thick $warning; background: $surface; padding: 1; }
        """
        BINDINGS = [Binding("escape", "cancel", "Cancel")]

        def __init__(self, message: str, hidden: bool = False, initial: str = ""):
            super().__init__()
            self.message, self.hidden, self.initial = message, hidden, initial

        def compose(self) -> ComposeResult:
            with Vertical(id="box"):
                yield Static(self.message, markup=False)
                yield Label("enter save · esc cancel")
                yield Input(value=self.initial, password=self.hidden, id="value")

        def on_input_submitted(self, ev: Input.Submitted) -> None:
            self.dismiss(ev.value)

        def action_cancel(self) -> None:
            self.dismiss(None)

    class EditScreen(ModalScreen[Any]):
        DEFAULT_CSS = """
        EditScreen { align: center middle; }
        #box { width: 90%; height: 80%; border: thick $accent; background: $surface; padding: 1; }
        #ta { height: 1fr; }
        """
        BINDINGS = [Binding("ctrl+s", "save", "Save"), Binding("ctrl+n", "set_null", "Set NULL"),
                    Binding("escape", "cancel", "Cancel")]

        def __init__(self, cell: Cell):
            super().__init__()
            self.cell = cell

        def compose(self) -> ComposeResult:
            with Vertical(id="box"):
                yield Label(Text(f"{self.cell.col} · ctrl+s save · ctrl+n set NULL · esc cancel"))
                yield TextArea(self.cell.value or "", id="ta")

        def action_save(self) -> None:
            self.dismiss({"value": self.query_one("#ta", TextArea).text})

        def action_set_null(self) -> None:
            self.dismiss({"value": None})

        def action_cancel(self) -> None:
            self.dismiss(None)

    class RowScreen(ModalScreen[None]):
        DEFAULT_CSS = """
        RowScreen { align: center middle; }
        #box { width: 96%; height: 90%; border: thick $accent; background: $surface; padding: 1; }
        #cols { height: 2fr; }
        #full { height: 1fr; border-top: solid $accent; overflow-y: auto; }
        """
        BINDINGS = [Binding("space", "toggle", "Write / keep target"), Binding("e", "edit", "Edit value"),
                    Binding("escape", "close", "Back")]

        def __init__(self, row: Row):
            super().__init__()
            self.row = row
            rank = {"changed": 0, "added": 0, "keep": 1, "new id": 2, "target-owned": 2, "now": 3}
            self.cells = sorted(row.cells, key=lambda c: (not c.write, rank.get(c.kind, 5)))

        def compose(self) -> ComposeResult:
            with Vertical(id="box"):
                yield Label(Text(f"{self.row.op}  {self.row.label()}  ·  unticked = keep target value"))
                yield DataTable(id="cols", cursor_type="row", zebra_stripes=True)
                yield VerticalScroll(Static("", id="full", markup=False))
                yield Footer()

        def on_mount(self) -> None:
            t = self.query_one("#cols", DataTable)
            t.add_columns(" ", "column", "kind", "source", "target now", "will write")
            for n, cell in enumerate(self.cells):
                t.add_row(*self._line(cell), key=str(n))
            t.focus()
            if self.cells:
                self._detail(0)

        def _line(self, cell: Cell):
            tick = Text("✓", style="bold green") if cell.write else Text(" ")
            kind = cell.kind + ("*" if cell.edited else "") + (" ⚠" if cell.warn else "")
            will = short(cell.value, 34) if cell.write else "(keep)" if self.row.op == UPDATE else "(skip)"
            return tick, Text(cell.col), Text(kind), Text(short(cell.src, 34)), Text(short(cell.tgt, 34)), Text(will)

        def _refresh_line(self, n: int) -> None:
            t = self.query_one("#cols", DataTable)
            for col, val in zip(t.columns.keys(), self._line(self.cells[n])):
                t.update_cell(str(n), col, val)
            self._detail(n)

        def _detail(self, n: int) -> None:
            c = self.cells[n]
            warn = ("\n⚠ Looks like a reference to another row. Its id may not exist in the target; "
                    "tick only after checking.") if c.warn else ""
            self.query_one("#full", Static).update(
                f"{c.col}{warn}\nsource: {c.src}\ntarget: {c.tgt}\nwrite : {c.value}")

        def on_data_table_row_highlighted(self, ev: DataTable.RowHighlighted) -> None:
            if ev.cursor_row is not None and ev.cursor_row < len(self.cells):
                self._detail(ev.cursor_row)

        def action_toggle(self) -> None:
            n = self.query_one("#cols", DataTable).cursor_row
            cell = self.cells[n]
            if cell.locked:
                self.notify(f"{cell.col} is fixed for this operation", severity="warning")
                return
            cell.write = not cell.write
            self._refresh_line(n)

        def action_edit(self) -> None:
            n = self.query_one("#cols", DataTable).cursor_row
            cell = self.cells[n]
            if cell.locked:
                self.notify(f"{cell.col} is fixed for this operation", severity="warning")
                return

            def done(result: Any) -> None:
                if result is None:
                    return
                cell.value, cell.write, cell.edited = result["value"], True, True
                self._refresh_line(n)

            self.app.push_screen(EditScreen(cell), done)

        def action_close(self) -> None:
            if self.row.op == UPDATE and not self.row.written():
                self.row.checked = False
            elif self.row.op == UPDATE:
                self.row.checked = True
            self.dismiss(None)

    class ConfirmScreen(ModalScreen[bool]):
        DEFAULT_CSS = """
        ConfirmScreen { align: center middle; }
        #box { width: 96%; height: 90%; border: thick $warning; background: $surface; padding: 1; }
        #summary { height: 1fr; }
        #btns { height: 3; align: center middle; }
        """
        BINDINGS = [Binding("escape", "cancel", "Cancel")]

        def __init__(self, text: str, require: str | None):
            super().__init__()
            self.text, self.require = text, require

        def compose(self) -> ComposeResult:
            with Vertical(id="box"):
                yield VerticalScroll(Static(self.text, markup=False), id="summary")
                if self.require:
                    yield Label(Text(f"Target is protected. Type {self.require} to enable Apply:"))
                    yield Input(id="confirm")
                with Horizontal(id="btns"):
                    yield Button("Validate only (dry run)" if dry_run else "Apply",
                                 variant="warning", id="yes", disabled=bool(self.require))
                    yield Button("Cancel", id="no")

        def on_input_changed(self, ev: Input.Changed) -> None:
            self.query_one("#yes", Button).disabled = ev.value != self.require

        def on_button_pressed(self, ev: Button.Pressed) -> None:
            self.dismiss(ev.button.id == "yes")

        def action_cancel(self) -> None:
            self.dismiss(False)

    class SyncApp(App):
        TITLE = f"DB config sync  {src_name}  →  {tgt_name}"
        CSS = """
        #left { width: 44; border-right: solid $accent; }
        #tables { height: 1fr; }
        #right { width: 1fr; }
        #rows { height: 2fr; }
        #detail { height: 1fr; border-top: solid $accent; padding: 0 1; overflow-y: auto; }
        #log { height: 8; border-top: solid $accent; display: none; }
        """
        BINDINGS = [
            Binding("space", "toggle", "Tick"),
            Binding("a", "toggle_all", "Tick all"),
            Binding("o", "open_row", "Columns"),
            Binding("d", "dimensions", "Dimensions"),
            Binding("l", "load_all", "Load green+yellow"),
            Binding("r", "reload", "Reload"),
            Binding("k", "cookie", "Cookie"),
            Binding("p", "apply", "Apply table"),
            Binding("P", "apply_all", "Apply all"),
            Binding("slash", "filter", "Filter"),
            Binding("q", "quit", "Quit"),
        ]

        def __init__(self):
            super().__init__()
            self.diffs: dict[str, TableDiff] = {}
            self.errors: dict[str, str] = {}
            self.current: str | None = None
            self.names = sorted(ctx.tables)
            self.busy = False

        def compose(self) -> ComposeResult:
            yield Header()
            with Horizontal():
                with Vertical(id="left"):
                    yield Input(placeholder="filter tables", id="filter")
                    yield OptionList(id="tables")
                with Vertical(id="right"):
                    yield DataTable(id="rows", cursor_type="row", zebra_stripes=True)
                    yield Static("Select a table and press enter.", id="detail", markup=False)
            yield RichLog(id="log", markup=False, wrap=True)
            yield Footer()

        def on_mount(self) -> None:
            t = self.query_one("#rows", DataTable)
            t.add_columns(" ", "op", "row", "changed columns")
            self._fill_tables("")
            self.query_one("#tables", OptionList).focus()
            self._update_title()

        def _table_label(self, name: str) -> Text:
            if name in self.errors:
                return Text(f"{name}  ! error", style="red")
            diff = self.diffs.get(name)
            return Text(f"{name}  {diff.summary()}" if diff else name)

        def _fill_tables(self, needle: str) -> None:
            options = self.query_one("#tables", OptionList)
            options.clear_options()
            for zone, title, style in ZONES:
                names = [n for n in self.names if needle.lower() in n.lower() and ctx.zone(n) == zone]
                if not names:
                    continue
                options.add_option(Option(Text(title, style=style), disabled=True))
                for name in names:
                    options.add_option(Option(self._table_label(name), id=name))

        def _refresh_table_label(self, name: str) -> None:
            options = self.query_one("#tables", OptionList)
            try:
                options.replace_option_prompt(name, self._table_label(name))
            except Exception:  # noqa: BLE001
                pass

        def _update_title(self) -> None:
            total = sum(len(d.checked()) for d in self.diffs.values())
            tables = sum(bool(d.checked()) for d in self.diffs.values())
            self.sub_title = f"{total} rows ticked in {tables} tables{' · DRY RUN' if dry_run else ''}"

        def _log(self, line: str) -> None:
            log = self.query_one("#log", RichLog)
            log.display = True
            log.write(line)

        def on_input_changed(self, ev: Input.Changed) -> None:
            if ev.input.id == "filter":
                self._fill_tables(ev.value)

        def on_input_submitted(self, ev: Input.Submitted) -> None:
            if ev.input.id == "filter":
                self.query_one("#tables", OptionList).focus()

        def on_option_list_option_selected(self, ev: OptionList.OptionSelected) -> None:
            if ev.option.id in self.diffs:
                self._after_load(ev.option.id)
            else:
                self._start_load([ev.option.id], show=ev.option.id)

        def _ask(self, message: str, hidden: bool = False) -> str | None:
            done = threading.Event()
            box: dict[str, Any] = {}

            def finish(value: Any) -> None:
                box["value"] = value
                done.set()

            self.call_from_thread(self.push_screen, PromptScreen(message, hidden), finish)
            done.wait()
            return box.get("value")

        def _auth(self, client: DBClient, fn: Callable, *args: Any) -> Any:
            while True:
                try:
                    return fn(*args)
                except AuthExpired as e:
                    cookie = self._ask(str(e), hidden=True)
                    if not cookie or not normalize_cookie(cookie):
                        raise DBError("Cookie refresh cancelled") from None
                    client.set_cookie(cookie)

        def _start(self, work: Callable[[], None]) -> None:
            if self.busy:
                self.notify("Still working on the previous action", severity="warning")
                return
            self.busy = True

            def runner() -> None:
                try:
                    work()
                except Exception as e:  # noqa: BLE001
                    self.call_from_thread(self._log, f"[FAIL] {type(e).__name__}: {e}")
                finally:
                    self.busy = False

            self.run_worker(runner, thread=True, exit_on_error=False)

        def _start_load(self, names: list[str], show: str | None = None) -> None:
            def work() -> None:
                total = len(names)
                for n, name in enumerate(names, 1):
                    self.call_from_thread(setattr, self, "sub_title", f"loading {name} ({n}/{total})")
                    self._load_one(name)
                self.call_from_thread(self._after_load, show)

            self._start(work)

        def _load_one(self, name: str) -> None:
            try:
                diff = load_table(ctx, name, self._auth)
                self.diffs[name] = diff
                self.errors.pop(name, None)
            except (DBError, RoleDenied, PasswordRequired, re.error) as e:
                self.diffs.pop(name, None)
                self.errors[name] = str(e)
                self.call_from_thread(self._log, f"[{name}] {e}")
            self.call_from_thread(self._refresh_table_label, name)

        def _after_load(self, show: str | None) -> None:
            self._update_title()
            if show:
                self.current = show
                self._render_rows()
                self.query_one("#rows", DataTable).focus()
            elif self.current:
                self._render_rows()

        def _rows(self) -> list[Row]:
            diff = self.diffs.get(self.current or "")
            return diff.rows if diff else []

        def _cells(self, row: Row):
            tick = Text("✓", style="bold green") if row.checked else Text(" ")
            style = {ADD: "green", UPDATE: "yellow", DELETE: "red", DUP: "magenta"}[row.op]
            cols = ", ".join(row.changed()) if row.op == UPDATE else ""
            return tick, Text(row.op, style=style), Text(short(row.label(), 60)), Text(short(cols, 70))

        def _render_rows(self) -> None:
            t = self.query_one("#rows", DataTable)
            t.clear()
            for n, row in enumerate(self._rows()):
                t.add_row(*self._cells(row), key=str(n))
            self._show_detail(0)

        def _refresh_row(self, n: int) -> None:
            t = self.query_one("#rows", DataTable)
            for col, val in zip(t.columns.keys(), self._cells(self._rows()[n])):
                t.update_cell(str(n), col, val)
            if self.current:
                self._refresh_table_label(self.current)
            self._update_title()

        def _show_detail(self, n: int) -> None:
            detail = self.query_one("#detail", Static)
            name = self.current or ""
            if name in self.errors:
                detail.update(f"{name}: {self.errors[name]}")
                return
            diff = self.diffs.get(name)
            if not diff:
                detail.update("Select a table and press enter.")
                return
            notes = "\n".join(diff.notes)
            head = (f"{name}  zone: {ctx.zone(name)}  "
                    f"dimensions: {diff.rules.dimensions or '(single row)'}  scope: {diff.scope.column}")
            if not diff.rows:
                detail.update(f"{head}\nNo differences.\n{notes}")
                return
            row = diff.rows[min(n, len(diff.rows) - 1)]
            lines = [head, f"{row.op} {row.label()}"]
            for cell in row.cells:
                if cell.write or cell.kind in ("changed", "keep"):
                    mark = "✓" if cell.write else " "
                    lines.append(f"{mark} {cell.col}: {short(cell.tgt, 60)} → {short(cell.value, 60)}"
                                 f"{'  ⚠' if cell.warn else ''}")
            detail.update("\n".join(lines + ([notes] if notes else [])))

        def on_data_table_row_highlighted(self, ev: DataTable.RowHighlighted) -> None:
            if ev.data_table.id == "rows" and ev.cursor_row is not None and ev.cursor_row < len(self._rows()):
                self._show_detail(ev.cursor_row)

        def on_data_table_row_selected(self, ev: DataTable.RowSelected) -> None:
            if ev.data_table.id == "rows":
                self.action_open_row()

        def action_filter(self) -> None:
            self.query_one("#filter", Input).focus()

        def action_toggle(self) -> None:
            rows = self._rows()
            t = self.query_one("#rows", DataTable)
            if not rows or not t.has_focus:
                return
            n = t.cursor_row
            row = rows[n]
            if not row.can_tick():
                self.notify("Duplicate key rows cannot be applied; press d to fix", severity="warning")
                return
            if row.op == UPDATE and not row.written():
                self.notify("No columns selected; press o and tick a column", severity="warning")
                return
            row.checked = not row.checked
            self._refresh_row(n)

        def action_toggle_all(self) -> None:
            allowed = TICK_BY_ZONE.get(ctx.zone(self.current), ()) if self.current else ()
            selectable = [(n, r) for n, r in enumerate(self._rows())
                          if r.op in allowed and (r.op == ADD or r.written())]
            if not selectable:
                self.notify(f"Nothing to tick in a {ctx.zone(self.current)} table; use space per row",
                            severity="warning")
                return
            value = not all(r.checked for _, r in selectable)
            for n, row in selectable:
                row.checked = value
                self._refresh_row(n)

        def action_open_row(self) -> None:
            rows = self._rows()
            t = self.query_one("#rows", DataTable)
            if not rows or not t.has_focus:
                return
            n = t.cursor_row
            if rows[n].op == DUP:
                self.notify("Duplicate key rows have no column view; press d to fix", severity="warning")
                return

            def done(_: Any) -> None:
                self._refresh_row(n)
                self._show_detail(n)

            self.push_screen(RowScreen(rows[n]), done)

        def action_dimensions(self) -> None:
            name = self.current
            if not name:
                self.notify("Open a table first", severity="warning")
                return
            diff = self.diffs.get(name)
            columns = ", ".join(diff.tmeta) if diff else ""
            current = ctx.tables[name].get("dimensions") or []

            def done(value: Any) -> None:
                if value is None:
                    return
                dims = [c.strip() for c in value.split(",") if c.strip()]
                ctx.save_dimensions(name, dims)
                self._start_load([name], show=name)

            self.push_screen(PromptScreen(
                f"Comma-separated columns that identify a row in {name}. Saved to the config file. "
                f"Leave empty for a single row per scope.\nColumns: {columns}",
                initial=", ".join(current)), done)

        def action_load_all(self) -> None:
            self._start_load([n for n in self.names if ctx.zone(n) != "red"])

        def action_reload(self) -> None:
            if self.current:
                self._start_load([self.current], show=self.current)

        def action_cookie(self) -> None:
            clients = {id(s.client): s for s in (src, tgt)}

            def done_for(client: DBClient):
                def done(value: Any) -> None:
                    if value and normalize_cookie(value):
                        client.set_cookie(value)
                        self.notify(f"Cookie updated for {client.host}")
                return done

            for side in clients.values():
                self.push_screen(PromptScreen(f"Cookie for {side.env_name} ({side.client.host})", hidden=True),
                                 done_for(side.client))

        def _plan(self, names: list[str]) -> list[tuple[TableDiff, list[tuple[Row, str]]]]:
            plan = []
            for name in names:
                diff = self.diffs.get(name)
                if not diff:
                    continue
                items = []
                for row in sorted(diff.checked(), key=lambda r: APPLY_ORDER.get(r.op, 9)):
                    sql = statement_for(row, tgt.schema, name, diff.tmeta, diff.rules, diff.scope)
                    if sql:
                        items.append((row, sql))
                if items:
                    plan.append((diff, items))
            return plan

        def _confirm_apply(self, names: list[str], show_sql: bool) -> None:
            try:
                plan = self._plan(names)
            except DBError as e:
                self.notify(str(e), severity="error")
                return
            if not plan:
                self.notify("Nothing ticked", severity="warning")
                return
            lines = [f"Apply to {tgt_name} on {tgt.client.host}  (from {src_name})",
                     f"{len(plan)} tables, one atomic block per table, run one after another; "
                     "stops at the first table that fails.", ""]
            for diff, items in plan:
                lines.append(f"-- {diff.name} ({len(items)} statements)")
                warned = [f"{r.label()}: {c.col}" for r, _ in items for c in r.written() if c.warn]
                lines.extend(f"-- ⚠ reference column copied: {w}" for w in warned)
                if show_sql:
                    lines.extend(short(sql, 300) for _, sql in items)
                else:
                    lines.extend(f"   {r.op} {short(r.label(), 80)}" for r, _ in items)
                lines.append("")

            def done(ok: bool) -> None:
                if ok:
                    self._start(lambda: self._run_apply(plan))

            self.push_screen(ConfirmScreen("\n".join(lines), tgt.env_name if protected and not dry_run else None), done)

        def action_apply(self) -> None:
            if not self.current:
                self.notify("Open a table first", severity="warning")
                return
            self._confirm_apply([self.current], show_sql=True)

        def action_apply_all(self) -> None:
            self._confirm_apply(self.names, show_sql=False)

        def _execute(self, sql: str) -> None:
            client = tgt.client
            password = None
            while True:
                try:
                    execution_id = self._auth(client, client.submit, sql, tgt.database, tgt.schema,
                                              client.mode, password)
                    break
                except PasswordRequired as e:
                    password = self._ask(f"{e}\nEnter your db-manager password:", hidden=True)
                    if not password:
                        raise DBError("Password prompt cancelled") from None
            ensure_success(self._auth(client, client.wait, execution_id))

        def _request_approval(self, reason_hint: str, sql: str) -> None:
            client = tgt.client
            reason = self._ask(f"{reason_hint}\nEnter a reason to submit this change for approval "
                               "(esc to stop without submitting):")
            if not reason or not reason.strip():
                self.call_from_thread(self._log, "[stopped] approval request not submitted; nothing applied")
                return
            items = [{"query": sql, "database": tgt.database, "mode": client.mode, "pgSchema": tgt.schema}]
            group = self._auth(client, client.create_request, reason.strip(), items)
            self.call_from_thread(self._log, "[pending approval] not applied until approved: "
                                             f"{client.request_link(group)}")

        def _apply_table(self, diff: TableDiff, items: list[tuple[Row, str]]) -> bool:
            log = lambda line: self.call_from_thread(self._log, line)
            client = tgt.client
            statements = [(f"{r.op} {diff.name}", sql) for r, sql in items]
            block = atomic_block(statements)
            try:
                self._auth(client, client.validate, block)
                stale = stale_rows(ctx, diff, self._auth)
                if stale:
                    raise DBError(f"{diff.name}: target changed since it was loaded ({'; '.join(stale[:5])}). "
                                  "Press r to reload; nothing applied for this table.")
                if dry_run:
                    log(f"[dry run] {diff.name}: {len(statements)} statements validated, nothing executed")
                    return True
                try:
                    self._execute(block)
                except RoleDenied as e:
                    if e.can_request:
                        self._request_approval(str(e), block)
                    else:
                        log(f"[FAIL] {diff.name}: {e}; nothing applied")
                    return False
                except ConnectionLost as e:
                    log(f"[UNKNOWN] {diff.name}: {e}. The change may or may not have applied. "
                        "Check db-manager history and press r before retrying.")
                    return False
                except DBError as e:
                    log(f"[FAIL] {diff.name}: rolled back, nothing applied: {e}")
                    return False
            except (DBError, RoleDenied, PasswordRequired) as e:
                log(f"[FAIL] {e}")
                return False
            log(f"[done] {diff.name}: applied {len(statements)} statements atomically")
            self._load_one(diff.name)
            return True

        def _run_apply(self, plan: list) -> None:
            for n, (diff, items) in enumerate(plan, 1):
                self.call_from_thread(setattr, self, "sub_title", f"applying {diff.name} ({n}/{len(plan)})")
                if not self._apply_table(diff, items):
                    self.call_from_thread(self._log, f"[stopped] {len(plan) - n} remaining tables not applied")
                    break
            self.call_from_thread(self._after_load, self.current)

    SyncApp().run()


def ask(prompt: str) -> str:
    try:
        return input(prompt).strip()
    except (EOFError, KeyboardInterrupt):
        raise SystemExit("\nAborted.")


def pick(title: str, options: list[str], labels: list[str] | None = None) -> str:
    print(f"\n{title}")
    for n, option in enumerate(options, 1):
        print(f"  {n:>2}. {labels[n - 1] if labels else option}")
    while True:
        value = ask("Choose (number): ")
        if value.isdigit() and 1 <= int(value) <= len(options):
            return options[int(value) - 1]
        if value in options:
            return value
        print("  invalid choice")


def cli_auth(client: DBClient, fn: Callable, *args: Any) -> Any:
    while True:
        try:
            return fn(*args)
        except AuthExpired as e:
            print(e)
            client.set_cookie(ask(f"Cookie for {client.host}: "))


def pick_option(side_name: str, client: DBClient, config: dict, database: str, schema: str,
                wanted: str | None) -> dict:
    scope = config["scope"]
    rows = read_rows(client, cli_auth, scope["optionsSql"].format(schema=schema), database, schema)
    if not rows:
        raise SystemExit(f"No {scope['name']} options found on {client.host}")
    labels = [" / ".join(str(r.get(f)) for f in scope["displayFields"]) for r in rows]
    if wanted:
        matches = [r for r, label in zip(rows, labels) if wanted in (r.get(scope["valueField"]), label)]
        if len(matches) != 1:
            raise SystemExit(f"{side_name} {scope['name']} '{wanted}' matched {len(matches)} rows; "
                             "pass the id or the full label")
        return matches[0]
    values = [str(n) for n in range(len(rows))]
    chosen = pick(f"{side_name} {scope['name']}", values, labels)
    return rows[int(chosen)]


def make_client(config: dict, env_name: str, cookie: str | None, insecure: bool,
                clients: dict[str, DBClient]) -> DBClient:
    env = config["environments"][env_name]
    host = env["host"].rstrip("/")
    mode = env.get("mode", "gcp")
    existing = clients.get(host)
    if existing:
        return existing
    cookie = normalize_cookie(cookie or ask(f"Cookie for {env_name} ({host}): "))
    if not cookie:
        raise SystemExit("No cookie provided.")
    clients[host] = DBClient(host, mode, env.get("readMode"), cookie, insecure)
    return clients[host]


def main() -> None:
    ap = argparse.ArgumentParser(
        description="Review and apply config table differences between two db-manager environments and scopes.")
    ap.add_argument("--config", type=Path, default=DEFAULT_CONFIG)
    ap.add_argument("--app", help="app key from the config, e.g. driver or rider")
    ap.add_argument("--source-env")
    ap.add_argument("--target-env")
    ap.add_argument("--source-scope", help="scope id or full label, e.g. a merchant_operating_city id")
    ap.add_argument("--target-scope")
    ap.add_argument("--source-cookie", default=os.getenv("DB_SYNC_SOURCE_COOKIE"))
    ap.add_argument("--target-cookie", default=os.getenv("DB_SYNC_TARGET_COOKIE"))
    ap.add_argument("--dry-run", action="store_true", help="validate statements, never execute them")
    ap.add_argument("--insecure", action="store_true", help="skip TLS verification")
    a = ap.parse_args()

    config = json.loads(a.config.read_text())
    apps = list(config["apps"])
    usable = [n for n, e in config["environments"].items() if e.get("host")]
    if not usable:
        raise SystemExit(f"Set a host for at least one environment in {a.config}")
    app_name = a.app or pick("App", apps)
    if app_name not in apps:
        raise SystemExit(f"Unknown app {app_name}; expected one of {apps}")
    source_env = a.source_env or pick("Source environment", usable)
    target_env = a.target_env or pick("Target environment", usable)
    for name in (source_env, target_env):
        if name not in usable:
            raise SystemExit(f"Environment {name} has no host in {a.config}; available: {usable}")

    clients: dict[str, DBClient] = {}
    app = config["apps"][app_name]
    schema = app["schema"]
    source_db = config["environments"][source_env].get("databases", {}).get(app_name, app["database"])
    target_db = config["environments"][target_env].get("databases", {}).get(app_name, app["database"])
    source_client = make_client(config, source_env, a.source_cookie, a.insecure, clients)
    target_client = make_client(config, target_env, a.target_cookie, a.insecure, clients)
    source_option = pick_option("Source", source_client, config, source_db, schema, a.source_scope)
    target_option = pick_option("Target", target_client, config, target_db, schema, a.target_scope)
    value_field = config["scope"]["valueField"]
    if source_env == target_env and source_option.get(value_field) == target_option.get(value_field):
        raise SystemExit("Source and target are the same environment and scope.")

    ctx = Context(config, a.config, app_name,
                  Side(source_env, source_client, source_db, schema, source_option),
                  Side(target_env, target_client, target_db, schema, target_option))
    run_tui(ctx, a.dry_run)


if __name__ == "__main__":
    main()
