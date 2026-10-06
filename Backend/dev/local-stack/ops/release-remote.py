#!/usr/bin/env python3
"""The server half of a release. Run by ops/deploy.sh, never by hand.

    release-remote.py plan     <release dir>   what would change, and nothing else
    release-remote.py apply    <release dir>   do it
    release-remote.py rollback                 put back what the last release replaced
    release-remote.py status                   what is deployed, and has anyone edited it since
    release-remote.py tidy                     archive the .bak / .before-* leftovers

Runs ON the VPS, as root, against /opt/ny/local-stack. Phase 3 of the backend
restructuring plan (2026-10-06).

── What a release may touch, and what it never does ───────────────────────────
Only files the repository ships: stack/ of one commit, listed with their hashes
in the release's MANIFEST. Everything else in /opt/ny/local-stack is the box's
own and is never read, written or deleted here: .env and the secrets, the
certificates, the website's build in edge-web/, the binaries, the map data,
the bot's state, the drivers' codes.

── The rules it keeps, each one paid for ──────────────────────────────────────
  * Files are written IN PLACE (same inode). nginx.conf and others are
    bind-mounted one by one; replacing the file instead of rewriting it leaves
    the container serving the old one while `nginx -t` passes (2026-09-23,
    33 minutes without document uploads).
  * A file edited by hand on the server since the last release is not
    overwritten: the release stops and names it. --force overrides, knowingly.
  * SQL is applied only when its content is new to this server. Re-running a
    tariff file would undo every fare changed since.
  * Only what changed is restarted, and nginx is tested before it is reloaded.
    A config that fails `nginx -t` is rolled back on the spot.
  * The previous version of everything replaced or removed is kept in
    /opt/ny/local-stack.prev, one named directory, for `rollback`.
"""
import datetime
import hashlib
import json
import os
import shutil
import subprocess
import sys
import urllib.request

# Overridable for the test in tests/release.test.sh, which runs a whole release
# against a copy of the server's layout with docker and systemd stubbed out.
STACK = os.environ.get('MOVIN_STACK', '/opt/ny/local-stack')
TEST = os.environ.get('MOVIN_RELEASE_TEST') == '1'
PREV = STACK + '.prev'
SHIPPED = os.path.join(STACK, '.shipped')
SHIPPED_FILES = os.path.join(STACK, '.shipped.files')
LEFTOVERS = os.environ.get('MOVIN_LEFTOVERS', '/root/snapshots')

# What a change under each path means for the running stack. The first match wins.
ACTIONS = [
    ('auth-guard/', 'restart ny-auth-guard'),
    ('maps-shim/', 'restart ny-maps-shim'),
    ('Dockerfile.maps-shim', 'rebuild maps-shim'),
    ('edge/nginx.conf', 'reload ny-edge'),
    ('edge/proxy-common.inc', 'reload ny-edge'),
    ('demo-map/nginx.conf', 'reload ny-map'),
    ('docker-compose.yml', 'compose up changed services'),
    ('simulate-driver.py', 'restart movin-fleet'),
    ('movin-bot.py', 'restart movin-bot'),
    ('backup.sh', 'note backup'),
    ('Dockerfile.rider', 'note image'),
]

GREEN, RED, YELLOW, BOLD, END = '\033[1;32m', '\033[1;31m', '\033[1;33m', '\033[1m', '\033[0m'


def say(msg):
    print(f'\n{BOLD}== {msg}{END}', flush=True)


def ok(msg):
    print(f'   {GREEN}ok  {END}{msg}', flush=True)


def warn(msg):
    print(f'   {YELLOW}!!  {END}{msg}', flush=True)


def bad(msg):
    print(f'   {RED}BAD {END}{msg}', flush=True)


def die(msg):
    bad(msg)
    sys.exit(1)


def sha(path):
    h = hashlib.sha256()
    with open(path, 'rb') as fh:
        for block in iter(lambda: fh.read(1 << 16), b''):
            h.update(block)
    return h.hexdigest()


def run(cmd, check=True, quiet=False):
    if TEST:
        print(f'       [test] {cmd}')
        out = '{"services": {}}' if 'config --format json' in cmd else ''
        return subprocess.CompletedProcess(cmd, 0, out, '')
    r = subprocess.run(cmd, shell=True, capture_output=True, text=True)
    if not quiet and r.stdout.strip():
        print('       ' + r.stdout.strip().replace('\n', '\n       '))
    if check and r.returncode != 0:
        die(f'{cmd}\n       {r.stderr.strip()[:600]}')
    return r


def read_manifest(path):
    """`sha256  mode  path` per line -> {path: (sha, mode)}."""
    out = {}
    if path and os.path.exists(path):
        for line in open(path, encoding='utf-8'):
            line = line.rstrip('\n')
            if not line:
                continue
            digest, mode, rel = line.split('  ', 2)
            out[rel] = (digest, int(mode, 8))
    return out


def live(rel):
    p = os.path.join(STACK, rel)
    return sha(p) if os.path.isfile(p) else None


def action_for(rel):
    for prefix, action in ACTIONS:
        if rel == prefix or (prefix.endswith('/') and rel.startswith(prefix)):
            return action
    return None


def compose_services():
    """The resolved config of each service, with the website's overlay (COMPOSE_FILE in .env)."""
    r = run(f'cd {STACK} && docker compose config --format json', quiet=True)
    return {k: json.dumps(v, sort_keys=True) for k, v in json.loads(r.stdout)['services'].items()}


def running():
    r = run("docker ps --format '{{.Names}}'", quiet=True)
    return set(r.stdout.split())


def http_ok(url):
    if TEST:
        return True
    try:
        with urllib.request.urlopen(url, timeout=5) as resp:
            return resp.status == 200
    except Exception:
        return False


# ── the plan ────────────────────────────────────────────────────────────────
def make_plan(rel_dir, force):
    new = read_manifest(os.path.join(rel_dir, 'MANIFEST'))
    if not new:
        die('the release has no MANIFEST')
    # What the last release shipped: the box's own record, or -- for the very
    # first release, before there was one -- the commit the box was proven
    # identical to (phase 1), passed by deploy.sh as PREVIOUS.
    prev = read_manifest(SHIPPED_FILES) or read_manifest(os.path.join(rel_dir, 'PREVIOUS'))
    plan = {'same': [], 'new': [], 'changed': [], 'remove': [], 'keep': [], 'conflict': []}
    for rel, (digest, _mode) in sorted(new.items()):
        have = live(rel)
        if have == digest:
            plan['same'].append(rel)
        elif have is None:
            plan['new'].append(rel)
        elif rel in prev and have == prev[rel][0]:
            plan['changed'].append(rel)
        else:
            # On the server and different from what was shipped -- or never
            # shipped at all. Either way someone's work is in it.
            plan['conflict'].append(rel)
    for rel, (digest, _mode) in sorted(prev.items()):
        if rel in new:
            continue
        have = live(rel)
        if have is None:
            continue
        (plan['remove'] if have == digest else plan['keep']).append(rel)

    old_contents = {d for d, _m in prev.values()}
    plan['sql'] = [r for r in plan['new'] + plan['changed']
                   if r.startswith('db/') and r.endswith('.sql') and new[r][0] not in old_contents]
    plan['actions'] = sorted({a for r in plan['new'] + plan['changed'] + plan['remove']
                              if (a := action_for(r))})
    plan['manifest'] = new
    plan['force'] = force
    return plan


def show(plan, info):
    say(f"release {info.get('commit', '?')[:10]}  ({info.get('subject', '')})")
    print(f"   {len(plan['manifest'])} files in the release: "
          f"{len(plan['same'])} already identical, {len(plan['new'])} new, "
          f"{len(plan['changed'])} changed, {len(plan['remove'])} to remove")
    for key, label in (('new', 'new'), ('changed', 'changed'), ('remove', 'remove')):
        for rel in plan[key]:
            print(f'       {label:8} {rel}')
    for rel in plan['keep']:
        warn(f'{rel}: no longer shipped, but edited on the server since -- left in place')
    for rel in plan['conflict']:
        bad(f'{rel}: different on the server from what was last shipped -- edited by hand?')
    say('then')
    if plan['sql']:
        for rel in plan['sql']:
            print(f'       apply SQL  {rel}')
    else:
        ok('no SQL to apply (none is new to this server)')
    if plan['actions']:
        for a in plan['actions']:
            print(f'       {a}')
    else:
        ok('nothing to restart: no file a running service reads has changed')


# ── apply ───────────────────────────────────────────────────────────────────
def owner_of(path):
    while not os.path.exists(path):
        path = os.path.dirname(path)
    st = os.stat(path)
    return st.st_uid, st.st_gid


def write_in_place(src, dst, mode):
    """Rewrite dst through its existing inode, or create it. Never rename over it."""
    existed = os.path.exists(dst)
    if not existed:
        parent = os.path.dirname(dst)
        if not os.path.isdir(parent):
            uid, gid = owner_of(parent)
            os.makedirs(parent)
            os.chown(parent, uid, gid)
        uid, gid = owner_of(parent)
    with open(src, 'rb') as fin, open(dst, 'r+b' if existed else 'wb') as fout:
        data = fin.read()
        fout.seek(0)
        fout.write(data)
        fout.truncate()
    os.chmod(dst, mode)
    if not existed:
        os.chown(dst, uid, gid)


def do_actions(actions, before_cfg):
    for a in actions:
        if a == 'reload ny-edge' or a == 'reload ny-map':
            c = a.split()[1]
            if run(f'docker exec {c} nginx -t', check=False, quiet=True).returncode != 0:
                return f'{c}: nginx -t failed'
            run(f'docker exec {c} nginx -s reload')
            ok(f'{c}: config tested, reloaded')
        elif a.startswith('restart ny-'):
            c = a.split()[1]
            run(f'docker restart {c}', quiet=True)
            ok(f'{c} restarted')
        elif a == 'rebuild maps-shim':
            run(f'cd {STACK} && docker compose build maps-shim && docker compose up -d --no-deps maps-shim')
            ok('maps-shim rebuilt and recreated')
        elif a == 'compose up changed services':
            after = compose_services()
            changed = sorted(s for s in after if after.get(s) != before_cfg.get(s))
            if changed:
                run(f"cd {STACK} && docker compose up -d --no-deps {' '.join(changed)}")
                ok(f"recreated: {', '.join(changed)}")
            else:
                ok('docker-compose.yml changed, but no service\'s resolved config did: nothing recreated')
        elif a.startswith('restart movin-'):
            unit = a.split()[1]
            if run(f'systemctl is-active --quiet {unit}', check=False).returncode == 0:
                run(f'systemctl restart {unit}')
                ok(f'{unit} restarted')
            else:
                ok(f'{unit} is not running here; nothing to restart')
        elif a == 'note backup':
            warn('backup.sh changed -- movin-backup.service still runs /root/backup.sh '
                 '(until phase 4): copy it there by hand if the change should be live')
        elif a == 'note image':
            warn('Dockerfile.rider changed -- the backend image is built by CI, not here')
    return None


def checks(before_running, manifest):
    say('checks')
    failed = []
    gone = sorted(before_running - running())
    if gone:
        failed.append(f'containers that were running and are not: {", ".join(gone)}')
    else:
        ok(f'{len(before_running)} containers still running')
    for name, url in (('auth guard', 'http://127.0.0.1:8031/healthz'),
                      ('maps shim', 'http://127.0.0.1:8030/healthz')):
        (ok(f'{name} healthz 200') if http_ok(url) else failed.append(f'{name} healthz not 200'))
    if run('docker exec ny-edge nginx -t', check=False, quiet=True).returncode == 0:
        ok('nginx -t')
    else:
        failed.append('nginx -t fails')
    differ = [r for r, (d, _m) in manifest.items() if live(r) != d]
    if differ:
        failed.append(f'{len(differ)} files do not match the release: {", ".join(differ[:5])}')
    else:
        ok(f'every one of the {len(manifest)} files matches the release, by sha256')
    for f in failed:
        bad(f)
    return not failed


def apply(rel_dir):
    info = json.load(open(os.path.join(rel_dir, 'INFO.json')))
    plan = make_plan(rel_dir, '--force' in sys.argv)
    show(plan, info)
    if plan['conflict'] and not plan['force']:
        die('stopping: a file above was changed on the server since it was shipped. '
            'Bring that change into git first, or re-run with --force to overwrite it.')

    before_running = running()
    before_cfg = compose_services()

    say(f'keeping the previous version in {PREV}')
    if os.path.exists(PREV):
        shutil.rmtree(PREV)
    os.makedirs(PREV, mode=0o700)
    for rel in plan['changed'] + plan['conflict'] + plan['remove']:
        dst = os.path.join(PREV, 'files', rel)
        os.makedirs(os.path.dirname(dst), exist_ok=True)
        shutil.copy2(os.path.join(STACK, rel), dst)
    for f in (SHIPPED, SHIPPED_FILES):
        if os.path.exists(f):
            shutil.copy2(f, os.path.join(PREV, os.path.basename(f)))
    json.dump({'commit': info.get('commit'), 'new': plan['new'],
               'changed': plan['changed'] + plan['conflict'], 'removed': plan['remove'],
               'actions': plan['actions'],
               'new_hashes': {r: plan['manifest'][r][0] for r in plan['new']}},
              open(os.path.join(PREV, 'RELEASE.json'), 'w'), indent=1)
    ok(f"{len(plan['changed']) + len(plan['conflict']) + len(plan['remove'])} files saved")

    say('writing')
    src_root = os.path.join(rel_dir, 'stack')
    for rel in plan['new'] + plan['changed'] + plan['conflict']:
        write_in_place(os.path.join(src_root, rel), os.path.join(STACK, rel), plan['manifest'][rel][1])
    for rel in plan['remove']:
        os.remove(os.path.join(STACK, rel))
    ok(f"{len(plan['new'])} new, {len(plan['changed']) + len(plan['conflict'])} rewritten in place, "
       f"{len(plan['remove'])} removed")

    for rel in plan['sql']:
        say(f'applying {rel}')
        base = os.path.basename(rel)
        run(f'docker cp {STACK}/{rel} ny-postgres:/tmp/{base}', quiet=True)
        r = run(f'docker exec ny-postgres psql -U postgres -d atlas_dev -v ON_ERROR_STOP=1 -q '
                f'-f /tmp/{base}', check=False)
        run(f'docker exec ny-postgres rm -f /tmp/{base}', check=False, quiet=True)
        if r.returncode != 0:
            bad(r.stderr.strip()[:800])
            die(f'{rel} failed. Files are written; `ops/deploy.sh rollback` puts them back '
                '(the database is as the SQL left it -- check it).')
        ok(rel)

    if plan['actions']:
        say('restarting only what changed')
        problem = do_actions(plan['actions'], before_cfg)
        if problem:
            bad(problem)
            say('rolling back on the spot')
            rollback()
            die('release rolled back')

    good = checks(before_running, plan['manifest'])

    stamp = datetime.datetime.now(datetime.timezone.utc).strftime('%Y-%m-%d %H:%M:%S UTC')
    with open(SHIPPED, 'w') as fh:
        fh.write(f"commit    {info['commit']}\n"
                 f"branch    {info.get('branch', '')}\n"
                 f"subject   {info.get('subject', '')}\n"
                 f"released  {stamp}\n"
                 f"by        {info.get('by', '')}\n"
                 f"files     {len(plan['manifest'])}  (sha256 of each in .shipped.files)\n"
                 f"changed   {len(plan['new'])} new, {len(plan['changed']) + len(plan['conflict'])} "
                 f"rewritten, {len(plan['remove'])} removed\n"
                 f"checks    {'passed' if good else 'FAILED -- see the release output'}\n"
                 f"previous  {PREV}  (ops/deploy.sh rollback)\n")
    shutil.copyfile(os.path.join(rel_dir, 'MANIFEST'), SHIPPED_FILES)
    say('.shipped')
    print(open(SHIPPED).read())
    if not good:
        die('the release is written but a check failed -- read above; `ops/deploy.sh rollback` undoes it')
    print(f'{GREEN}released{END}')


# ── rollback ────────────────────────────────────────────────────────────────
def rollback():
    rec_path = os.path.join(PREV, 'RELEASE.json')
    if not os.path.exists(rec_path):
        die(f'nothing to roll back: {PREV} holds no release')
    rec = json.load(open(rec_path))
    before_running = running()
    before_cfg = compose_services()
    say(f"rolling back release {str(rec.get('commit'))[:10]}")
    for rel in rec['changed'] + rec['removed']:
        write_in_place(os.path.join(PREV, 'files', rel), os.path.join(STACK, rel),
                       os.stat(os.path.join(PREV, 'files', rel)).st_mode & 0o7777)
    for rel in rec['new']:
        p = os.path.join(STACK, rel)
        if os.path.exists(p) and sha(p) == rec['new_hashes'].get(rel):
            os.remove(p)
        elif os.path.exists(p):
            warn(f'{rel} was edited since the release -- left in place')
    for f in (SHIPPED, SHIPPED_FILES):
        saved = os.path.join(PREV, os.path.basename(f))
        if os.path.exists(saved):
            shutil.copyfile(saved, f)
        elif os.path.exists(f):
            os.remove(f)
    ok(f"{len(rec['changed']) + len(rec['removed'])} files restored, {len(rec['new'])} new ones removed")
    if rec['actions']:
        problem = do_actions(rec['actions'], before_cfg)
        if problem:
            bad(problem)
    os.rename(PREV, f"{PREV}-rolled-back-{datetime.datetime.now():%Y%m%d-%H%M%S}")
    ok('rolled back; the previous version is the live one again')
    gone = sorted(before_running - running())
    (bad(f'not running: {", ".join(gone)}') if gone else ok('containers all running'))


# ── status ──────────────────────────────────────────────────────────────────
def status():
    if not os.path.exists(SHIPPED):
        die('no .shipped: nothing has been released by ops/deploy.sh on this server yet')
    say('what is deployed')
    print(open(SHIPPED).read())
    shipped = read_manifest(SHIPPED_FILES)
    edited = [r for r, (d, _m) in shipped.items() if live(r) != d]
    say('has anything been edited on the server since?')
    if edited:
        for r in edited:
            warn(f'{r} differs from what was shipped')
    else:
        ok(f'no: all {len(shipped)} shipped files are exactly as released')


# ── tidy ────────────────────────────────────────────────────────────────────
def tidy():
    """Archive the copies past in-place edits left beside the files they backed up."""
    import re
    dest = os.path.join(LEFTOVERS, f'leftovers-{datetime.date.today():%Y-%m-%d}')
    pattern = re.compile(r'\.(bak|before|broken)(\b|-|\d|$)')
    found = []
    for base, dirs, files in os.walk(STACK):
        dirs[:] = [d for d in dirs if d not in ('2023', 'bin', 'tiles-config', 'logs', 'backups',
                                                 'edge-certs', 'edge-web', 'edge-webroot')]
        for f in files:
            if pattern.search(f):
                found.append(os.path.relpath(os.path.join(base, f), STACK))
    if not found:
        ok('no leftover copies')
        return
    os.makedirs(dest, mode=0o700, exist_ok=True)
    for rel in sorted(found):
        target = os.path.join(dest, rel)
        os.makedirs(os.path.dirname(target), exist_ok=True)
        shutil.move(os.path.join(STACK, rel), target)
        print(f'       {rel}')
    run(f'chmod -R go-rwx {dest}', quiet=True)
    ok(f'{len(found)} leftover copies moved to {dest} (root only)')


def main():
    if os.geteuid() != 0 and not TEST:
        die('run as root, on the server')
    mode = sys.argv[1] if len(sys.argv) > 1 else ''
    if mode == 'plan':
        info = json.load(open(os.path.join(sys.argv[2], 'INFO.json')))
        plan = make_plan(sys.argv[2], False)
        show(plan, info)
        if plan['conflict']:
            bad('a real release would STOP here (see above)')
        print('\n   dry run: nothing was written')
    elif mode == 'apply':
        apply(sys.argv[2])
    elif mode == 'rollback':
        rollback()
    elif mode == 'status':
        status()
    elif mode == 'tidy':
        tidy()
    else:
        die('usage: release-remote.py plan|apply <release dir> | rollback | status | tidy')


if __name__ == '__main__':
    main()
