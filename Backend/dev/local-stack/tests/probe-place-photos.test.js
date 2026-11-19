'use strict';
// investigations/probe-place-photos.sh -- the live check after the day-4
// release -- run here for real before anyone runs it on the server:
//
//   db    against the real table (PGlite, through lib/fake-docker.js)
//   shim  against the real maps-shim, as a process (server.js)
//
//   node tests/probe-place-photos.test.js
//
// Its `edge` part needs the server's nginx and is not run here; where each
// address goes is tests/edge-places.test.js. Then the probe is broken on
// purpose twice -- a table without its cascade, a shim that is not there --
// to prove it says BAD and exits 1 when it should.
const fs = require('fs');
const os = require('os');
const path = require('path');
const { spawn } = require('child_process');
const { open } = require('./lib/db');
const fakes = require('./lib/fakes');
const fakeDocker = require('./lib/fake-docker');

const PROBE = path.join(__dirname, '..', 'investigations', 'probe-place-photos.sh');
const SHIM = path.join(__dirname, '..', 'stack', 'maps-shim', 'server.js');
const PRELOAD = path.join(__dirname, 'lib', 'fake-pg-preload.js');
const PORT = 8094;

const run = (env) => new Promise((resolve) => {
  const p = spawn('bash', [PROBE], { env: { ...process.env, ...env }, stdio: ['ignore', 'pipe', 'pipe'] });
  let out = '';
  p.stdout.on('data', (c) => { out += c; });
  p.stderr.on('data', (c) => { out += c; });
  p.on('close', (code) => resolve({ code, out }));
});

(async () => {
  const t = fakes.checker();
  const db = await open();
  const fd = await fakeDocker.start(db);

  const pgLog = path.join(fs.mkdtempSync(path.join(os.tmpdir(), 'probe-photos-')), 'pg.jsonl');
  fs.writeFileSync(pgLog, '');
  const shim = spawn(process.execPath, ['-r', PRELOAD, SHIM], {
    cwd: path.dirname(SHIM),
    env: {
      PATH: process.env.PATH, PORT: String(PORT), PG_URL: 'postgres://fake', FAKE_PG_LOG: pgLog,
      OSRM_URL: 'http://127.0.0.1:9', MOCK_GOOGLE_URL: 'http://127.0.0.1:9',
      REDIS_HOST: '127.0.0.1', REDIS_PORT: '9', DRIVER_URL: 'http://127.0.0.1:9', RIDER_URL: 'http://127.0.0.1:9',
      WALLET_SWEEP_MS: '3600000', RESTRICTED_REFRESH_MS: '3600000',
    },
    stdio: ['ignore', 'ignore', 'ignore'],
  });
  for (let i = 0; i < 60; i += 1) {
    const up = await fetch(`http://127.0.0.1:${PORT}/healthz`).then((r) => r.ok, () => false);
    if (up) break;
    await new Promise((r) => setTimeout(r, 100));
  }
  const env = { PATH: `${fd.bin}:${process.env.PATH}`, PROBE_PARTS: 'db shim', SHIM: `http://127.0.0.1:${PORT}` };

  t.section('1. the probe on a database and a shim as the release leaves them');
  let r = await run(env);
  const lines = r.out.split('\n').filter((l) => /^\s+(ok|BAD)\s/.test(l));
  t.ok(r.code === 0 && /ALL PASSED \(11\)/.test(r.out), 'exit 0, ALL PASSED (11): 5 about the table, 6 about the shim', r.out);
  t.ok(lines.every((l) => /^\s+ok\s/.test(l)) && lines.length === 11, 'every line ok', lines.filter((l) => /BAD/.test(l)).join('\n'));
  t.ok(/photos stored so far: 0/.test(r.out), 'and it says how many photos are stored (0)');

  t.section('2. it can fail');
  await db.exec(`ALTER TABLE movin.suggested_place_photo DROP CONSTRAINT suggested_place_photo_suggested_place_id_fkey;
                 ALTER TABLE movin.suggested_place_photo ADD CONSTRAINT suggested_place_photo_suggested_place_id_fkey
                   FOREIGN KEY (suggested_place_id) REFERENCES movin.suggested_place (id)`);
  r = await run({ ...env, PROBE_PARTS: 'db' });
  t.ok(r.code === 1 && /BAD\s+deleted with its place/.test(r.out) && /FAILED \(1 of 5\)/.test(r.out),
    'a table whose photos would outlive their place: BAD, FAILED (1 of 5), exit 1', r.out);
  r = await run({ ...env, PROBE_PARTS: 'shim', SHIM: 'http://127.0.0.1:9' });
  t.ok(r.code === 1 && /FAILED \(6 of 6\)/.test(r.out) && /got: 000/.test(r.out),
    'no shim answering: every shim line BAD (000), exit 1', r.out.split('\n').slice(-3).join(' '));

  shim.kill();
  await fd.stop();
  await db.close();
  t.finish();
})().catch((e) => { console.error(e); process.exit(1); });
