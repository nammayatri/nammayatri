'use strict';
// A stand-in for `docker exec ... psql` that runs the SQL on PGlite, so a shell
// script that talks to its database through docker can be run here for real,
// with no Docker and no server: the script is the real file, the database is a
// real Postgres, only the door between them is fake.
//
//   const fd = await require('./lib/fake-docker').start(db);   // db from lib/db.js
//   spawn('bash', ['x.sh'], { env: { ...process.env, PATH: fd.bin + ':' + process.env.PATH } })
//   await fd.stop();
//
// The server runs in the test's process (PGlite is one connection and cannot be
// shared); the `docker` on PATH is a two-line wrapper whose client posts argv
// and stdin to it. What it understands, and nothing more:
//   docker inspect -f ... NAME                       -> "true"
//   docker exec [-i] NAME psql -U u -d d [-v k=v]... [-X -q -A -t -At] [-F sep] (-c SQL | -f FILE | -f -)
//   docker cp FILE NAME:PATH       -> remembered; a later `psql -f PATH` reads it
//   docker exec NAME redis-cli (--scan --pattern P | DEL k... | DBSIZE)
//                                  -> an empty Redis; every call kept in `redis`
// A psql that fails rolls back, as psql's connection closing would: PGlite is
// one session that outlives the call, and an aborted BEGIN would poison the next.
// psql's `:'name'` is substituted as a quoted literal, as psql does; its output
// is the unaligned tuples-only form the scripts ask for. Anything else answers
// exit 127, so a script that starts using a new docker command fails loudly here.
const http = require('http');
const fs = require('fs');
const os = require('os');
const path = require('path');

function parsePsql(args, stdin, copied = new Map()) {
  const vars = {};
  let sep = '|';
  let sql = null;
  for (let i = 0; i < args.length; i += 1) {
    const a = args[i];
    if (a === '-U' || a === '-d') i += 1;
    else if (a === '-v') {
      const kv = args[(i += 1)];
      const eq = kv.indexOf('=');
      vars[kv.slice(0, eq)] = kv.slice(eq + 1);
    } else if (a === '-F') sep = args[(i += 1)];
    else if (a === '-c') sql = args[(i += 1)];
    else if (a === '-f') {
      const f = args[(i += 1)];
      sql = f === '-' ? stdin : copied.has(f) ? copied.get(f) : fs.readFileSync(f, 'utf8');
    }
    // -X -q -A -t -At and the like change only the output shape
  }
  if (sql === null) throw new Error('psql: no -c or -f');
  // psql substitutes :'name' as a quoted literal and a bare :name raw.
  sql = sql.replace(/:'([A-Za-z_]\w*)'/g, (m, k) => {
    if (!(k in vars)) return m;
    return `'${vars[k].replace(/'/g, "''")}'`;
  });
  // psql's own meta-commands are not SQL.
  sql = sql.split('\n').filter((l) => !/^\s*\\/.test(l)).join('\n');
  return { sql, sep };
}

async function handle(db, argv, stdin, copied, redis) {
  const [cmd, ...rest] = argv;
  if (cmd === 'inspect') return { code: 0, out: 'true\n' };
  if (cmd === 'cp') {
    const [src, dest] = rest;
    const at = dest.indexOf(':');
    copied.set(dest.slice(at + 1), fs.readFileSync(src, 'utf8'));
    return { code: 0, out: '' };
  }
  if (cmd === 'exec') {
    const a = rest.filter((x) => x !== '-i' && x !== '-t');
    const prog = a[1];
    if (prog === 'redis-cli') {
      redis.push(a.slice(2));
      const op = (a[2] || '').toUpperCase();
      if (op === 'DBSIZE') return { code: 0, out: '0\n' };
      if (op === 'DEL') return { code: 0, out: '0\n' };
      return { code: 0, out: '' }; // --scan: an empty Redis has no keys
    }
    if (prog !== 'psql') return { code: 127, out: '', err: `fake docker: exec ${prog} is not supported\n` };
    let parsed;
    try {
      parsed = parsePsql(a.slice(2), stdin, copied);
    } catch (e) {
      return { code: 2, out: '', err: `${e.message}\n` };
    }
    try {
      const results = await db.exec(parsed.sql);
      let out = '';
      for (const r of results) {
        if (!r.fields || r.fields.length === 0) continue;
        for (const row of r.rows) {
          out += `${r.fields.map((f) => (row[f.name] === null || row[f.name] === undefined ? '' : String(row[f.name]))).join(parsed.sep)}\n`;
        }
      }
      return { code: 0, out };
    } catch (e) {
      await db.exec('ROLLBACK').catch(() => {});
      return { code: 1, out: '', err: `ERROR:  ${e.message}\n` };
    }
  }
  return { code: 127, out: '', err: `fake docker: ${cmd} is not supported\n` };
}

async function start(db) {
  // One statement at a time: PGlite is one connection.
  let chain = Promise.resolve();
  const copied = new Map();
  const redis = [];
  const server = http.createServer((req, res) => {
    const chunks = [];
    req.on('data', (c) => chunks.push(c));
    req.on('end', () => {
      chain = chain.then(async () => {
        const { argv, stdin } = JSON.parse(Buffer.concat(chunks).toString('utf8'));
        const r = await handle(db, argv, stdin, copied, redis);
        res.end(JSON.stringify(r));
      }).catch((e) => { res.statusCode = 500; res.end(String(e)); });
    });
  });
  await new Promise((r) => server.listen(0, '127.0.0.1', r));
  const port = server.address().port;

  const bin = fs.mkdtempSync(path.join(os.tmpdir(), 'fake-docker-'));
  const client = path.join(__dirname, 'fake-docker-client.js');
  fs.writeFileSync(path.join(bin, 'docker'), `#!/bin/sh\nFAKE_DOCKER_PORT=${port} exec node "${client}" "$@"\n`, { mode: 0o755 });
  return {
    bin,
    redis,
    stop: () => new Promise((r) => { server.close(r); fs.rmSync(bin, { recursive: true, force: true }); }),
  };
}

module.exports = { start };
