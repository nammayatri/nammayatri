'use strict';
// stack/launch-cleanup.sh, the real script, run against a real Postgres
// (PGlite) through a stand-in `docker` (lib/fake-docker.js). 2026-10-18.
//
//   cd Backend/dev/local-stack/tests && npm ci && node launch-cleanup.test.js
//
// What must hold for a script that erases people on the eve of a launch:
//   * it names exactly the twelve cars and two test passengers, and the list
//     is the simulator's own roster, not a copy that can drift;
//   * a real person is never selected, even one whose number looks like a
//     test number in the other country or on the other side;
//   * a dry run writes nothing; the rehearsal erases those fourteen and no one
//     else, leaves an audit row each, and is idempotent;
//   * it refuses, changing nothing, when the selection is not what it was
//     written for or when a target has papers on a volume SQL cannot reach.
const fs = require('fs');
const os = require('os');
const path = require('path');
const { spawn } = require('child_process');
const { open } = require('./lib/db');
const fakes = require('./lib/fakes');
const fakeDocker = require('./lib/fake-docker');

const STACK = path.join(__dirname, '..', 'stack');
const SCRIPT = path.join(STACK, 'launch-cleanup.sh');

const run = (args, env) => new Promise((resolve) => {
  const p = spawn('bash', [SCRIPT, ...args], { env, stdio: ['ignore', 'pipe', 'pipe'] });
  let out = '';
  p.stdout.on('data', (d) => { out += d; });
  p.stderr.on('data', (d) => { out += d; });
  p.on('close', (code) => resolve({ code, out: out.replace(/\x1b\[[0-9;]*m/g, '') }));
});

(async () => {
  const t = fakes.checker();
  const db = await open();
  const MR = 'favorit0-0000-0000-0000-00000favorit';

  // The columns the script reads, on top of the cut-down upstream tables.
  await db.sql('ALTER TABLE atlas_driver_offer_bpp.person ADD COLUMN mobile_country_code text, ADD COLUMN first_name text');
  await db.sql('ALTER TABLE atlas_app.person ADD COLUMN mobile_country_code text, ADD COLUMN first_name text');
  await db.sql('CREATE TABLE movin.driver_document (driver_id text, stored_path text)');
  await db.sql('CREATE TABLE movin.admin_audit (actor_email text, action text)');

  // The roster, from the simulator itself.
  const sim = fs.readFileSync(path.join(STACK, 'simulate-driver.py'), 'utf8');
  const cars = [...sim.matchAll(/\("(\d{8,10})",\s*"(?:SEDAN|HATCHBACK|SUV)"/g)].map((m) => m[1]);
  t.section('0. the list is the simulator\'s roster');
  t.ok(cars.length === 12, 'the simulator has twelve cars', String(cars.length));
  const sqlText = fs.readFileSync(path.join(STACK, 'launch-cleanup-targets.sql'), 'utf8');
  t.ok(cars.every((n) => sqlText.includes(`'${n}'`)), 'every one of them is in launch-cleanup-targets.sql');

  const cc = (n) => (n.startsWith('0') ? '+213' : '+222');
  for (const [i, n] of cars.entries()) {
    await db.sql('INSERT INTO atlas_driver_offer_bpp.person (id, merchant_id, role, unencrypted_mobile_number, mobile_country_code, first_name) VALUES ($1,$2,$3,$4,$5,$6)',
      [`car${i}`, MR, 'DRIVER', n, cc(n), `Car${i}`]);
  }
  await db.sql("INSERT INTO atlas_app.person (id, unencrypted_mobile_number, mobile_country_code, first_name) VALUES ('rider-mr','22778899','+222','Test MR'), ('rider-dz','0555000199','+213','Test DZ')");

  // People who must survive, and the ones that only look like targets.
  const keep = [
    ['atlas_driver_offer_bpp', 'real-d1', '22123456', '+222', 'Real driver'],
    ['atlas_driver_offer_bpp', 'real-d2', '0555100001', '+222', 'Mauritanian row with an Algerian-looking number'],
    ['atlas_driver_offer_bpp', 'real-d3', '22100001', '+213', 'Algerian row with a Mauritanian-looking number'],
    ['atlas_app', 'real-r1', '22100001', '+222', 'A passenger on a car number (wrong side)'],
    ['atlas_app', 'real-r2', '22778899', '+213', 'A passenger on the test number, other country'],
    ['atlas_app', 'real-r3', '22334455', '+222', 'Real passenger'],
  ];
  for (const [sch, id, num, c, name] of keep) {
    if (sch === 'atlas_app') await db.sql(`INSERT INTO atlas_app.person (id, unencrypted_mobile_number, mobile_country_code, first_name) VALUES ($1,$2,$3,$4)`, [id, num, c, name]);
    else await db.sql(`INSERT INTO atlas_driver_offer_bpp.person (id, merchant_id, role, unencrypted_mobile_number, mobile_country_code, first_name) VALUES ($1,$2,'DRIVER',$3,$4,$5)`, [id, MR, num, c, name]);
  }
  // A rider-role row in the driver table with a car's number: not a DRIVER.
  await db.sql(`INSERT INTO atlas_driver_offer_bpp.person (id, merchant_id, role, unencrypted_mobile_number, mobile_country_code, first_name) VALUES ('real-d4',$1,'ADMIN','22100002','+222','Not a driver')`, [MR]);

  const fd = await fakeDocker.start(db);
  const anon = path.join(os.tmpdir(), `anonymise-stub-${process.pid}.sql`);
  // The same variables and the same last line (rows_changed) as the console's
  // file; the real one is in the website repository.
  fs.writeFileSync(anon, `
    UPDATE atlas_driver_offer_bpp.person SET unencrypted_mobile_number = NULL, first_name = 'deleted' WHERE :'side' = 'driver' AND id::text = :'person_id';
    UPDATE atlas_app.person SET unencrypted_mobile_number = NULL, first_name = 'deleted' WHERE :'side' = 'rider' AND id::text = :'person_id';
    INSERT INTO movin.admin_audit (actor_email, action) VALUES (:'actor', 'deletion.anonymise');
    SELECT 1 AS rows_changed;
  `);
  const env = { ...process.env, PATH: `${fd.bin}:${process.env.PATH}`, ANONYMISE_SQL: anon, ACTOR: 'owner: test run', FAKE_DOCKER_PORT: '' };
  const people = async () => (await db.sql('SELECT id, unencrypted_mobile_number n, first_name FROM atlas_driver_offer_bpp.person ORDER BY id')).length
    + (await db.sql('SELECT id FROM atlas_app.person ORDER BY id')).length;
  const rowsOf = (sql) => db.sql(sql);

  try {
    t.section('1. a dry run lists the fourteen and writes nothing');
    const total = await people();
    let r = await run(['--rehearsal', 'fake-pg'], env);
    t.ok(r.code === 0, 'exit 0', r.out.slice(-300));
    t.ok(/12 driver\(s\) and 2 passenger\(s\)/.test(r.out), 'twelve cars and two passengers', r.out.split('\n').filter((l) => /driver\(s\)/.test(l)).join());
    for (const n of [...cars, '22778899', '0555000199']) t.ok(r.out.includes(n), `lists ${n}`);
    for (const [, id] of keep) t.ok(!r.out.includes(id), `does not list ${id}`);
    t.ok(!r.out.includes('real-d4'), 'does not list the non-driver on a car number');
    t.ok(/dry run: nothing was written/.test(r.out), 'says it wrote nothing');
    t.ok((await rowsOf("SELECT 1 FROM atlas_driver_offer_bpp.person WHERE first_name = 'deleted'")).length === 0
      && (await rowsOf('SELECT 1 FROM movin.admin_audit')).length === 0, 'and wrote nothing');

    t.section('2. the rehearsal erases those fourteen and nobody else');
    r = await run(['--rehearsal', 'fake-pg', '--apply'], env);
    t.ok(r.code === 0, 'exit 0', r.out.slice(-400));
    const gone = await rowsOf("SELECT id FROM atlas_driver_offer_bpp.person WHERE first_name = 'deleted' ORDER BY id");
    const goneR = await rowsOf("SELECT id FROM atlas_app.person WHERE first_name = 'deleted' ORDER BY id");
    t.ok(gone.length === 12 && goneR.length === 2, 'twelve drivers and two passengers erased', `${gone.length}/${goneR.length}`);
    const left = await rowsOf("SELECT id FROM atlas_driver_offer_bpp.person WHERE first_name <> 'deleted' ORDER BY id");
    t.ok(left.map((x) => x.id).join() === 'real-d1,real-d2,real-d3,real-d4', 'the four look-alikes and the real driver are untouched', left.map((x) => x.id).join());
    const leftR = await rowsOf("SELECT id FROM atlas_app.person WHERE first_name <> 'deleted' ORDER BY id");
    t.ok(leftR.map((x) => x.id).join() === 'real-r1,real-r2,real-r3', 'the real passengers are untouched', leftR.map((x) => x.id).join());
    t.ok((await people()) === total, 'no person row was deleted: erased, not removed');
    t.ok((await rowsOf("SELECT 1 FROM movin.admin_audit WHERE actor_email = 'owner: test run'")).length === 14, 'one audit row per account');
    t.ok(/none of the listed numbers can be found any more/.test(r.out), 'it checked afterwards that none can be found');

    t.section('3. run again: nothing to do');
    r = await run(['--rehearsal', 'fake-pg', '--apply'], env);
    t.ok(r.code === 0 && /nothing to do/.test(r.out), 'a second run finds none and says so', r.out.slice(-200));
    t.ok((await rowsOf('SELECT 1 FROM movin.admin_audit')).length === 14, 'and writes no more audit rows');

    t.section('4. it refuses, changing nothing, when the selection is not what it was written for');
    // Thirteen cars: a duplicate of a car's number appears.
    await db.sql("INSERT INTO atlas_driver_offer_bpp.person (id, merchant_id, role, unencrypted_mobile_number, mobile_country_code, first_name) VALUES ('dup', $1, 'DRIVER', '22100001', '+222', 'Duplicate')", [MR]);
    await db.sql("INSERT INTO atlas_driver_offer_bpp.person (id, merchant_id, role, unencrypted_mobile_number, mobile_country_code, first_name) SELECT 'x' || g, $1, 'DRIVER', '2210000' || (g % 6 + 1), '+222', 'Extra' FROM generate_series(1, 12) g", [MR]);
    r = await run(['--rehearsal', 'fake-pg', '--apply'], env);
    t.ok(r.code === 1 && /at most 12 were expected/.test(r.out), 'more than twelve cars: stopped', r.out.slice(-200));
    t.ok((await rowsOf("SELECT 1 FROM atlas_driver_offer_bpp.person WHERE id = 'dup' AND first_name = 'Duplicate'")).length === 1, 'and nothing was erased');
    await db.sql("DELETE FROM atlas_driver_offer_bpp.person WHERE id = 'dup' OR id LIKE 'x%'");

    t.section('5. papers on the volume stop it until someone has dealt with them');
    await db.sql("UPDATE atlas_driver_offer_bpp.person SET unencrypted_mobile_number = '22100003', mobile_country_code = '+222', first_name = 'Mohamed' WHERE id = 'real-d1'");
    await db.sql("INSERT INTO movin.driver_document (driver_id, stored_path) VALUES ('real-d1', '/data/docs/real-d1/licence.jpg')");
    // (real-d1 now wears a car's number, as a stand-in for "a target with papers")
    r = await run(['--rehearsal', 'fake-pg', '--apply'], env);
    t.ok(r.code === 1 && /papers on the documents volume/.test(r.out) && r.out.includes('/data/docs/real-d1/licence.jpg'),
      'stopped, naming the file', r.out.slice(-300));
    t.ok((await rowsOf("SELECT 1 FROM atlas_driver_offer_bpp.person WHERE id = 'real-d1' AND first_name = 'Mohamed'")).length === 1, 'and nothing was erased');
    r = await run(['--rehearsal', 'fake-pg', '--apply', '--papers-handled'], env);
    t.ok(r.code === 0 && (await rowsOf("SELECT 1 FROM atlas_driver_offer_bpp.person WHERE id = 'real-d1' AND first_name = 'deleted'")).length === 1,
      'with --papers-handled it goes on', r.out.slice(-200));

    t.section('6. the live mode needs root, and the rehearsal hook exists');
    // Not run as root here: only the refusal can be tested honestly.
    if (typeof process.getuid === 'function' && process.getuid() !== 0) {
      r = await run([], env);
      t.ok(r.code === 1 && /run it with sudo/.test(r.out), 'the live mode refuses without root');
    }
    t.ok(/REHEARSE_HOOK/.test(fs.readFileSync(path.join(STACK, 'restore.sh'), 'utf8')), 'restore.sh rehearse has the REHEARSE_HOOK');
  } finally {
    await fd.stop();
    fs.rmSync(anon, { force: true });
  }
  await db.close();
  t.finish();
})().catch((e) => { console.error(e); process.exit(2); });
