'use strict';
// The backend apps' own log FILES must go nowhere (2026-11-03).
//
// dhall sets logFilePath with logRawSql = True, so the driver app appended every
// SQL statement to /tmp/dynamic-offer-driver-app.log inside its container, with
// nothing to rotate it: 33 GB in 16 days. The compose file binds /dev/null over
// that path (and the rider's); stdout, which Docker rotates, is untouched. Checked
// on the text, because no Docker runs where this is tested.
//
//   cd Backend/dev/local-stack/tests && node compose-logfiles.test.js
const fs = require('fs');
const path = require('path');
const fakes = require('./lib/fakes');

const compose = fs.readFileSync(path.join(__dirname, '..', 'stack', 'docker-compose.yml'), 'utf8');

/** The text of one top-level service, up to the next one. */
function service(name) {
  const start = compose.indexOf(`\n  ${name}:\n`);
  if (start < 0) return '';
  const rest = compose.slice(start + 1);
  const next = rest.slice(1).search(/\n  [a-z][a-z0-9-]*:\n/);
  return next < 0 ? rest : rest.slice(0, next + 1);
}

const c = fakes.checker();
for (const [svc, file] of [
  ['driver-app', '/tmp/dynamic-offer-driver-app.log'],
  ['rider-app', '/tmp/rider-app.log'],
]) {
  const body = service(svc);
  c.ok(body.length > 0, `${svc} is found in the compose file`);
  c.ok(
    new RegExp(`^\\s+- /dev/null:${file.replace(/[.]/g, '\\.')}\\s*$`, 'm').test(body),
    `${svc} binds /dev/null over ${file}`,
  );
}
c.finish();
