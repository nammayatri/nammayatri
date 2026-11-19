'use strict';
// The `docker` on the test's PATH: posts its arguments and stdin to
// lib/fake-docker.js and prints what comes back. See that file.
const http = require('http');

const argv = process.argv.slice(2);
const read = () => new Promise((resolve) => {
  // Only `exec -i ... -f -` reads stdin; anything else must not wait on a terminal.
  if (process.stdin.isTTY || !argv.includes('-i')) return resolve('');
  const chunks = [];
  process.stdin.on('data', (c) => chunks.push(c));
  process.stdin.on('end', () => resolve(Buffer.concat(chunks).toString('utf8')));
});

read().then((stdin) => {
  const body = JSON.stringify({ argv, stdin });
  const req = http.request(
    { host: '127.0.0.1', port: Number(process.env.FAKE_DOCKER_PORT), method: 'POST', path: '/' },
    (res) => {
      const chunks = [];
      res.on('data', (c) => chunks.push(c));
      res.on('end', () => {
        const r = JSON.parse(Buffer.concat(chunks).toString('utf8'));
        if (r.out) process.stdout.write(r.out);
        if (r.err) process.stderr.write(r.err);
        process.exit(r.code);
      });
    },
  );
  req.on('error', (e) => { process.stderr.write(`fake docker: ${e.message}\n`); process.exit(125); });
  req.end(body);
});
