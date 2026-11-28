'use strict';
// The JSON answer every Google-shaped route gives: one helper, so the
// modules split out of server.js (phase 5, 2026-10-06) answer with exactly
// the headers they had when they lived there.

function send(res, code, obj) {
  const body = JSON.stringify(obj);
  res.writeHead(code, { 'content-type': 'application/json;charset=utf-8', 'content-length': Buffer.byteLength(body) });
  res.end(body);
}

module.exports = { send };
