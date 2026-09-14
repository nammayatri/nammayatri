#!/usr/bin/env node
'use strict';

const fs = require('fs');
const newman = require('newman');

const [, , collectionPath, envPath] = process.argv;

function emit(type, data) {
  process.stdout.write(JSON.stringify({ type, ...data }) + '\n');
}

function readJson(path, label) {
  try {
    return JSON.parse(fs.readFileSync(path, 'utf8'));
  } catch (e) {
    emit('done', { passed: 0, failed: 1, error: `failed to read ${label} (${path}): ${e.message}` });
    process.exit(1);
  }
}

if (!collectionPath) {
  emit('done', { passed: 0, failed: 1, error: 'usage: qa_newman_runner.js <collectionPath> [envPath]' });
  process.exit(2);
}

const collection = readJson(collectionPath, 'collection');
const environment = envPath ? readJson(envPath, 'environment') : undefined;

let passed = 0;
let failed = 0;

const run = newman.run(
  {
    collection,
    environment,
    reporters: [],
    timeoutRequest: 30000,
  },
  (err, summary) => {
    if (err) {
      emit('done', { passed, failed: failed || 1, error: err.message });
      process.exit(1);
      return;
    }
    const runFailures = (summary.run.failures || []).length;
    if (runFailures > 0 && failed === 0) failed = runFailures;
    emit('done', {
      passed,
      failed,
      totalRequests: summary.run.stats.requests.total,
    });
    process.exit(failed > 0 ? 1 : 0);
  }
);

run.on('start', () => {
  emit('start', { collection: collection.info && collection.info.name });
});

run.on('beforeItem', (err, args) => {
  if (err || !args) return;
  emit('item_start', { name: args.item && args.item.name });
});

// 4000 was too small for real API responses (e.g. booking/ride list payloads)
// — truncating mid-JSON breaks JSON.parse on the dashboard side, which then
// falls back to showing the raw escaped string instead of pretty-printing it.
const MAX_BODY_CHARS = 20000;

function truncate(s) {
  if (typeof s !== 'string') return s;
  return s.length > MAX_BODY_CHARS ? s.slice(0, MAX_BODY_CHARS) + '…(truncated)' : s;
}

run.on('request', (err, args) => {
  try {
    if (err) {
      emit('request', { name: args && args.item && args.item.name, error: err.message });
      return;
    }
    const response = args.response;
    let url;
    try { url = args.request && args.request.url && args.request.url.toString(); } catch (_) { /* best-effort */ }
    const status = response && response.code;

    const event = {
      name: args.item && args.item.name,
      method: args.request && args.request.method,
      url,
      status,
      responseTime: response && response.responseTime,
    };

    // Always capture body/headers (not just on failure) — the dashboard's step
    // list shows the raw request/response for every step, pass or fail, same
    // as the in-browser runtime does for non-QA collections.
    try { event.responseBody = truncate(response && response.text()); } catch (_) { /* binary or unreadable body */ }
    try { event.responseHeaders = response && response.headers && response.headers.all().map(h => ({ key: h.key, value: h.value })); } catch (_) { /* best-effort */ }
    try { event.requestBody = truncate(args.request && args.request.body && args.request.body.toString()); } catch (_) { /* best-effort */ }
    try { event.requestHeaders = args.request && args.request.headers && args.request.headers.all().map(h => ({ key: h.key, value: h.value })); } catch (_) { /* best-effort */ }

    emit('request', event);
  } catch (_) { /* never let a malformed event crash the run */ }
});

run.on('assertion', (err, args) => {
  if (!args) return;
  const ok = !err;
  if (ok) passed += 1; else failed += 1;
  emit('assertion', {
    name: args.assertion,
    passed: ok,
    error: err ? err.message : null,
    item: args.item && args.item.name,
  });
});

run.on('script', (err) => {
  if (err) emit('script_error', { error: err.message });
});

run.on('console', (err, args) => {
  if (err || !args) return;
  emit('console', { level: args.level, messages: (args.messages || []).map(String) });
});
