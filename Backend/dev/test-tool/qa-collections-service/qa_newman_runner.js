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

run.on('request', (err, args) => {
  try {
    if (err) {
      emit('request', { name: args && args.item && args.item.name, error: err.message });
      return;
    }
    const response = args.response;
    let url;
    try { url = args.request && args.request.url && args.request.url.toString(); } catch (_) { /* best-effort */ }
    emit('request', {
      name: args.item && args.item.name,
      method: args.request && args.request.method,
      url,
      status: response && response.code,
      responseTime: response && response.responseTime,
    });
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
