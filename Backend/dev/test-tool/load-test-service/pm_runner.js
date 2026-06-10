#!/usr/bin/env node
/**
 * pm_runner.js — Persistent Postman-script executor
 *
 * Protocol (JSON lines on stdin/stdout):
 *   IN:  { id, script, responseBody, responseStatus, responseHeaders, stores }
 *   OUT: { id, stores, assertions, consoleLogs, error, skipped }
 *
 * Stays alive until stdin closes (one process per load-test run).
 * Date.now() is overridden so busy-wait delay loops (while Date.now()-start < N)
 * exit immediately; Python handles the actual sleep.
 */
'use strict';

const readline = require('readline');

const rl = readline.createInterface({ input: process.stdin, crlfDelay: Infinity, terminal: false });

rl.on('line', (line) => {
  if (!line.trim()) return;
  let req;
  try { req = JSON.parse(line); }
  catch (e) {
    process.stdout.write(JSON.stringify({ id: null, error: 'JSON parse: ' + e.message }) + '\n');
    return;
  }
  let result;
  try { result = runScript(req); }
  catch (e) { result = { stores: req.stores || {}, assertions: [], consoleLogs: [], error: String(e), skipped: false }; }
  process.stdout.write(JSON.stringify({ id: req.id, ...result }) + '\n');
});

rl.on('close', () => process.exit(0));

// ── Expect-chain builder (chai-compatible subset) ─────────────────────────────

function makeChain(val, negate) {
  negate = !!negate;

  const CHAIN_WORDS = new Set([
    'to','be','been','is','that','which','and','has','have','with','at','of','same','deep','any','all',
  ]);

  const handler = {
    get(_, prop) {
      if (prop === 'not') return makeChain(val, !negate);

      if (CHAIN_WORDS.has(prop)) return makeChain(val, negate);

      // Getters that evaluate immediately
      if (prop === 'ok') {
        if (negate ? !!val : !val) throw new Error(`Expected value to${negate ? ' not' : ''} be ok`);
        return makeChain(val, negate);
      }
      if (prop === 'empty') {
        const isEmpty = val == null || val === '' ||
          (Array.isArray(val) && val.length === 0) ||
          (typeof val === 'object' && val !== null && !Array.isArray(val) && Object.keys(val).length === 0);
        if (negate ? isEmpty : !isEmpty) throw new Error(`Expected ${JSON.stringify(val)} to${negate ? ' not' : ''} be empty`);
        return makeChain(val, negate);
      }

      // Methods — return a function that runs the assertion then returns chain
      return (...args) => {
        const neg = negate; // capture snapshot; `not` resets for each chain
        switch (prop) {
          case 'equal': case 'equals': case 'eql': {
            const eq = JSON.stringify(val) === JSON.stringify(args[0]);
            if (neg ? eq : !eq) throw new Error(`Expected ${JSON.stringify(val)} to${neg ? ' not' : ''} equal ${JSON.stringify(args[0])}`);
            break;
          }
          case 'include': case 'contain': case 'includes': {
            const has = Array.isArray(val) ? val.includes(args[0]) : String(val).includes(String(args[0]));
            if (neg ? has : !has) throw new Error(`Expected ${JSON.stringify(val)} to${neg ? ' not' : ''} include ${JSON.stringify(args[0])}`);
            break;
          }
          case 'an': case 'a': {
            if (args[0] === 'array') {
              if (neg ? Array.isArray(val) : !Array.isArray(val)) throw new Error(`Expected value to${neg ? ' not' : ''} be an array`);
            } else if (args[0] === 'object') {
              const isObj = typeof val === 'object' && val !== null && !Array.isArray(val);
              if (neg ? isObj : !isObj) throw new Error(`Expected value to${neg ? ' not' : ''} be an object`);
            } else {
              const t = typeof val;
              if (neg ? t === args[0] : t !== args[0]) throw new Error(`Expected type '${t}' to${neg ? ' not' : ''} equal '${args[0]}'`);
            }
            break;
          }
          case 'above': case 'gt': {
            if (neg ? val > args[0] : val <= args[0]) throw new Error(`Expected ${val} to${neg ? ' not' : ''} be above ${args[0]}`);
            break;
          }
          case 'below': case 'lt': {
            if (neg ? val < args[0] : val >= args[0]) throw new Error(`Expected ${val} to${neg ? ' not' : ''} be below ${args[0]}`);
            break;
          }
          case 'least': case 'gte': {
            if (neg ? val >= args[0] : val < args[0]) throw new Error(`Expected ${val} to${neg ? ' not' : ''} be at least ${args[0]}`);
            break;
          }
          case 'most': case 'lte': {
            if (neg ? val <= args[0] : val > args[0]) throw new Error(`Expected ${val} to${neg ? ' not' : ''} be at most ${args[0]}`);
            break;
          }
          case 'property': {
            const exists = val != null && args[0] in Object(val);
            if (neg ? exists : !exists) throw new Error(`Expected object to${neg ? ' not' : ''} have property '${args[0]}'`);
            if (args.length > 1 && exists) {
              const eq = JSON.stringify(val[args[0]]) === JSON.stringify(args[1]);
              if (neg ? eq : !eq) throw new Error(`Expected '${args[0]}' = ${JSON.stringify(val[args[0]])} to${neg ? ' not' : ''} equal ${JSON.stringify(args[1])}`);
            }
            break;
          }
          case 'lengthOf': case 'length': {
            const len = val?.length ?? 0;
            if (neg ? len === args[0] : len !== args[0]) throw new Error(`Expected length ${len} to${neg ? ' not' : ''} equal ${args[0]}`);
            break;
          }
          case 'oneOf': {
            const inSet = (args[0] ?? []).includes(val);
            if (neg ? inSet : !inSet) throw new Error(`Expected ${JSON.stringify(val)} to${neg ? ' not' : ''} be one of ${JSON.stringify(args[0])}`);
            break;
          }
          case 'match': {
            const m = new RegExp(args[0]).test(String(val));
            if (neg ? m : !m) throw new Error(`Expected ${JSON.stringify(val)} to${neg ? ' not' : ''} match ${args[0]}`);
            break;
          }
          case 'status': {
            // pm.expect(pm.response.code).to.equal(n) style
            if (neg ? val === args[0] : val !== args[0]) throw new Error(`Expected status ${val} to${neg ? ' not' : ''} equal ${args[0]}`);
            break;
          }
          // Unknown assertion — no-op, allow chaining
          default: break;
        }
        return makeChain(val, negate);
      };
    },
  };
  return new Proxy({}, handler);
}

// ── Script runner ─────────────────────────────────────────────────────────────

function runScript({ id, script, responseBody, responseStatus, responseHeaders, stores }) {
  if (!stores) stores = {};
  if (!stores.environment) stores.environment = {};
  if (!stores.collection) stores.collection = {};

  const assertions = [];
  const consoleLogs = [];
  let skipped = false;
  let error = null;

  // Date.now() override: each call returns a value 100 000ms ahead of the previous.
  // First call → a fixed base (used as `start`), next call → base + 100000.
  // So: Date.now() - start ≈ 100000 > any reasonable busy-wait duration → loop exits immediately.
  //
  // IMPORTANT: we only override the *static* Date.now(). `new Date()` must stay real so
  // prerequest scripts can generate ISO timestamps (e.g. Driver Set Location body).
  let _dnCounter = 0;
  const _dnBase = 1_000_000_000_000;
  const fakeDateNow = () => _dnBase + (++_dnCounter) * 100_000;

  // Proxy wraps the real Date constructor: new Date() works normally, only .now() is faked.
  const fakeDate = new Proxy(Date, {
    get(target, prop) {
      if (prop === 'now') return fakeDateNow;
      const val = Reflect.get(target, prop);
      return typeof val === 'function' ? val.bind(target) : val;
    },
    construct(target, args) {
      return Reflect.construct(target, args);
    },
  });

  let parsedBody;
  try { parsedBody = JSON.parse(responseBody || 'null'); } catch { parsedBody = null; }

  const pm = {
    response: {
      json: () => parsedBody,
      text: () => responseBody ?? '',
      code: responseStatus ?? 0,
      status: responseStatus ?? 0,
      headers: {
        get: (k) => (responseHeaders ?? {})[k.toLowerCase()] ?? (responseHeaders ?? {})[k] ?? '',
        has: (k) => k in (responseHeaders ?? {}),
      },
      to: {
        have: {
          status: (code) => {
            if ((responseStatus ?? 0) !== code)
              throw new Error(`Expected status ${code} but got ${responseStatus}`);
          },
        },
      },
    },
    environment: {
      set: (k, v) => { stores.environment[String(k)] = v == null ? '' : String(v); },
      get: (k) => { const s = String(k); return s in stores.environment ? stores.environment[s] : null; },
      has: (k) => String(k) in stores.environment,
      unset: (k) => { delete stores.environment[String(k)]; },
    },
    collectionVariables: {
      set: (k, v) => { stores.collection[String(k)] = v == null ? '' : String(v); },
      get: (k) => { const s = String(k); return s in stores.collection ? stores.collection[s] : null; },
      has: (k) => String(k) in stores.collection,
      unset: (k) => { delete stores.collection[String(k)]; },
    },
    variables: {
      set: (k, v) => { stores.collection[String(k)] = v == null ? '' : String(v); },
      get: (k) => {
        const s = String(k);
        if (s in stores.collection) return stores.collection[s];
        if (s in stores.environment) return stores.environment[s];
        return null;
      },
      has: (k) => String(k) in stores.collection || String(k) in stores.environment,
    },
    globals: {
      set: (k, v) => { stores.environment[String(k)] = v == null ? '' : String(v); },
      get: (k) => { const s = String(k); return s in stores.environment ? stores.environment[s] : null; },
      has: (k) => String(k) in stores.environment,
    },
    execution: { skipRequest: () => { skipped = true; } },
    info: { requestName: String(id ?? '') },
    request: { url: { toString: () => '' }, headers: { get: () => '' } },
    sendRequest: (opts, cb) => { if (typeof cb === 'function') cb(null, { json: () => ({}), code: 200, text: () => '{}' }); },
    test: (name, fn) => {
      try { fn(); assertions.push({ name, passed: true, error: null }); }
      catch (e) { assertions.push({ name, passed: false, error: e.message || String(e) }); }
    },
    expect: (val) => makeChain(val, false),
  };

  const postman = {
    setNextRequest: () => {},
    setEnvironmentVariable: (k, v) => pm.environment.set(k, v),
    getEnvironmentVariable: (k) => pm.environment.get(k),
    clearEnvironmentVariable: (k) => pm.environment.unset(k),
    getCollectionVariable: (k) => pm.collectionVariables.get(k),
    setCollectionVariable: (k, v) => pm.collectionVariables.set(k, v),
  };

  const con = {
    log:   (...a) => consoleLogs.push(a.map(x => typeof x === 'object' ? JSON.stringify(x) : String(x)).join(' ')),
    warn:  (...a) => consoleLogs.push('[W] ' + a.join(' ')),
    error: (...a) => consoleLogs.push('[E] ' + a.join(' ')),
    info:  (...a) => consoleLogs.push('[I] ' + a.join(' ')),
  };


  try {
    // eslint-disable-next-line no-new-func
    new Function(
      'pm', 'postman', 'console',
      'setTimeout', 'clearTimeout', 'setInterval', 'clearInterval',
      'Date', 'Math', 'JSON', 'String', 'Number', 'Boolean', 'Array', 'Object',
      'parseInt', 'parseFloat', 'isNaN', 'isFinite', 'encodeURIComponent', 'decodeURIComponent',
      'responseBody', 'responseCode',
      script || ''
    )(
      pm, postman, con,
      () => {}, () => {}, () => {}, () => {},
      fakeDate, Math, JSON, String, Number, Boolean, Array, Object,
      parseInt, parseFloat, isNaN, isFinite, encodeURIComponent, decodeURIComponent,
      responseBody ?? '', { code: responseStatus ?? 0 }
    );
  } catch (e) {
    error = e.message || String(e);
  }

  return { stores, assertions, consoleLogs, error, skipped };
}
