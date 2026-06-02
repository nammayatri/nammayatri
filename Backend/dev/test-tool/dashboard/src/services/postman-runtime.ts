/**
 * Minimal Postman Script Runtime
 *
 * Executes Postman test/prerequest scripts in the browser.
 * Only implements the subset of pm.* API used by our collections.
 */

import axios from 'axios';
import { PROXY_BASE, MOCK_SERVER_URL, RIDER_URL, DRIVER_URL, PROVIDER_DASHBOARD_URL } from '../config';

export interface PostmanRuntimeResult {
  assertions: Array<{ name: string; passed: boolean; error?: string }>;
  consoleLogs: string[];
  error?: string;
}

export interface VariableStores {
  environment: Record<string, string>;
  collection: Record<string, string>;
}

// Browser → mock-server / rider / driver direct calls fail CORS — route through the local proxy.
const SERVICE_URL_REWRITES: Array<[string, string]> = [
  [MOCK_SERVER_URL, PROXY_BASE + '/proxy/mock-server'],
  [RIDER_URL, PROXY_BASE + '/proxy/rider-raw'],
  [DRIVER_URL, PROXY_BASE + '/proxy/driver-raw'],
  [PROVIDER_DASHBOARD_URL, PROXY_BASE + '/proxy/provider-dashboard'],
];

function rewriteUrl(url: string): string {
  for (const [from, to] of SERVICE_URL_REWRITES) {
    if (url.startsWith(from)) return to + url.slice(from.length);
  }
  return url;
}

/**
 * Build async helpers (real setTimeout + sendRequest) plus a drain() that awaits all pending callbacks
 * (and any further setTimeouts/sendRequests they queue).
 */
function makeAsyncHelpers(consoleLogs: string[]) {
  const pending: Promise<void>[] = [];
  const logCbError = (where: string, e: any) => {
    consoleLogs.push(`[${where} cb error] ` + (e && e.message ? e.message : String(e)));
  };

  const setTimeoutShim = (cb: () => void, ms: number) => {
    const p = new Promise<void>(resolve => {
      setTimeout(() => {
        try { cb(); } catch (e) { logCbError('setTimeout', e); } finally { resolve(); }
      }, ms);
    });
    pending.push(p);
  };

  const sendRequestShim = (opts: any, cb: (err: any, res: any) => void) => {
    const isStr = typeof opts === 'string';
    const url = rewriteUrl(isStr ? opts : opts.url);
    const method = isStr ? 'GET' : (opts.method || 'GET');
    const headers: Record<string, string> = {};
    if (!isStr && opts.header) {
      if (Array.isArray(opts.header)) {
        for (const h of opts.header) headers[h.key] = h.value;
      } else if (typeof opts.header === 'object') {
        Object.assign(headers, opts.header);
      }
    }
    // Postman-style body: { mode: 'raw', raw: <string|object> } or a plain string/object.
    // Axios with our identity transformRequest won't stringify objects, so do it here.
    let body: any = undefined;
    if (!isStr && opts.body) {
      const raw = opts.body.mode === 'raw' ? opts.body.raw : opts.body;
      body = typeof raw === 'string' ? raw : JSON.stringify(raw);
      if (!headers['Content-Type']) headers['Content-Type'] = 'application/json';
    }
    const p = axios({
      url, method, headers,
      data: body,
      transformRequest: [(d: any) => d],
      timeout: 30000,
      validateStatus: () => true,
    }).then(resp => {
      const data = resp.data;
      const res = {
        json: () => (typeof data === 'string' ? (() => { try { return JSON.parse(data); } catch { return null; } })() : data),
        text: () => (typeof data === 'string' ? data : JSON.stringify(data)),
        code: resp.status,
        status: resp.status,
      };
      try { cb(null, res); } catch (e) { logCbError('sendRequest', e); }
    }).catch(err => {
      try { cb(err, null); } catch (e) { logCbError('sendRequest', e); }
    });
    pending.push(p);
  };

  const drain = async () => {
    while (pending.length > 0) {
      const p = pending.shift()!;
      try { await p; } catch { /* swallowed at callback edge above */ }
    }
  };

  return { setTimeoutShim, sendRequestShim, drain };
}

/**
 * Resolve all {{variable}} placeholders in a string using the variable stores.
 * Resolution order: collection vars → environment vars (matching Postman precedence).
 */
/**
 * Resolve all {{variable}} placeholders in a string using the variable stores.
 * Resolution order: collection vars → environment vars (matching Postman precedence).
 */
export function resolveVariables(template: string, stores: VariableStores): string {
  return template.replace(/\{\{(\w+)\}\}/g, (_, key) => {
    return stores.collection[key] ?? stores.environment[key] ?? '';
  });
}

/**
 * Execute a Postman test script against a response.
 *
 * Async to support pm.sendRequest + real setTimeout from inside scripts (e.g. polling loops).
 * After the synchronous body of the script returns, we drain all pending async callbacks
 * (setTimeout fires, sendRequest responses) — including any new ones queued from within those callbacks —
 * so the script effectively runs to completion before this function resolves.
 */
export async function executeTestScript(
  script: string,
  responseData: any,
  responseStatus: number,
  stores: VariableStores
): Promise<PostmanRuntimeResult> {
  const assertions: PostmanRuntimeResult['assertions'] = [];
  const consoleLogs: string[] = [];

  const { setTimeoutShim, sendRequestShim, drain } = makeAsyncHelpers(consoleLogs);

  const pm: any = buildPmObject(responseData, responseStatus, stores, assertions);
  pm.sendRequest = sendRequestShim;

  const consoleObj = {
    log: (...args: any[]) => consoleLogs.push(args.map(a => typeof a === 'object' ? JSON.stringify(a) : String(a)).join(' ')),
    warn: (...args: any[]) => consoleLogs.push('[WARN] ' + args.join(' ')),
    error: (...args: any[]) => consoleLogs.push('[ERROR] ' + args.join(' ')),
  };

  try {
    const postman = {
      setNextRequest: () => {},
      setEnvironmentVariable: (key: string, val: any) => { stores.environment[key] = String(val ?? ''); },
      getEnvironmentVariable: (key: string) => stores.environment[key] ?? '',
      setGlobalVariable: (key: string, val: any) => { stores.environment[key] = String(val ?? ''); },
      getGlobalVariable: (key: string) => stores.environment[key] ?? '',
      clearEnvironmentVariable: (key: string) => { delete stores.environment[key]; },
    };
    const responseBody = responseData != null ? JSON.stringify(responseData) : '';
    const responseCode = { code: responseStatus };
    // eslint-disable-next-line no-new-func
    const fn = new Function('pm', 'console', 'setTimeout', 'postman', 'responseBody', 'responseCode', 'JSON', script);
    fn(pm, consoleObj, setTimeoutShim, postman, responseBody, responseCode, JSON);
    await drain();
  } catch (e: any) {
    return { assertions, consoleLogs, error: e.message };
  }

  return { assertions, consoleLogs };
}

/**
 * Execute a Postman prerequest script (for collection-level variable init).
 *
 * Async, like executeTestScript — supports pm.sendRequest + real setTimeout for prereq polling.
 */
export async function executePrereqScript(
  script: string,
  stores: VariableStores
): Promise<{ consoleLogs: string[]; error?: string; skipped?: boolean }> {
  const consoleLogs: string[] = [];
  const assertions: PostmanRuntimeResult['assertions'] = [];

  const { setTimeoutShim, sendRequestShim, drain } = makeAsyncHelpers(consoleLogs);

  const pm: any = buildPmObject(null, 0, stores, assertions);
  let skipped = false;
  pm.execution = { skipRequest: () => { skipped = true; } };
  pm.sendRequest = sendRequestShim;

  const consoleObj = {
    log: (...args: any[]) => consoleLogs.push(args.join(' ')),
    warn: (...args: any[]) => consoleLogs.push('[WARN] ' + args.join(' ')),
    error: (...args: any[]) => consoleLogs.push('[ERROR] ' + args.join(' ')),
  };

  try {
    // eslint-disable-next-line no-new-func
    const fn = new Function('pm', 'console', 'setTimeout', 'Math', 'String', 'Date', 'JSON', 'postman', script);
    const postman = {
      setNextRequest: () => {},
      setEnvironmentVariable: (key: string, val: any) => { stores.environment[key] = String(val ?? ''); },
      getEnvironmentVariable: (key: string) => stores.environment[key] ?? '',
      setGlobalVariable: (key: string, val: any) => { stores.environment[key] = String(val ?? ''); },
      getGlobalVariable: (key: string) => stores.environment[key] ?? '',
      clearEnvironmentVariable: (key: string) => { delete stores.environment[key]; },
    };
    fn(pm, consoleObj, setTimeoutShim, Math, String, Date, JSON, postman);
    await drain();
  } catch (e: any) {
    return { consoleLogs, error: e.message, skipped };
  }

  return { consoleLogs, skipped };
}

// ── pm object builder ──

function buildPmObject(
  responseData: any,
  responseStatus: number,
  stores: VariableStores,
  assertions: PostmanRuntimeResult['assertions']
) {
  return {
    response: {
      json: () => responseData,
      text: () => responseData != null ? JSON.stringify(responseData) : '',
      code: responseStatus,
      status: responseStatus,
      to: {
        have: {
          status: (code: number) => {
            if (responseStatus !== code) {
              throw new Error(`expected response to have status code ${code} but got ${responseStatus}`);
            }
          },
        },
      },
    },
    environment: {
      set: (key: string, value: any) => { stores.environment[key] = String(value ?? ''); },
      get: (key: string) => stores.environment[key] ?? '',
      has: (key: string) => key in stores.environment,
    },
    collectionVariables: {
      set: (key: string, value: any) => { stores.collection[key] = String(value ?? ''); },
      get: (key: string) => stores.collection[key] ?? '',
      has: (key: string) => key in stores.collection,
    },
    globals: {
      set: (key: string, value: any) => { stores.environment[key] = String(value ?? ''); },
      get: (key: string) => stores.environment[key] ?? '',
    },
    // pm.variables — transient request-scoped variables (resolve before environment)
    variables: {
      set: (key: string, value: any) => { stores.collection[key] = String(value ?? ''); },
      get: (key: string) => stores.collection[key] ?? stores.environment[key] ?? '',
      has: (key: string) => key in stores.collection || key in stores.environment,
    },
    execution: {
      // Newman-native primitive. The dashboard runtime hides mock-only steps
      // at the UI layer, so this is a no-op here; we keep it defined so
      // collection-level prerequest scripts that call it don't throw.
      skipRequest: () => {},
    },
    info: { requestName: '' },
    request: { url: { toString: () => '' } },
    test: (name: string, fn: () => void) => {
      try {
        fn();
        assertions.push({ name, passed: true });
      } catch (e: any) {
        assertions.push({ name, passed: false, error: e.message });
      }
    },
    expect: (val: any) => createExpectChain(val),
  };
}

// ── Chai-like expect chain ──

function createExpectChain(val: any): any {
  let currentVal = val;
  let neg = false;

  const c: any = {};

  // Passthrough chainable words — return same object, no recursion
  for (const word of ['to', 'be', 'have', 'that', 'is', 'and', 'at']) {
    Object.defineProperty(c, word, { get: () => c, configurable: true });
  }
  Object.defineProperty(c, 'not', {
    get: () => { neg = !neg; return c; },
    configurable: true,
  });

  c.equal = (expected: any) => {
    const pass = currentVal === expected;
    if (neg ? pass : !pass) throw new Error(`expected ${JSON.stringify(currentVal)} ${neg ? 'not ' : ''}to equal ${JSON.stringify(expected)}`);
    return c;
  };
  c.eql = c.equal;

  c.status = (code: number) => {
    if (currentVal !== code) throw new Error(`expected response to have status code ${code} but got ${currentVal}`);
    return c;
  };

  c.property = (prop: string) => {
    const has = currentVal != null && typeof currentVal === 'object' && prop in currentVal;
    if (neg ? has : !has) throw new Error(`expected object ${neg ? 'not ' : ''}to have property '${prop}'`);
    currentVal = currentVal?.[prop];
    neg = false;
    return c;
  };

  Object.defineProperty(c, 'undefined', {
    get: () => {
      const isUndef = currentVal === undefined || currentVal === null;
      if (neg ? isUndef : !isUndef) throw new Error(`expected value ${neg ? 'not ' : ''}to be undefined`);
      return c;
    },
    configurable: true,
  });

  Object.defineProperty(c, 'null', {
    get: () => {
      const isNull = currentVal === null;
      if (neg ? isNull : !isNull) throw new Error(`expected value ${neg ? 'not ' : ''}to be null`);
      return c;
    },
    configurable: true,
  });

  Object.defineProperty(c, 'true', {
    get: () => {
      if (neg ? currentVal === true : currentVal !== true) throw new Error(`expected ${currentVal} ${neg ? 'not ' : ''}to be true`);
      return c;
    },
    configurable: true,
  });

  Object.defineProperty(c, 'false', {
    get: () => {
      if (neg ? currentVal === false : currentVal !== false) throw new Error(`expected ${currentVal} ${neg ? 'not ' : ''}to be false`);
      return c;
    },
    configurable: true,
  });

  Object.defineProperty(c, 'empty', {
    get: () => {
      const isEmpty = !currentVal || (Array.isArray(currentVal) && currentVal.length === 0) ||
        (typeof currentVal === 'string' && currentVal.length === 0);
      if (neg ? isEmpty : !isEmpty) throw new Error(`expected value ${neg ? 'not ' : ''}to be empty`);
      return c;
    },
    configurable: true,
  });

  Object.defineProperty(c, 'length', {
    get: () => {
      currentVal = Array.isArray(currentVal) ? currentVal.length : (currentVal?.length ?? 0);
      return c;
    },
    configurable: true,
  });

  c.above = (n: number) => {
    if (neg ? currentVal > n : currentVal <= n) {
      throw new Error(`expected ${currentVal} ${neg ? 'not ' : ''}to be above ${n}`);
    }
    return c;
  };

  c.below = c.lessThan = (n: number) => {
    if (neg ? currentVal < n : currentVal >= n) {
      throw new Error(`expected ${currentVal} ${neg ? 'not ' : ''}to be below ${n}`);
    }
    return c;
  };

  c.most = (n: number) => {
    if (neg ? currentVal <= n : currentVal > n) {
      throw new Error(`expected ${currentVal} ${neg ? 'not ' : ''}to be at most ${n}`);
    }
    return c;
  };

  c.least = (n: number) => {
    if (neg ? currentVal >= n : currentVal < n) {
      throw new Error(`expected ${currentVal} ${neg ? 'not ' : ''}to be at least ${n}`);
    }
    return c;
  };

  c.closeTo = c.approximately = (expected: number, delta: number) => {
    const pass = typeof currentVal === 'number' && Math.abs(currentVal - expected) <= delta;
    if (neg ? pass : !pass) {
      throw new Error(`expected ${currentVal} ${neg ? 'not ' : ''}to be close to ${expected} ± ${delta}`);
    }
    return c;
  };

  c.within = (start: number, finish: number) => {
    const pass = currentVal >= start && currentVal <= finish;
    if (neg ? pass : !pass) {
      throw new Error(`expected ${currentVal} ${neg ? 'not ' : ''}to be within ${start}..${finish}`);
    }
    return c;
  };

  c.a = c.an = (type: string) => {
    let pass: boolean;
    if (type === 'array') pass = Array.isArray(currentVal);
    else if (type === 'object') pass = typeof currentVal === 'object' && currentVal !== null;
    else if (type === 'string') pass = typeof currentVal === 'string';
    else if (type === 'number') pass = typeof currentVal === 'number';
    else pass = typeof currentVal === type;
    if (neg ? pass : !pass) throw new Error(`expected ${JSON.stringify(currentVal)?.slice(0, 50)} ${neg ? 'not ' : ''}to be ${type}`);
    return c;
  };

  c.include = c.includes = (item: any) => {
    const has = Array.isArray(currentVal) ? currentVal.includes(item) :
      typeof currentVal === 'string' ? currentVal.includes(item) : false;
    if (neg ? has : !has) throw new Error(`expected value ${neg ? 'not ' : ''}to include ${JSON.stringify(item)}`);
    return c;
  };

  c.oneOf = (list: any[]) => {
    const pass = list.includes(currentVal);
    if (neg ? pass : !pass) throw new Error(`expected ${JSON.stringify(currentVal)} ${neg ? 'not ' : ''}to be one of ${JSON.stringify(list)}`);
    return c;
  };

  return c;
}
