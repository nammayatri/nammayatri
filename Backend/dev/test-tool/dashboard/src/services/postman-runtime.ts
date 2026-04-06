/**
 * Minimal Postman Script Runtime
 *
 * Executes Postman test/prerequest scripts in the browser.
 * Only implements the subset of pm.* API used by our collections.
 */

export interface PostmanRuntimeResult {
  assertions: Array<{ name: string; passed: boolean; error?: string }>;
  consoleLogs: string[];
  error?: string;
}

export interface VariableStores {
  environment: Record<string, string>;
  collection: Record<string, string>;
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
 */
export function executeTestScript(
  script: string,
  responseData: any,
  responseStatus: number,
  stores: VariableStores
): PostmanRuntimeResult {
  const assertions: PostmanRuntimeResult['assertions'] = [];
  const consoleLogs: string[] = [];

  // Build the pm object
  const pm = buildPmObject(responseData, responseStatus, stores, assertions);

  // Build console capture
  const consoleObj = {
    log: (...args: any[]) => consoleLogs.push(args.map(a => typeof a === 'object' ? JSON.stringify(a) : String(a)).join(' ')),
    warn: (...args: any[]) => consoleLogs.push('[WARN] ' + args.join(' ')),
    error: (...args: any[]) => consoleLogs.push('[ERROR] ' + args.join(' ')),
  };

  try {
    // eslint-disable-next-line no-new-func
    // Legacy postman.* API (pre-pm era) — used by some older collections
    const postman = {
      setNextRequest: () => {},
      setEnvironmentVariable: (key: string, val: any) => { stores.environment[key] = String(val ?? ''); },
      getEnvironmentVariable: (key: string) => stores.environment[key] ?? '',
      setGlobalVariable: (key: string, val: any) => { stores.environment[key] = String(val ?? ''); },
      getGlobalVariable: (key: string) => stores.environment[key] ?? '',
      clearEnvironmentVariable: (key: string) => { delete stores.environment[key]; },
    };
    // Legacy responseBody / responseCode globals — used by older Postman scripts
    const responseBody = responseData != null ? JSON.stringify(responseData) : '';
    const responseCode = { code: responseStatus };
    const dummySetTimeout = (cb: () => void) => cb();
    // eslint-disable-next-line no-new-func
    const fn = new Function('pm', 'console', 'setTimeout', 'postman', 'responseBody', 'responseCode', 'JSON', script);
    fn(pm, consoleObj, dummySetTimeout, postman, responseBody, responseCode, JSON);
  } catch (e: any) {
    return { assertions, consoleLogs, error: e.message };
  }

  return { assertions, consoleLogs };
}

/**
 * Execute a Postman prerequest script (for collection-level variable init).
 */
export function executePrereqScript(
  script: string,
  stores: VariableStores
): { consoleLogs: string[]; error?: string } {
  const consoleLogs: string[] = [];
  const assertions: PostmanRuntimeResult['assertions'] = [];

  const pm = buildPmObject(null, 0, stores, assertions);
  const consoleObj = {
    log: (...args: any[]) => consoleLogs.push(args.join(' ')),
    warn: (...args: any[]) => consoleLogs.push('[WARN] ' + args.join(' ')),
    error: (...args: any[]) => consoleLogs.push('[ERROR] ' + args.join(' ')),
  };

  try {
    // eslint-disable-next-line no-new-func
    const fn = new Function('pm', 'console', 'setTimeout', 'Math', 'String', 'Date', 'JSON', 'postman', script);
    const dummySetTimeout = (cb: () => void) => cb();
    const postman = {
      setNextRequest: () => {},
      setEnvironmentVariable: (key: string, val: any) => { stores.environment[key] = String(val ?? ''); },
      getEnvironmentVariable: (key: string) => stores.environment[key] ?? '',
      setGlobalVariable: (key: string, val: any) => { stores.environment[key] = String(val ?? ''); },
      getGlobalVariable: (key: string) => stores.environment[key] ?? '',
      clearEnvironmentVariable: (key: string) => { delete stores.environment[key]; },
    };
    fn(pm, consoleObj, dummySetTimeout, Math, String, Date, JSON, postman);
  } catch (e: any) {
    return { consoleLogs, error: e.message };
  }

  return { consoleLogs };
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
  for (const word of ['to', 'be', 'have', 'that', 'is', 'and']) {
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

  c.below = (n: number) => {
    if (neg ? currentVal < n : currentVal >= n) {
      throw new Error(`expected ${currentVal} ${neg ? 'not ' : ''}to be below ${n}`);
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
