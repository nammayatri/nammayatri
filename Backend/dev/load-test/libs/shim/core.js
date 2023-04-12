import k6 from 'k6';
import http from 'k6/http';

/* Load dynamic variables */
import { dynamicGenerators, dynamicGeneratorsRegex } from './dynamic.js';

/* Constants */
const undef = void 0; /* eslint-disable-line no-void */

/* Symbols */
const Assign = Symbol('assign');
const Clear = Symbol('clear');
const Define = Symbol.for('define');
const Extend = Symbol.for('extend');
const Has = Symbol('has');
const Initial = Symbol.for('initial');
const Iteration = Symbol.for('iteration');
const Post = Symbol.for('post');
const Pre = Symbol.for('pre');
const Request = Symbol.for('request');
const Reset = Symbol.for('reset');
const Var = Symbol.for('variable');
const Write = Symbol('write');

/* Expressions */
const expression = {
  variableStart: /^{{.+}}/,
  variables: /{{(.*?)}}/g,
};

String.prototype.has = function(v) {
  return this.toString().indexOf(v) !== -1;
};

/*
 * Functional manipulation of frozen dicts
 *
 * These objects are frozen for consistency with Postman readonly semantics.
 * The constructor property is forced clear to keep it as clean as possible.
 *
 * Ideally these objects would have no prototype but it seems impossible.
 * The following methods fail:
 *
 * - Object.create(null) - Throws a TypeError on access of created object:
 *   Could not convert &{Object 0xc420ba9980 <nil> true map[] []} to primitive
 * - Object.setPrototypeOf() - Not implemented.
 * - Reflect.setPrototypeOf() - Reports success but prototype persists.
 * - obj.__proto__ - Not available. Access returns undefined.
 * - Proxy intercepting property access - Proxies not implemented.
 */
class Dict {
  constructor(values = {}) {
    this.constructor = undef;
    Object.assign(this, values);
    Object.freeze(this);
  }

  [Assign](updates) {
    const values = Object.assign({}, this);
    Object.assign(values, updates);
    return new Dict(values);
  }

  [Clear](key) {
    const values = Object.assign({}, this);
    delete values[key];
    return new Dict(values);
  }

  [Has](key) {
    return this[key] !== undef;
  }

  [Write](key, value) {
    const values = Object.assign({}, this);
    values[key] = value;
    return new Dict(values);
  }
}

/* State */
const state = {
  initialized: false,
  collection: false,
  environment: false,
  data: false,
  request: false,
  post: false,
  test: false,
};
const scope = {
  global: new Dict(),
  collection: new Dict(),
  environment: new Dict(),
  data: new Dict(),
  local: new Dict(),
};
const data = {
  file: [],
};
const store = {
  request: {
    data: Object.freeze({}),
    headers: Object.freeze({}),
    name: null,
    id: null,
    method: null,
    url: null,
  },
  response: {
    body: {
      text: null,
      json: null,
    },
    code: null,
    cookies: {
      list: [],
      dict: {},
      simple: {},
    },
    headers: {
      cased: {},
      uncased: {},
    },
    time: null,
  },
  test: [],
};
const definition = {
  request: {},
};
const setting = {
  options: {},
};
const surface = {
  pm: null,
  response: null,
  tests: {},
};
const standard = {
  require: global.require,
};

/* State validation */
function requireEnvironment() {
  if (!state.environment) {
    throw new Error('Missing Postman environment');
  }
}
function requireRequest() {
  if (!state.request) {
    throw new Error('May only be used in a request scope');
  }
}
function requirePost() {
  if (!state.post) {
    throw new Error('May only be used in a postrequest script');
  }
}

/* Dynamic variables */
const dynamic = {
  /* Version 4 GUID */
  guid() {
    return guid();
  },
  /* Random integer [0,1000] */
  randomInt() {
    return Math.floor(Math.random() * 1001);
  },
  /* Current time as Unix timestamp */
  timestamp() {
    return Date.now();
  },
};

function computeDynamic(name) {
  const functionName = '$' + name;
  if (dynamicGenerators[functionName]) {
    return dynamicGenerators[functionName].generator();
  } else {
    throw new Error(`Unsupported dynamic variable: ${name}`);
  }
}

/* postman.* */
const postman = Object.freeze({
  clearEnvironmentVariable(name) {
    requireEnvironment();
    pm.environment.unset(name);
  },
  clearEnvironmentVariables() {
    requireEnvironment();
    pm.environment.clear();
  },
  clearGlobalVariable(name) {
    pm.globals.unset(name);
  },
  clearGlobalVariables() {
    pm.globals.clear();
  },
  getEnvironmentVariable(name) {
    requireEnvironment();
    return pm.environment.get(name);
  },
  getGlobalVariable(name) {
    return pm.globals.get(name);
  },
  getResponseCookie(name) {
    requirePost();
    return store.response.cookies.dict[name];
  },
  getResponseHeader(name) {
    requirePost();
    return store.response.headers.uncased[name.toLowerCase()];
  },
  setEnvironmentVariable(name, value) {
    requireEnvironment();
    pm.environment.set(name, value);
  },
  setGlobalVariable(name, value) {
    pm.globals.set(name, value);
  },
  setNextRequest() {
    throw new Error('postman.setNextRequest not supported');
  },

  /*
   * Reset shim
   *
   * Clears to preinitialized state. For use in tests.
   */
  [Reset]() {
    state.initialized = false;
    state.collection = false;
    state.environment = false;
    state.data = false;
    state.scope = false;
    state.post = false;
    scope.global = new Dict();
    setting.options = {};
    scope.collection = new Dict();
    scope.environment = new Dict();
    scope.data = new Dict();
    scope.local = new Dict();
    definition.request = {};
    exposePm();
  },

  /*
   * Initialize shim
   *
   * Only necessary if k6 options are defined or setting initial variable
   * values. May only be called once. Must be called before other methods.
   * Shim behavior is undefined if called after other methods.
   */
  [Initial](initial = {}) {
    if (state.initialized) {
      throw new Error('Scope already initialized');
    }
    state.initialized = true;
    if ('options' in initial) {
      setting.options = initial.options;
    }
    if ('global' in initial) {
      scope.global = scope.global[Assign](initial.global);
    }
    if ('collection' in initial) {
      state.collection = true;
      scope.collection = scope.collection[Assign](initial.collection);
    }
    if ('environment' in initial) {
      state.environment = true;
      scope.environment = scope.environment[Assign](initial.environment);
    }
    if ('data' in initial) {
      state.data = true;
      data.file = [...initial.data].reverse();
    }
    exposePm();
  },

  /*
   * Initialize test iteration
   *
   * Advances to next row of data variables. Stays on final row after end.
   */
  [Iteration]() {
    if (data.file.length) {
      const row = data.file.pop();
      scope.data = new Dict(row);
    }
  },

  /* Outer scope prerequest scripts */
  [Pre]: [],

  /* Outer scope postrequest scripts */
  [Post]: [],

  /**
   * Execute named or specified request
   *
   * To execute a named request pass name and optionally a 1 based index.
   * To execute an ad hoc specified request pass an options object.
   *
   * @param {string|RequestParams}
   * @param {number} [index] - Named request index.
   * @return k6 response.
   */
  [Request](...args) {
    switch (typeof args[0]) {
      case 'object':
        return executeRequest(...args);
      case 'string':
        return namedRequest(...args);
      default:
        throw new Error('Invalid postman[Request] params');
    }
  },

  /* Define request */
  [Define](spec) {
    if (!spec.name) {
      throw new Error('Attempted to define request without name');
    }
    if (!definition.request[spec.name]) {
      definition.request[spec.name] = [];
    }
    definition.request[spec.name].push(spec);
  },

  /* Extension point. Enables selective loading of external libraries. */
  [Extend]: {
    AssertionError: null,
    expect() {
      throw new Error('To use pm.expect import "./libs/shim/expect.js"');
    },
    jsonSchema() {
      throw new Error('To use JSON schema import "./libs/shim/jsonSchema.js"');
    },
    jsonSchemaNot() {
      throw new Error('To use JSON schema impot "./libs/shim/jsonSchema.js"');
    },
    module: {},
  },
});

/* pm.* */
const pm = Object.freeze({
  /* Cookies */
  cookies: Object.freeze({
    get(name) {
      const cookie = store.response.cookies.dict[name];
      return cookie ? cookie.value : null;
    },
    has(name) {
      return name in store.response.cookies.dict;
    },
    toObject() {
      return store.response.cookies.simple;
    },
  }),

  /* Environment variables */
  environment: Object.freeze({
    clear() {
      scope.environment = new Dict();
    },
    get(name) {
      return scope.environment[name];
    },
    has(name) {
      return scope.environment[Has](name);
    },
    set(name, value) {
      scope.environment = scope.environment[Write](name, value);
    },
    toObject() {
      return Object.assign({}, scope.environment);
    },
    unset(name) {
      scope.environment = scope.environment[Clear](name);
    },
  }),

  /* Value assertions */
  expect(...args) {
    return postman[Extend].expect(...args);
  },

  /* Global variables */
  globals: Object.freeze({
    clear() {
      scope.global = new Dict();
    },
    get(name) {
      return scope.global[name];
    },
    has(name) {
      return scope.global[Has](name);
    },
    set(name, value) {
      scope.global = scope.global[Write](name, value);
    },
    toObject() {
      return Object.assign({}, scope.global);
    },
    unset(name) {
      scope.global = scope.global[Clear](name);
    },
  }),

  /* Runtime information */
  info: Object.freeze({
    get eventName() {
      if (state.post) {
        return 'test';
      } else {
        return 'prerequest';
      }
    },
    get iteration() {
      return global.__ITER;
    },
    get iterationCount() {
      const value = demarshal.integer(setting.options.iterations);
      if (value === undef) {
        return Number.POSITIVE_INFINITY;
      } else {
        return value;
      }
    },
    get requestId() {
      return request.id;
    },
    get requestName() {
      return request.name;
    },
  }),

  /* Data variables */
  iterationData: Object.freeze({
    get(name) {
      return scope.data[name];
    },
    toObject() {
      return Object.assign({}, scope.data);
    },
  }),

  /* Request information */
  request: Object.freeze({
    get headers() {
      throw new Error('pm.request.headers not supported');
    },
    get url() {
      throw new Error('pm.request.url not supported');
    },
  }),

  /* Response information */
  response: Object.freeze({
    get code() {
      return store.response.code;
    },
    get headers() {
      throw new Error('pm.response.headers not supported');
    },
    json() {
      parseBodyJson();
      const body = store.response.body;
      if (body.json === false) {
        throw new Error('JSON parsing of body failed');
      }
      return body.json;
    },
    reason() {
      throw new Error('pm.response.reason: Response reason unavailable in k6');
    },
    get responseTime() {
      return store.response.time;
    },
    text() {
      return store.response.body.text;
    },
    get to() {
      return state.test ? to : undef;
    },
  }),

  /* Web transactions */
  sendRequest() {
    throw new Error('pm.sendRequest not supported');
  },

  /* Test definition */
  test(name, logic) {
    if (state.test) {
      throw new Error('Nested pm.test calls not allowed');
    }
    try {
      enterTest();
      logic();
      k6.check(store.response, {
        [name]: response => {
          for (const test of store.test) {
            if (!test(response)) {
              return false;
            }
          }
          return true;
        },
      });
    } catch (error) {
      const AssertionError = postman[Extend].AssertionError;
      if (AssertionError && error instanceof AssertionError) {
        k6.check(null, { [name]: () => false });
      } else {
        throw error;
      }
    } finally {
      exitTest();
    }
  },

  /* General variable access */
  variables: Object.freeze({
    get(name) {
      if (scope.local[Has](name)) {
        return scope.local[name];
      }
      if (scope.data[Has](name)) {
        return scope.data[name];
      }
      if (scope.environment[Has](name)) {
        return scope.environment[name];
      }
      if (scope.collection[Has](name)) {
        return scope.collection[name];
      }
      if (scope.global[Has](name)) {
        return scope.global[name];
      }

      return undef;
    },
    set(name, value) {
      requireRequest();
      scope.local = scope.local[Write](name, value);
    },
    replaceIn(template) {
      if (dynamicGeneratorsRegex.test(template)) {
        // Replace dynamic variables with a generated value
        return template.replace(dynamicGeneratorsRegex, function(match) {
          // We remove the enclosing {{ }} to extract the generator type like "$randomName"
          const generatorType = match.slice(2, -2);
          return dynamicGenerators[generatorType].generator();
        });
      } else if (expression.variables && expression.variables.test(template)) {
        // Replace regular variables with a Postman value
        return template.replace(expression.variables, (match, name) =>
          pm[Var](name)
        );
      }
      return template;
    },
  }),

  collectionVariables: Object.freeze({
    clear() {
      scope.collection = new Dict();
    },
    get(name) {
      return scope.collection[name];
    },
    set(name, value) {
      requireRequest();
      scope.collection = scope.collection[Write](name, value);
    },
    unset(name) {
      scope.collection = scope.collection[Clear](name);
    },
  }),

  /**
   * Evaluate variable
   *
   * For use in string interpolation. Computes dynamic variables.
   * Reads simple variables from full scope hierarchy.
   *
   * @example
   * const Var = Symbol.for('variable')
   * const address = `${pm[Var]('protocol')}://${pm[Var]('domain')}/index.html`
   */
  [Var](name) {
    if (name[0] === '$') {
      return computeDynamic(name.substring(1));
    } else {
      return this.variables.get(name);
    }
  },
});
function exposePm() {
  const exposed = {
    environment: pm.environment,
    globals: pm.globals,
    info: pm.info,
    request: pm.request,
    sendRequest: pm.sendRequest,
    variables: pm.variables,
    collectionVariables: pm.collectionVariables,
    [Var]: pm[Var],
  };
  if (state.data) {
    Object.assign(exposed, {
      iterationData: pm.iterationData,
    });
  }
  if (state.post) {
    Object.assign(exposed, {
      cookies: pm.cookies,
      response: pm.response,
      test: pm.test,
    });
  }
  if (state.test) {
    Object.assign(exposed, {
      expect: pm.expect,
    });
  }
  Object.freeze(exposed);
  surface.pm = exposed;
}

/* Test assertions */
const to = Object.freeze({
  /* response.to.be */
  be: Object.freeze({
    get accepted() {
      // Response code 202
      store.test.push(response => response.code === 202);
    },
    get badRequest() {
      // Response code 400
      store.test.push(response => response.code === 400);
    },
    get clientError() {
      // Response code 4xx
      store.test.push(response => ((response.code / 100) | 0) === 4);
    },
    get error() {
      // Response code 4xx|5xx
      store.test.push(response => [4, 5].includes((response.code / 100) | 0));
    },
    get forbidden() {
      // Response code 403
      store.test.push(response => response.code === 403);
    },
    get info() {
      // Response code 1xx
      store.test.push(response => ((response.code / 100) | 0) === 1);
    },
    get notFound() {
      // Response code 404
      store.test.push(response => response.code === 404);
    },
    get ok() {
      // Response code 200
      store.test.push(response => response.code === 200);
    },
    get rateLimited() {
      // Response code 429
      store.test.push(response => response.code === 429);
    },
    get redirection() {
      // Response code 3xx
      store.test.push(response => ((response.code / 100) | 0) === 3);
    },
    get serverError() {
      // Response code 5xx
      store.test.push(response => ((response.code / 100) | 0) === 5);
    },
    get success() {
      // Response code 2xx
      store.test.push(response => ((response.code / 100) | 0) === 2);
    },
    get unauthorized() {
      // Response code 401
      store.test.push(response => response.code === 401);
    },
  }),

  /* response.to.have */
  have: Object.freeze({
    body(value) {
      if (!value) {
        store.test.push(response => !!response.body.text);
      } else if (typeof value === 'string') {
        store.test.push(response => response.body.text === value);
      } else if (value instanceof RegExp) {
        store.test.push(response => value.test(response.body.text));
      } else {
        throw new Error('Unrecognized argument type');
      }
    },
    header(key, value) {
      if (arguments.length > 1) {
        store.test.push(
          response => response.headers.uncased[key.toLowerCase()] === value
        );
      } else {
        store.test.push(
          response => key.toLowerCase() in response.headers.uncased
        );
      }
    },
    jsonBody() {
      parseBodyJson();
      if (arguments.length === 0) {
        store.test.push(response => !!store.response.body.json);
      } else if (arguments.length === 1) {
        if (typeof arguments[0] === 'object') {
          const [expected] = arguments;
          store.test.push(response => deepEqual(response.body.json, expected));
        } else if (typeof arguments[0] === 'string') {
          const [path] = arguments;
          const value = jsonPath(store.response.body.json, path);
          store.test.push(response => !!value);
        } else {
          throw new Error('Unrecognized argument type');
        }
      } else {
        const [path, expected] = arguments;
        const value = jsonPath(store.response.body.json, path);
        store.test.push(response => value === expected);
      }
    },
    jsonSchema(...args) {
      parseBodyJson();
      postman[Extend].jsonSchema(store, ...args);
    },
    status(value) {
      switch (typeof value) {
        case 'number':
          store.test.push(response => response.code === value);
          break;
        case 'string':
          throw new Error('Response reason unavailable in k6');
        default:
          throw new Error(`Unrecognized argument type: ${typeof value}`);
      }
    },
  }),

  not: Object.freeze({
    /* response.to.not.be */
    be: Object.freeze({
      get accepted() {
        // Response code not 202
        store.test.push(response => response.code !== 202);
      },
      get badRequest() {
        // Response code not 400
        store.test.push(response => response.code !== 400);
      },
      get clientError() {
        // Response code not 4xx
        store.test.push(response => ((response.code / 100) | 0) !== 4);
      },
      get error() {
        // Response code not 4xx|5xx
        store.test.push(
          response => ![4, 5].includes((response.code / 100) | 0)
        );
      },
      get forbidden() {
        // Response code not 403
        store.test.push(response => response.code !== 403);
      },
      get info() {
        // Response code not 1xx
        store.test.push(response => ((response.code / 100) | 0) !== 1);
      },
      get notFound() {
        // Response code not 404
        store.test.push(response => response.code !== 404);
      },
      get ok() {
        // Response code not 200
        store.test.push(response => response.code !== 200);
      },
      get rateLimited() {
        // Response code not 429
        store.test.push(response => response.code !== 429);
      },
      get redirection() {
        // Response code not 3xx
        store.test.push(response => ((response.code / 100) | 0) !== 3);
      },
      get serverError() {
        // Response code not 5xx
        store.test.push(response => ((response.code / 100) | 0) !== 5);
      },
      get success() {
        // Response code not 2xx
        store.test.push(response => ((response.code / 100) | 0) !== 2);
      },
      get unauthorized() {
        // Response code not 401
        store.test.push(response => response.code !== 401);
      },
    }),

    /* response.to.not.have */
    have: Object.freeze({
      body(value) {
        if (!value) {
          store.test.push(response => !response.body.text);
        } else if (typeof value === 'string') {
          store.test.push(response => response.body.text !== value);
        } else if (value instanceof RegExp) {
          store.test.push(response => !value.test(response.body.text));
        } else {
          throw new Error('Unrecognized argument type');
        }
      },
      header(key, value) {
        if (arguments.length > 1) {
          store.test.push(
            response => response.headers.uncased[key.toLowerCase()] !== value
          );
        } else {
          store.test.push(
            response => !(key.toLowerCase() in response.headers.uncased)
          );
        }
      },
      jsonBody() {
        parseBodyJson();
        if (arguments.length === 0) {
          store.test.push(response => !response.body.json);
        } else if (arguments.length === 1) {
          if (typeof arguments[0] === 'object') {
            const [expected] = arguments;
            store.test.push(
              response => !deepEqual(response.body.json, expected)
            );
          } else if (typeof arguments[0] === 'string') {
            const [path] = arguments;
            const value = jsonPath(store.response.body.json, path);
            store.test.push(response => !value);
          } else {
            throw new Error('Unrecognized argument type');
          }
        } else {
          const [path, expected] = arguments;
          const value = jsonPath(store.response.body.json, path);
          store.test.push(response => value !== expected);
        }
      },
      jsonSchema(...args) {
        parseBodyJson();
        postman[Extend].jsonSchemaNot(store, ...args);
      },
      status(value) {
        switch (typeof value) {
          case 'number':
            store.test.push(response => response.code !== value);
            break;
          case 'string':
            throw new Error('Response reason unavailable in k6');
          default:
            throw new Error(`Unrecognized argument type: ${typeof value}`);
        }
      },
    }),
  }),
});

/* request */
const request = Object.freeze({
  get data() {
    return store.request.data;
  },
  get headers() {
    return store.request.headers;
  },
  get id() {
    return store.request.id;
  },
  get method() {
    return store.request.method;
  },
  get name() {
    return store.request.name;
  },
  get url() {
    return store.request.url;
  },
});

/* responseCode */
const responseCode = Object.freeze({
  get code() {
    return store.response.code;
  },
  get detail() {
    throw new Error('responseCode.detail: Response message unavailable in k6');
  },
  get name() {
    throw new Error('responseCode.name: Response message unavailable in k6');
  },
});

/* Conversion */
function xml2Json(xml) {
  throw new Error('To use xml2Json import ./libs/shim/xml2Json.js');
}
function xmlToJson(xml) {
  throw new Error('Deprecated function xmlToJson not supported');
}

/* Internal utility */
function parseBodyJson() {
  const body = store.response.body;
  if (body.text === null || body.json === false || body.json) {
    // No body or parsing failed or already parsed
    return;
  }
  try {
    body.json = JSON.parse(body.text);
  } catch (e) {
    body.json = false;
  }
}
function deepEqual(x, y) {
  // From https://stackoverflow.com/a/32922084/10086414
  const ok = Object.keys;
  const tx = typeof x;
  const ty = typeof y;
  return x && y && tx === 'object' && tx === ty
    ? ok(x).length === ok(y).length &&
        ok(x).every(key => deepEqual(x[key], y[key]))
    : x === y;
}
function jsonPath(value, path) {
  try {
    return eval(`value.${path}`); /* eslint-disable-line no-eval */
  } catch (e) {
    return undef;
  }
}

/* Standard library */
function require(id) {
  switch (id) {
    case 'lodash':
    case 'cheerio':
    case 'crypto-js':
      if (postman[Extend].module[id]) {
        return postman[Extend].module[id];
      } else {
        throw new Error(`To use module ${id} import ./libs/shim/${id}.js`);
      }
    default:
      throw new Error(
        `Can't load module '${id}', Node.js modules aren't supported in k6`
      );
  }
}

/* Template strings */
function evaluate(text) {
  return text.replace(expression.variables, (match, name) => pm[Var](name));
}

/* Request execution */
/**
 * @typedef {object} RequestParams
 * @param {string} name - Request name.
 * @param {string} [id] - Request ID. Random ID generated if not provided.
 * @param {string} method - Request method.
 * @param {string} address - Request address.
 * @param {string|object} [data] - Data for request body.
 * @param {object} [headers] - Request headers.
 * @param {object} [options] - k6 request options, except headers.
 * @param {function} [pre] - Prerequest logic.
 * @param {function} [auth] - Authentication logic.
 * @param {function} [post] - Postrequest logic. Receives k6 response.
 */
/**
 * Execute request with Postman semantics
 *
 * Executes a request and surrounding logic with a scoped set of local
 * variables. Exposes request specific API surface during execution. Exposes
 * test script specific API surface during test script execution. Executes
 * provided logic synchronously.
 *
 * @param {RequestParams} params
 * @return k6 response.
 */
function executeRequest({
  name,
  id = guid(),
  method,
  address,
  data,
  headers,
  options,
  tags,
  pre,
  auth,
  post,
}) {
  try {
    enterRequest(name, id, method, address, data, headers);
    executePrerequest(postman[Pre], pre);
    const config = makeRequestConfig(
      method,
      address,
      data,
      headers,
      options,
      tags
    );
    if (auth) {
      auth(config, Var);
    }
    const args = makeRequestArgs(config);
    const response = http.request(...args);
    if (post) {
      enterPost(response);
      executePostrequest(postman[Post], post, response);
      executeTests();
    }
    return response;
  } finally {
    exitRequest();
  }
}
function namedRequest(name, index = 1) {
  const requests = definition.request[name];
  if (!requests) {
    throw new Error(`No request with name '${name}'`);
  }
  if (!(requests.length >= index)) {
    throw new Error(`No request with name '${name}' and index ${index}`);
  }
  const config = requests[index - 1];
  return executeRequest(config);
}
function makeRequestConfig(method, address, data, headers, options, tags) {
  const config = {};
  config.method = method || null;
  config.address = address ? evaluateAddress(address) : null;
  if (typeof data === 'string') {
    config.data = evaluate(data);
  } else if (data) {
    evaluateDataObject(data);
    config.data = data;
  } else {
    config.data = {};
  }
  if (headers) {
    for (const key of Object.keys(headers)) {
      headers[key] = evaluate(headers[key]);
    }
    config.headers = headers;
  } else {
    config.headers = {};
  }

  config.options = options || {};
  if (tags && typeof tags === 'object') {
    config.options.tags = tags;
  }
  return config;
}

function evaluateAddress(address) {
  if (expression.variableStart.test(address)) {
    const evaluated = evaluate(address);
    return defaultProtocol(evaluated);
  } else {
    return evaluate(address);
  }
}
function defaultProtocol(addressText) {
  if (!postman[Extend].module.urijs) {
    throw new Error(
      'To use a variable at address start import "./libs/shim/urijs.js"'
    );
  }
  const URI = postman[Extend].module.urijs;
  const address = new URI(addressText);
  if (!address.protocol()) {
    address.protocol('http');
  }
  return address.toString();
}
function evaluateDataObject(data) {
  for (const key of Object.keys(data)) {
    if (typeof data[key] === 'string') {
      data[key] = evaluate(data[key]);
    }
  }
}
function makeRequestArgs({ method, address, data, headers, options }) {
  // Set K6 request params from setting
  if (setting.options) {
    options = Object.assign(setting.options, options);
  }
  let requestHeaders = headers;
  // Merge setting headers & request headers
  if (setting.options && setting.options.headers) {
    requestHeaders = Object.assign(setting.options.headers, headers);
  }
  options.headers = requestHeaders;
  return [method, address, data, options];
}
function enterRequest(name, id, method, address, data, headers) {
  state.request = true;
  store.request.id = id;
  if (name) {
    store.request.name = name;
  }
  if (method) {
    store.request.method = method.toUpperCase();
  }
  if (address) {
    store.request.url = address;
  }
  if (data && typeof data === 'object') {
    store.request.data = Object.freeze(Object.assign({}, data));
  }
  if (headers) {
    store.request.headers = Object.freeze(Object.assign({}, headers));
  }
  Object.assign(global, {
    require,
  });
}
function enterPost(response = {}) {
  state.post = true;
  Object.assign(store.response, {
    code: response.status,
    time: response.timings ? response.timings.duration : undef,
  });
  if (typeof response.body === 'string') {
    store.response.body.text = response.body;
  }
  if (response.headers) {
    Object.assign(store.response.headers.cased, response.headers);
    for (const name of Object.keys(response.headers)) {
      const value = response.headers[name];
      store.response.headers.uncased[name.toLowerCase()] = value;
    }
  }
  if (response.cookies) {
    translateCookies(response.cookies);
  }
  exposePm();
}
function enterTest() {
  state.test = true;
  exposePm();
}
function exitTest() {
  state.test = false;
  store.test = [];
  exposePm();
}
function exitRequest() {
  state.request = false;
  state.post = false;
  state.test = false;
  scope.local = new Dict();
  store.request = {
    data: Object.freeze({}),
    headers: Object.freeze({}),
    method: null,
    name: null,
    id: null,
    url: null,
  };
  store.response = {
    body: {
      text: null,
      json: null,
    },
    code: null,
    cookies: {
      list: [],
      dict: {},
      simple: {},
    },
    headers: {
      cased: {},
      uncased: {},
    },
    time: null,
  };
  exposePm();
  surface.tests = {};
  Object.assign(global, {
    require: standard.require,
  });
}
function translateCookies(cookies) {
  for (const key of Object.keys(cookies)) {
    // Postman semantics have no duplicate cookie names. Duplicates discarded.
    const cookie = translateCookie(cookies[key][0]);
    store.response.cookies.list.push(cookie);
    store.response.cookies.dict[cookie.name] = cookie;
    store.response.cookies.simple[cookie.name] = cookie.value;
  }
}
function translateCookie(cookie) {
  return {
    domain: cookie.domain,
    get hostOnly() {
      throw new Error('cookie.hostOnly not supported');
    },
    httpOnly: cookie.httpOnly,
    name: cookie.name,
    path: cookie.path,
    secure: cookie.secure,
    get session() {
      throw new Error('cookie.session not supported');
    },
    get storeId() {
      throw new Error('cookie.storeId not supported');
    },
    value: cookie.value,
  };
}
function executePrerequest(outer, inner) {
  const scripts = [...outer];
  if (inner) {
    scripts.push(inner);
  }
  for (const script of scripts) {
    script();
  }
}
function executePostrequest(outer, inner, response) {
  const scripts = [...outer];
  if (inner) {
    scripts.push(inner);
  }
  for (const script of scripts) {
    script(response);
  }
}

/* Tests */
function executeTests() {
  for (const description of Object.keys(surface.tests)) {
    const value = surface.tests[description];
    k6.check(null, { [description]: () => value });
  }
}

/* Utility */
function guid() {
  // Version 4 GUID
  // From https://stackoverflow.com/a/2117523/10086414
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, c => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

/* Messaging with k6 */
const demarshal = Object.freeze({
  integer(encoded) {
    if (encoded === undef) {
      return undef;
    } else {
      return parseInt(JSON.stringify(encoded));
    }
  },
});

/* Interface */
Object.defineProperties(global, {
  data: {
    get() {
      return state.data ? scope.data : undef;
    },
  },
  environment: {
    get() {
      return scope.environment;
    },
  },
  globals: {
    get() {
      return scope.global;
    },
  },
  iteration: {
    get() {
      return pm.info.iteration;
    },
  },
  pm: {
    get() {
      return surface.pm;
    },
  },
  request: {
    get() {
      return state.request ? request : undef;
    },
  },
  responseBody: {
    get() {
      return state.post ? store.response.body.text : undef;
    },
  },
  responseCode: {
    get() {
      return state.post ? responseCode : undef;
    },
  },
  responseCookies: {
    get() {
      return state.post ? store.response.cookies.list : undef;
    },
  },
  responseHeaders: {
    get() {
      return state.post ? store.response.headers.cased : undef;
    },
  },
  responseTime: {
    get() {
      return state.post ? store.response.time : undef;
    },
  },
  tests: {
    get() {
      return state.post ? surface.tests : undef;
    },
  },
});
Object.assign(global, {
  postman,
  xml2Json,
  xmlToJson,
});
exposePm();
