/* global postman */

import Ajv from '../ajv.js';

const Extend = Symbol.for('extend');

Object.assign(postman[Extend], {
  jsonSchema(store, schema, options) {
    const ajv = new Ajv(options);
    const validate = ajv.compile(schema);
    store.test.push(response => validate(store.response.body.json));
  },

  jsonSchemaNot(store, schema, options) {
    const ajv = new Ajv(options);
    const validate = ajv.compile(schema);
    store.test.push(response => !validate(store.response.body.json));
  },
});
