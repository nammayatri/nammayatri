/* global postman */

import chai from '../chai.js';

const Extend = Symbol.for('extend');

Object.assign(postman[Extend], {
  AssertionError: chai.AssertionError,

  expect(value) {
    return chai.expect(value);
  },
});
