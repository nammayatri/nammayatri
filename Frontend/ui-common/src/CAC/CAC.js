let currentContext = undefined;
const cac = {}

export const getCurrentContext = function () {
  return currentContext ? currentContext : "";
}

export const isContextChanged = function (newContext) {
  return currentContext ? currentContext == newContext : false;
}

export const storeConfigWithContextInCache = function(config, context) {
  cac[context] = config;
  currentContext = context;
  return cac[context];
}

export const getCACFromCacheForContext = function(context, nothing, just) {
  return Object.prototype.hasOwnProperty.call(cac,context) ?
    just(cac[context]) : nothing;
}
export const getExistingCAC = function(context) {
  return cac[context];
}