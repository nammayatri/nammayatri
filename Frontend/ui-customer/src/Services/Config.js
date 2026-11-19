export const environment = function () {
  return (window.__payload.payload.environment === "staging") ? "master" : window.__payload.payload.environment;
};

export const getMerchant = function () {
  return window.merchantID;
}
