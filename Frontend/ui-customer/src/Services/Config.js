import { callbackMapper } from "presto-ui";

export const environment = function () {
  return window.configEnv
};

export const getMerchant = function () {
  return window.merchantID;
}