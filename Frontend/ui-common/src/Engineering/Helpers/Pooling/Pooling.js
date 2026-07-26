import { callbackMapper } from "presto-ui";
const JBridge = window.JBridge;

export const isTimerExist = function() {
  return JBridge.startCountDownTimerWithTimeV2 ? true : false;
}

export const startTimerForDelay = function (timerId, delay, cb) {
  if (JBridge.startCountDownTimerWithTimeV2) {
    const callback = callbackMapper.map(function () {
      cb()();
    });
    const interval = (delay / 1000.0).toString();
    return JBridge.startCountDownTimerWithTimeV2(interval,interval, timerId, callback);
  }
}