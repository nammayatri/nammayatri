import {
  callbackMapper
} from "presto-ui";

const JBridge = window.JBridge;

const activeTimers = {};

let activeTimerIds = [];

function instantGetTimer(fn, id, delay) {
  const timerId = setInterval(fn, delay, id);
  return timerId;
}

export const countDownImpl = function (countDownTime, id, interval, cb, action) {
  activeTimerIds.push(id);
  if (countDownTime < interval) interval = countDownTime;
  if (activeTimers[id] != undefined) {
    clearInterval(activeTimers[id].id);
  }
  const handler = function (keyId) {
    const timer = activeTimers[keyId];
    if (timer) {
      timer.time = timer.time - timer.timerInterval;
      if (timer.time <= 0) {
        clearInterval(timer.id);
        activeTimers[keyId] = undefined;
        delete activeTimers[keyId];
        cb(action(0)("EXPIRED")(keyId))();
      } else {
        cb(action(timer.time)("INPROGRESS")(keyId))();
      }
    }
  }
  const timerId = instantGetTimer(handler, id, interval * 1000);
  const timer = {
    time: countDownTime,
    id: timerId,
    timerInterval: parseInt(interval)
  }
  activeTimers[id] = timer;
  handler(id);
}

export const clearTimerWithId = function (id) {
  if (window.__OS == "IOS") {
    if (JBridge.clearTimerWithId) {
      JBridge.clearTimerWithId(id);
    } else {
      if (JBridge.clearCountDownTimer) {
        JBridge.clearCountDownTimer();
      }
      if (JBridge.clearCountUpTimer && id == "countUpTimerId") {
        window.JBridge.clearCountUpTimer();
      }
    }
  } else {
    if (activeTimers[id] != undefined) {
      clearInterval(activeTimers[id].id);
      activeTimers[id] = undefined;
      delete activeTimers[id];
    }
  }
  activeTimerIds = activeTimerIds.filter((value) => {
    return value != id
  });
}

export const startTimerWithTimeV2Impl = function (time, cdTimerId, interval, cb) {
  activeTimerIds.push(cdTimerId);
  if (JBridge.startCountDownTimerWithTimeV2) {
    const callback = callbackMapper.map(function (seconds, timerStatus, timerID, diffTime) {
      cb(seconds)(timerStatus)(timerID)(parseFloat(diffTime))();
    });
    return JBridge.startCountDownTimerWithTimeV2(time, interval, cdTimerId, callback);
  }
}

export const clearAllTimers = function () {
  activeTimerIds.forEach((value) => {
    clearTimerWithId(value);
  })
}