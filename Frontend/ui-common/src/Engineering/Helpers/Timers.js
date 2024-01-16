import { callbackMapper } from "presto-ui";

const JBridge = window.JBridge;

const activeTimers = {};

function instantGetTimer(fn, id, delay) {
  const timerId = setInterval(fn, delay, id);
  return timerId;
}

export const countDownImpl = function (countDownTime, id, cb, action) {
  if (activeTimers[id] != undefined) {
    clearInterval(activeTimers[id].id);
  }
  const handler = function (keyId) {
    const timer = activeTimers[keyId];
    if (timer) {
      timer.time = timer.time -= 1;
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
  const timerId = instantGetTimer(handler, id, 1000);
  const timer = {
    time: countDownTime,
    id: timerId
  }
  activeTimers[id] = timer;
  handler(id);
}

export const clearTimerWithId = function (id) {
  if (window.__OS == "IOS") {
    if (JBridge.clearTimerWithId) {
      JBridge.clearTimerWithId(id);
    }
    else {
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
}

function getTwoDigitsNumber(number) {
  return number >= 10 ? number : "0" + number.toString();
}

export const waitingCountdownTimerV2Impl = function (startingTime, interval, timerId, cb, action) {
  if (window.__OS == "IOS") {
    if (JBridge.startCountUpTimerV2) {
      const callbackIOS = callbackMapper.map(function (timerID, sec) {
        const minutes = getTwoDigitsNumber(Math.floor(sec / 60));
        const seconds = getTwoDigitsNumber(sec - minutes * 60);
        const timeInMinutesFormat = minutes + " : " + seconds;
        cb(action(timerID)(timeInMinutesFormat)(sec))();
      });
      JBridge.startCountUpTimerV2(startingTime.toString(), interval, timerId, callbackIOS);
    }
    else if (JBridge.startCountUpTimer) {
      const callbackIOS = callbackMapper.map(function (timerID, sec) {
        const minutes = getTwoDigitsNumber(Math.floor(sec / 60));
        const seconds = getTwoDigitsNumber(sec - minutes * 60);
        const timeInMinutesFormat = minutes + " : " + seconds;
        cb(action(timerID)(timeInMinutesFormat)(sec))();
      });
      JBridge.startCountUpTimer(startingTime.toString(), callbackIOS);
    }
  } else {
    if (activeTimers[timerId] != undefined) {
      clearInterval(activeTimers[timerId].id);
    }
    const handler = function (keyId) {
      const timer = activeTimers[keyId];
      if (timer) {
        timer.time = timer.time + 1;
        const minutes = getTwoDigitsNumber(Math.floor(timer.time / 60));
        const seconds = getTwoDigitsNumber(timer.time - minutes * 60);
        const timeInMinutesFormat = minutes + " : " + seconds;
        cb(action(keyId)(timeInMinutesFormat)(timer.time))();
      }
    }
    const timerID = instantGetTimer(handler, timerId, 1000);
    const timer = {
      time: startingTime,
      id: timerID
    }
    activeTimers[timerId] = timer;
    handler(timerId);
  }
}

export const startTimerWithTimeV2Impl = function (time, cdTimerId, interval, cb, action) {
  if (JBridge.startCountDownTimerWithTimeV2) {
    const callback = callbackMapper.map(function (seconds, timerStatus, timerID) {
      cb(action(seconds)(timerStatus)(timerID))();
    });
    return JBridge.startCountDownTimerWithTimeV2(time, interval, cdTimerId, callback);
  }
  if (JBridge.startCountDownTimerWithTime) {
    const callback = callbackMapper.map(function (seconds, id, timerStatus, timerID) {
      cb(action(seconds)(timerStatus)(timerID))();
    });
    return JBridge.startCountDownTimerWithTime(time, interval, cdTimerId, callback);
  }
}