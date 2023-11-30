import { callbackMapper } from "presto-ui";

const JBridge = window.JBridge;

const activeTimers = {};

function instantGetTimer (fn , delay) {
  fn();
  window.timerId = setInterval( fn, delay );
  return window.timerId;
}
  
export const countDownImpl = function (countDownTime, id, cb, action) {
  if (activeTimers[id] != undefined) {
    clearInterval(parseInt(activeTimers[id]));
  }
  const callback = callbackMapper.map(function () {
    let countDownCounter = countDownTime;
    activeTimers[id] = instantGetTimer(function () {
      if (activeTimers[id] != undefined) {
        countDownCounter -= 1;
        if (countDownCounter <= 0) {
          clearInterval(parseInt(activeTimers[id]));
          cb(action(0)("EXPIRED")(id))();
        } else {
          cb(action(countDownCounter)("INPROGRESS")(id))();
        }
      }
    }, 1000);
  });
  window.callUICallback(callback);
}

export const clearTimer = function (a)
{
  clearInterval(parseInt(activeTimers[a]));
};

export const clearTimerWithId = function (id) {
  if (window.__OS == "IOS") {
    if (JBridge.clearTimerWithId) {
      JBridge.clearTimerWithId(id);
    }
    else if (JBridge.clearCountDownTimer) {
      JBridge.clearCountDownTimer();
    }
  }
  else {
    clearInterval(parseInt(activeTimers[id]));
  }
}

function getTwoDigitsNumber(number) {
  return number >= 10 ? number : "0" + number.toString();
}

const driverWaitingTimerId = null;

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
    const callback = callbackMapper.map(function () {
      let sec = startingTime;

      function convertInMinutesFromat() {
        sec++;
        const minutes = getTwoDigitsNumber(Math.floor(sec / 60));
        const seconds = getTwoDigitsNumber(sec - minutes * 60);
        const timeInMinutesFormat = minutes + " : " + seconds;
        cb(action(driverWaitingTimerId)(timeInMinutesFormat)(sec))();
      }
      if (activeTimers[driverWaitingTimerId]) clearInterval(activeTimers[driverWaitingTimerId]);
      activeTimers[driverWaitingTimerId] = setInterval(
        convertInMinutesFromat,
        1000
      );
    });
    window.callUICallback(callback);
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
      cb(action(seconds)(id)(timerStatus)(timerID))();
    });
    return JBridge.startCountDownTimerWithTime(time, interval, cdTimerId, callback);
  } 
}
  
export const clearAllTimers = function(unit) {
  while(activeTimers.length > 0){
    clearInterval(parseInt(activeTimers.pop()));
  }
}