const {
  JBridge
} = window;

const nestJSON = function (original) {
  const nested = {};

  for (const [key, value] of Object.entries(original)) {
    const parts = key.split(".");
    let current = nested;

    for (let i = 0; i < parts.length - 1; i++) {
      const part = parts[i];
      if (!current[part]) {
        current[part] = {};
      }
      current = current[part];
    }

    current[parts[parts.length - 1]] = value;
  }

  return nested;
};

export const initMeasuringDuration = function (key) {
  return function () {
    try {
      window.events = window.events || {};
      if (typeof window.events[key] === "undefined")
        window.events[key] = new Date();
    } catch (error) {
      console.log("Latency start catch block" + error);
    }
  };
};

export const endMeasuringDuration = function (key) {
  return function () {
    try {
      window.events = window.events || {};
      if (typeof window.events[key] === "object")
        window.events[key] =
          new Date().getTime() - window.events[key].getTime();
    } catch (error) {
      console.log("Latency end catch block" + error);
    }
  };
};

export const getEvents = function () {
  try {
    window.user_id = window.user_id || JBridge.getFromSharedPrefs("CUSTOMER_ID");
    if (typeof window === "object") {
      if (window.Aggregate && !window.Aggregate.pushOnce) {
        window.events.Aggregate = window.Aggregate;
        window.Aggregate.pushOnce = true;        
      }
      const events = Object.assign({}, window.events, {
        appVersion: window.version["app"],
        configVersion: window.version["configuration"],
        hyperCoreVersion: window.top.hyper_core_version,
        os: window.__OS,
        josBundleLoadTime:
          window.top.__osBundleLoadLogLine.os_bundle_load.bundle_load_latency,
        sdkVersion: window.__payload.sdkVersion,
        service: window.__payload.service,
        sessionId: window.session_id,
        userId: window.user_id,
      });
      return JSON.stringify(nestJSON(events));
    } else {
      return JSON.stringify({});
    }
  } catch (error) {
    console.log("Latency getEvents catch block" + error);
    return JSON.stringify({});
  }
};

export const addEventData = function (key) {
  return function (value) {
    return function () {
      try {
        if (
          typeof window.events[key] === "undefined" &&
          typeof value !== "undefined" &&
          value !== ""
        ) {
          window.events[key] = { value, timestamp: new Date() };
        }
      } catch (error) {
        console.log("Add event data catch block" + error);
      }
    };
  };
};

export const addEventAggregate = function (key) {
    return function () {
      try {
        window.events.Events = window.events.Events || {};
        if (typeof window.events.Events[key] === "undefined") {
          window.events.Events[key] = 1;
        } else {
          window.events.Events[key] += 1;
        }
      } catch (error) {
        console.log("Add event aggregate catch block" + error);
      }
  };
}