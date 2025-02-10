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
      window.timeStamps[key] = window.timeStamps[key] || {}
      window.timeStamps[key]["start"] = Date.now();
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
      window.timeStamps[key] = window.timeStamps[key] || {}
      window.timeStamps[key]["end"] = Date.now();
      window.events = window.events || {};
      if (typeof window.events[key] === "object")
        window.events[key] =
          new Date().getTime() - window.events[key].getTime();
    } catch (error) {
      console.log("Latency end catch block" + error);
    }
  };
};

const processTimeStamp = () => {
  const result = {};
  Object.keys(window.timeStamps).filter((key) => {
    return window.timeStamps[key].end && window.timeStamps[key].start
  }).forEach((key) => {
    result[key] = window.timeStamps[key].end - window.timeStamps[key].start;
    delete window.timeStamps[key];
  })
  return result;
}

export const getEvents = function () {
  try {
    if (typeof window === "object") {
      if (window.Aggregate && !window.Aggregate.pushOnce) {
        window.events.Aggregate = window.Aggregate;
        window.Aggregate.pushOnce = true;        
      }
      const newEvents = processTimeStamp()
      const events = Object.assign({},  window.events, {
        appVersion: window.version["app"],
        configVersion: window.version["configuration"],
        hyperCoreVersion: window.version["assets_downloader"],
        assetDownloader: window.version["assets_downloader"],
        os: window.__OS,
        sdkVersion: window.__payload.sdkVersion,
        service: window.__payload.service,
        sessionId: window.session_id,
        "newEvents" : newEvents,
      }, newEvents);
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
