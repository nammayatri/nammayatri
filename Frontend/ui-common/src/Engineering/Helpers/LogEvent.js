const JBridge = window.JBridge;

export const getLogDestination = function () {
  return window.appConfig.logs;
}

export const firebaseLogEventWithArrayOfKeyValueImpl = (payload) => {
  if (JBridge.firebaseLogEventWithArrayOfKeyValue) {
    console.log("Firebase Log Event with Array of Key Value", payload);
    JBridge.firebaseLogEventWithArrayOfKeyValue(JSON.stringify({event: payload.key, object: JSON.stringify(payload.object)}));
  }
}