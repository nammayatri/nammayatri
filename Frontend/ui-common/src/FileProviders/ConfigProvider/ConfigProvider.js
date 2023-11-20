const JBridge = window.JBridge;
function isObject(item) {
  return (item && typeof item === "object" && !Array.isArray(item));
}

export const isDebugBuild = function () {
  if (JBridge.getDeviceInfo) {
    const sessionInfo = JSON.parse(JBridge.getDeviceInfo())
    return sessionInfo.package_name.includes(".debug") || sessionInfo.package_name.includes(".staging") ; 
  } else {
    return false;
  }
}

export const appendConfigToDocument = function (data) {
  const headID = document.getElementsByTagName("head")[0];
  console.log(headID)
  const newScript = document.createElement("script");
  newScript.type = "text/javascript";
  newScript.id = "ny-configuration";
  newScript.innerHTML = data;
  headID.appendChild(newScript);
  return window.getMerchantConfig();
}

export const mergeforegin = function (object) {
  let finalObject;
  function mergeObjects(obj1, obj2) {
    const returnObject = Object.assign({}, obj1);
    Object.keys(obj2).forEach(key => {
      if (isObject(obj2[key])) {
        if (!(key in returnObject))
          Object.assign(returnObject, {
            [key]: obj2[key]
          });
        else
          returnObject[key] = mergeObjects(returnObject[key], obj2[key]);
      } else {
        returnObject[key] = obj2[key];
      }
    });
    return returnObject;
  }

  for (let i = 0; i < object.length; i++) {
    let currentObject;
    if (isObject(object[i])) {
      currentObject = object[i];
    } else {
      currentObject = JSON.parse(object[i]);
    }
    if (!finalObject) {
      finalObject = Object.assign({}, currentObject);
      continue;
    } else {
      finalObject = mergeObjects(finalObject, currentObject);
    }
  }
  return finalObject;
}

export const loadInWindow = function (key, value) {
  try { // Checking whether it is a Stringified JSON
    window[key] = JSON.parse(value);
  } catch (err){
    window[key] = value;
  }
}

export const loadFileInDUI = function (fileName) {
  if (JBridge.loadFileInDUI) {
    return JBridge.loadFileInDUI(fileName);
  } else {
    return "";
  }
}

// JSON UTILS
export const stringifyJSON = function (obj) {
  let result;
  try{
    result = JSON.stringify(obj);
  } catch (errr) {
    result = ""
  }
  return result;
}