function isObject(item) {
  return (item && typeof item === 'object' && !Array.isArray(item));
}

export const appendConfigToDocument = function (data) {
  const headID = document.getElementsByTagName("head")[0];
  console.log(headID)
  const newScript = document.createElement("script");
  newScript.type = "text/javascript";
  newScript.id = "ny-customer-configuration";
  newScript.innerHTML = data;
  headID.appendChild(newScript);
  return window.getMerchantConfig();
}

export const mergeforegin = function (object) {
  var finalObject;
  for (var i = 0; i < object.length; i++) {
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
      Object.keys(currentObject).forEach(key => {
        if (isObject(currentObject[key])) {
          if (!(key in finalObject))
            Object.assign(finalObject, {
              [key]: currentObject[key]
            });
          else
            finalObject[key] = mergeforegin([finalObject[key], currentObject[key]]);
        } else {
          finalObject[key] = currentObject[key];
        }
      });
    }
  }
  return finalObject;
}

export const loadInWindow = function (key, value) {
  try { // Checking whether it is a Stringified JSON
    window[key] = JSON.parse(value);
  } catch {
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

export const stringifyJSON = function (obj) {
  let result;
  try{
    result = JSON.stringify(obj);
  } catch (errr) {
    result = ""
  }
  return result;
}