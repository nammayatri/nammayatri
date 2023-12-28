/*

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
*/
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

export const evalJSString = function (data) {
  eval(data);
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
