import { callbackMapper } from "presto-ui";

export const getPermissionStatusImpl = function(permission) {
  return function() {
    window.JBridge.checkPermission(permission);
  };
};

export const requestPermissionImpl = function(err, success, permissions) {
  return function() {
    const callback = callbackMapper.map(function(params) {
      console.warn("Permissions", params)
      success(JSON.stringify(params))();
    });
    console.log(permissions,typeof permissions)
    window.JBridge.requestPermission(permissions, "2", callback);
  };
};
