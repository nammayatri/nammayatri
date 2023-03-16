const callbackMapper = require('presto-ui').callbackMapper;

exports["getPermissionStatus'"] = function(permission) {
	return function() {
		JBridge.checkPermission(permission);
	};
};

exports["requestPermission'"] = function(err, success, permissions) {
	return function() {
		var callback = callbackMapper.map(function(params) {
			console.warn("Permissions", params)
			success(JSON.stringify(params))();
		});
		console.log(permissions,typeof permissions)
		JBridge.requestPermission(permissions, "2", callback);
	};
};
