function isObject(item) {
    return (item && typeof item === 'object' && !Array.isArray(item));
}

function mergeDeep(target, source) {
    let output = Object.assign({}, target);
    if (isObject(target) && isObject(source)) {
        Object.keys(source).forEach(key => {
            if (isObject(source[key])) {
                if (!(key in target))
                    Object.assign(output, { [key]: source[key] });
                else
                    output[key] = mergeDeep(target[key], source[key]);
            } else {
                Object.assign(output, { [key]: source[key] });
            }
        });
    }
    return output;
}

const defaultConfig = {
    primaryTextColor : "#FFFFFF",
    primaryBackground : "#000000"
};

exports.loadConfig = function() {
    console.log("hey khuzi")
    if (window.appConfig) {
      return;
    }
    const headID = document.getElementsByTagName("head")[0];
    console.log(headID)
    const newScript = document.createElement("script");
    newScript.type = "text/javascript";
    newScript.id = "ny-customer-configuration";
    newScript.innerHTML = window.DUIGatekeeper.loadFileInDUI("v1-configuration.js");
    headID.appendChild(newScript);
    try {
        const merchantConfig = (
            function(){
                try {
                    return JSON.parse(window.getMerchantConfig());
                } catch(e){
                    return "{}";
                }
            }
        )();
        console.log(merchantConfig)
        window.appConfig = mergeDeep(defaultConfig, merchantConfig);
    } catch(e){
        console.error("config parse/merge failed", e);
        window.appConfig = defaultConfig;
    }
    console.log("bye khuzi")
}