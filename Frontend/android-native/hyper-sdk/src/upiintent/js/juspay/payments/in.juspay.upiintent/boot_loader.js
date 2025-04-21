window.bootLoaderVersion="1.0rc3_1";
JBridge.trackEvent("bootLoader_version" , ""+window.bootLoaderVersion );
window.session_id = guid();
JBridge.setSessionId(window.session_id);
JBridge.setSessionAttribute("sessionStartTime", new Date().getTime().toString())
var clientId = JSON.parse(JBridge.getSessionAttribute("bundleParams", "{}")).client_id;

//window.__BOOT_LOADER = {}
//
//window.__BOOT_LOADER["config_downloaded"] = function(params) {
//    console.log("config downloaded " + params);
//    if(params == "true")
//        loadConfig();
//}
//
//if(JBridge.getResourceByName("beta_assets") == "true") {
//    var url = "https://d3n85rao6710xg.cloudfront.net/ec-upi-sdk";
//    var folder = "beta";
//    var remotesVersion = JBridge.getResourceByName("remotes_version");
//
//    JBridge.renewFile(url + "/" + folder + "/" + remotesVersion + "/" + "v1-ec-upi-boot_loader.zip");
//    JBridge.renewFile(url + "/" + folder + "/" + "v1-ec-upi-config.zip", "config_downloaded");
//    JBridge.renewFile(url + "/" + folder + "/" + remotesVersion + "/" + "v1-ec_upi_index_bundle.zip");
//} else {
//    var url = "https://d3n85rao6710xg.cloudfront.net/ec-upi-sdk";
//    var folder = "release";
//    var remotesVersion = JBridge.getResourceByName("remotes_version");
//
//    JBridge.renewFile(url + "/" + folder + "/" + remotesVersion + "/" + "v1-ec-upi-boot_loader.zip");
//    JBridge.renewFile(url + "/" + folder + "/" + "v1-ec-upi-config.zip", "config_downloaded");
//    JBridge.renewFile(url + "/" + folder + "/" + remotesVersion + "/" + "v1-ec_upi_index_bundle.zip");
//}

function guid() {
    function s4() {
        return Math.floor((1 + Math.random()) * 0x10000)
                .toString(16)
                .substring(1);
    }
    return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
            s4() + '-' + s4() + s4() + s4();
}