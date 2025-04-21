window.bootLoaderVersion="1.0rc2_2";
window.godel_session_id = guid();
DUIGatekeeper.setSessionId(window.godel_session_id);

window.configLoaded = function(argument) {
  try {
    var configJson = JSON.parse(getConfigString());
    window.configJson = configJson;
  } catch(err) {
    console.error(err);
    window.configJson = {
      weblab: {
        GODEL:0
      }
    }
  }

  DUIGatekeeper.invokeFnInDUIWebview("trackConfigInfo", JSON.stringify(["version", version]));
  processWeblab();
  DUIGatekeeper.setConfig(JSON.stringify(window.configJson));
}

function guid() {
        function s4() {
                return Math.floor((1 + Math.random()) * 0x10000)
                        .toString(16)
                        .substring(1);
        }
        return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
                s4() + '-' + s4() + s4() + s4();
}

var processWeblab = function() {
  var readTimed = function(key) {
    var value = DUIGatekeeper.getDataFromSharedPrefs(key, "-1");
    var keyEntryTimeKey = "key_entry_time_" + key;
    var life = 24 * 60 * 60 * 1000;
    if(parseInt(DUIGatekeeper.getDataFromSharedPrefs(keyEntryTimeKey, "0")) + life > Date.now())
      return value;
    else
      return "-1";
  }

  var writeTimed = function(key, value) {
    var keyEntryTimeKey = "key_entry_time_" + key;
    DUIGatekeeper.addDataToSharedPrefs(key, value);
    DUIGatekeeper.addDataToSharedPrefs(keyEntryTimeKey, "" + Date.now());
  }


  var weblab = configJson.weblab;
  for(var key in weblab) {
    try {
      if(weblab[key] != "0" && weblab[key] != "1") {
        
        var value = parseFloat(weblab[key]);
        if (value < 0 || value > 1) {
          console.log("The value given for the feature in the the rules map is invalid: ", value);
          continue;
        }
        
        var randomNumber = readTimed(key);
        var isEnabled = "1";
        if (randomNumber != "-1") {
          isEnabled = (parseInt(randomNumber) <= parseInt(value * 100)) ? "1" : "0";
          console.log("Returning sticky data for feature: " + key + " value: " + isEnabled);
          configJson.weblab[key] = parseInt(isEnabled);
          DUIGatekeeper.invokeFnInDUIWebview("trackJsInfo", JSON.stringify(["experiment_" + key, randomNumber]));
          continue;
        }

        randomNumber = parseInt(Math.random() * 100);
        console.log("Computing probablistic data for feature: " + key);
        isEnabled = (randomNumber <= parseInt(value * 100)) ? "1" : "0";
        writeTimed(key, randomNumber);
        configJson.weblab[key] = parseInt(isEnabled);
        console.log("Returning data for feature: " + key + " value: " + isEnabled);
        DUIGatekeeper.invokeFnInDUIWebview("trackJsInfo", JSON.stringify(["experiment_" + key, randomNumber]));
      }
    } catch(err) {
      console.log("", err);
    }
  }
}

DUIGatekeeper.setSessionAttribute("sessionStartTime", new Date().getTime().toString())