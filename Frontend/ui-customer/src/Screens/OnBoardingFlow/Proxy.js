export const dynamicImport = function (func) {
    return function (onError, onSuccess) {
        if (document.getElementById("onBoarding")) {
            console.log("dynamic import function " + func + " already loaded");
            import(/* webpackChunkName: "onBoarding" */"./../../output/Screens.OnBoardingFlow.Flow").then(module => {
                console.log("onSuccess import EnterMobileNumberScreen");
                onSuccess(module[func]);
            }
            ).catch(e => console.error("error in dHandler", e));

            return function (cancelError, onCancelerError, onCancelerSuccess) {
                onCancelerSuccess();
            };
        } else {
            const headID = document.getElementsByTagName("head")[0];
            const newScript = document.createElement("script");
            newScript.type = "text/javascript";
            const url = "http://" + "192.168.1.34" + ":" + "8081";

            newScript.onload = function () {
                console.log("dynamic import file");
                import(/* webpackChunkName: "onBoarding" */"./../../output/Screens.OnBoardingFlow.Flow").then(module => {
                    console.log("onSuccess import EnterMobileNumberScreen");
                    onSuccess(module[func]);
                }
                ).catch(e => console.error("error in dHandler", e));

                return function (cancelError, onCancelerError, onCancelerSuccess) {
                    onCancelerSuccess();
                };
            };
            // newScript.src = url + "/dist/onBoarding.index_bundle.js";
            newScript.innerHTML = JBridge.loadFileInDUI('2.index_bundle.js');
            headID.appendChild(newScript);
            newScript.onload();
        }
    };
};