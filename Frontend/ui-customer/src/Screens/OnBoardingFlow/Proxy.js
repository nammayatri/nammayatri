export const dynamicImport = function (func) {
    return function (onError, onSuccess) {
        const headID = document.getElementsByTagName("head")[0];
        const newScript = document.createElement("script");
        newScript.type = "text/javascript";
        var url = "http://" + "192.168.1.36" + ":" + "8081";

        newScript.onload = function () {
            console.log("dynamic import file");
            import("./../../output/Screens.OnBoardingFlow.Flow").then(module => {
                console.log("onSuccess import EnterMobileNumberScreen");
                onSuccess(module[func]);
            }
            ).catch(e => console.error("error in dHandler", e));

            return function (cancelError, onCancelerError, onCancelerSuccess) {
                onCancelerSuccess();
            };
        };
        newScript.src = url + '/dist/0.index_bundle.js';
        console.log("inner script", newScript.innerHTML);
        headID.appendChild(newScript);
    };
};