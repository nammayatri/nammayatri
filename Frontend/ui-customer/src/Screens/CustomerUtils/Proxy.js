// export const dynamicImport = function (func) {
//     return function (onError, onSuccess) {
//         const headID = document.getElementsByTagName("head")[0];
//         const newScript = document.createElement("script");
//         newScript.type = "text/javascript";
//         var url = "http://" + "192.168.1.36" + ":" + "8081";

//         newScript.onload = function () {
//             console.log("dynamic import file customerUtils " + func);
//             // import("./../../output/Screens.CustomerUtils.Flow/index.js").then(module => {
//             import("./../Screens.CustomerUtils.Flow").then(module => {
//                 console.log("onSuccess import " + func);
//                 onSuccess(module[func]);
//             }
//             ).catch(e => console.error("error in dynamic import CustomerUtils", e));

//             return function (cancelError, onCancelerError, onCancelerSuccess) {
//                 onCancelerSuccess();
//             };
//         };
//         newScript.src = url + '/dist/2.index_bundle.js';
//         console.log("inner script", newScript.innerHTML);
//         headID.appendChild(newScript);
//         // newScript.onload();
//     };
// };

export const dynamicImport = function (func) {
  return function (onError, onSuccess) {
    const headID = document.getElementsByTagName("head")[0];
    const newScript = document.createElement("script");
    newScript.type = "text/javascript";
    const url = "http://" + "192.168.1.34" + ":" + "8081";

    newScript.onload = function () {
      console.log("dynamic import file customerUtils " + func);
      import(/* webpackChunkName: "customerUtil" */"./../Screens.CustomerUtils.Flow").then(module => {
        console.log("onSuccess import " + func);
        onSuccess(module[func]);
      }
      ).catch(e => console.error("error in dynamicImport " + func, e));

      return function (cancelError, onCancelerError, onCancelerSuccess) {
        onCancelerSuccess();
      };
    };
    newScript.src = url + "/dist/customerUtil.index_bundle.js";
    // newScript.innerHTML = window.DUIGatekeeper.loadFileInDUI("0.index_bundle.js");
    console.log("inner script", newScript.innerHTML);
    headID.appendChild(newScript);
    // newScript.onload();
    // callImport();
  };
};