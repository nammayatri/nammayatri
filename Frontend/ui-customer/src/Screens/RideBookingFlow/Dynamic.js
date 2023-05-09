export const dHandler = function (func) {
  return function (onError, onSuccess) {
    const headID = document.getElementsByTagName("head")[0];
    const newScript = document.createElement("script");
    newScript.type = "text/javascript";
    const url = "http://" + "192.168.1.36" + ":" + "8081";

    newScript.onload = function () {
      console.log("dynamic import file");
      import("./../../output/Screens.HomeScreen.Handler").then(module => {
        console.log("onSuccess import dView");
        onSuccess(module[func]);
      }
      ).catch(e => console.error("error in dHandler", e));

      return function (cancelError, onCancelerError, onCancelerSuccess) {
        onCancelerSuccess();
      };
    };
    newScript.src = url + "/dist/1.index_bundle.js";
    console.log("inner script", newScript.innerHTML);
    headID.appendChild(newScript);
  };
};

// export const dView = function (func) {
//     return function (onError, onSuccess) {
//         const headID = document.getElementsByTagName("head")[0];
//         const newScript = document.createElement("script");
//         newScript.type = "text/javascript";
//         var url = "http://" + "192.168.1.36" + ":" + "8081";

//         newScript.onload = function () {
//             console.log("dynamic import file");
//             import("./../../output/Screens.HomeScreen.View").then(module => {
//                 console.log("onSuccess import dView");
//                 onSuccess(module[func]);
//             }
//             ).catch(e => console.error("error in dView", e));

//             return function (cancelError, onCancelerError, onCancelerSuccess) {
//                 onCancelerSuccess();
//             };
//         };
//         newScript.src = url + '/dist/1.index_bundle.js';
//         console.log("inner script", newScript.innerHTML);
//         headID.appendChild(newScript);
//     };
// };