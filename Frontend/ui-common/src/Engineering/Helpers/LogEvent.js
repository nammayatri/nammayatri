export const getLogDestination = function () {
    console.log(window.appConfig.logs);
    console.log("js function being called");
    return window.appConfig.logs;
}