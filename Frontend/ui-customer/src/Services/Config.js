export const environment = function () {
    return (window.__payload.payload.environment === "staging") ? "master" : window.__payload.payload.environment;
};
