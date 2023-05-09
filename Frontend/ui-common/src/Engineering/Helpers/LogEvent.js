export const isFirebaseEnabledFFI = function (just) {
    return function (nothing) {
      return function () {
        if (typeof window.appConfig != "undefined" && typeof window.appConfig.isFireBaseEnabled === "boolean") {
          return just(window.appConfig.isFireBaseEnabled);
        }
        return nothing;
      }
    }
  }