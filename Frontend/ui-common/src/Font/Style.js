export const getFontStyleFromConfig = function () {
  if (window.appConfig && window.appConfig.fontType) {
    return window.appConfig.fontType;
  }
  return "Assets";
}