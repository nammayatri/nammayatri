

let stringsModule;

export const stringsV2Lazy = () => {
    if (!stringsModule) {
        stringsModule = require('../Resource.Localizable.StringsV2OG/index.js');
    }
    return {
        getString: stringsModule.getString,
        getStringV2: stringsModule.getStringV2
    }
}