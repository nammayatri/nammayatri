var defaultPrimaryButtonConfig = {
    isGradient: false,
    gradient: [  ],
    loaderUrl: "primary_button_loader.json"
};
var defaultOthers = {
    otpRegex: "is your OTP for login to [A-Za-z]+ [A-Za-z]+ [A-Za-z]+",
    termsLink: "https://docs.google.com/document/d/1-oRR_oI8ncZRPZvFZEJZeCVQjTmXTmHA",
    privacyLink: "https://docs.google.com/document/d/128VU80K5E1iz-x6QnP1R127m_lwmDO3F"
};
var defaultNavigationAppConfig = {
    android: {
        query: "google.navigation:q=%f,%f",
        packageName: "com.google.android.apps.maps",
        walkQuery: "google.navigation:q=%f,%f&mode=w"
    },
    ios: {
        query: "http://maps.google.com///?saddr=&daddr=%@,%@&dirflg=d",
        walkQuery: "http://maps.google.com///?saddr=&daddr=%@,%@&dirflg=w",
        packageName: ""
    }
};
var defaultLoaderConfig = {
    color: "#2C2F3A"
};
var defaultGenericHeader = {
    backArrow: "ny_ic_chevron_left_white,https://assets.juspay.in/beckn/nammayatri/user/images/ny_ic_chevron_left_white.png"
};
var defaultFontConfig = {
    "default": "PlusJakartaSans",
    kannada: "NotoSansKannada",
    type: "Assets"
};
var defaultColors = {
    black800: "#454545",
    black900: "#2C2F3A",
    red: "#E55454"
};
var defaultAppData = {
    link: "",
    supportMail: "nammayatri.support@juspay.in",
    name: ""
};
export {
    defaultColors,
    defaultAppData,
    defaultPrimaryButtonConfig,
    defaultFontConfig,
    defaultLoaderConfig,
    defaultOthers,
    defaultGenericHeader,
    defaultNavigationAppConfig
};
