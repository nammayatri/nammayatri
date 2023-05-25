window.getMerchantConfig = function () {
    return JSON.stringify({
        primaryTextColor : "#FFFFFF",
        primaryBackground : "#03B9F5",
        merchantId : "PAYTM" ,
        searchLocationTheme : "#E3F4FC",
        estimateConfirmText : "Request a NammaYatri Ride",
        autoConfirmingLoaderColor : "#03B9F5",
        quoteListModelBackground : "#E3F4FC",
        quoteListModel : {
            backgroundColor : "#F5F9FE",
            textColor : "#2C2F3A",
            loaderColor : "#03B9F5"
        },
        profileBackground : "#81DFFA",
        profileName : "#101010",
        profileImage : "#012A72",
        feedbackBackground : "#D3D3D3",
        sideBarList : ["MyRides", "Favorites", "HelpAndSupport", "Language", "About"],
        otpBackground : "#F5F9FE",
        otpTextColor : "#101010",
        rateCardColor : "#00B8F5",
        nyBrandingVisibility: true,
        fontType : "System"
    })
}