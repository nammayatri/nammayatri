window.getMerchantConfig = function () {
    return JSON.stringify({
        primaryTextColor : "#FFFFFF",
        primaryBackground : "#00B8F5",
        merchantId : "PAYTM" ,
        searchLocationTheme : "#F5F9FE",
        estimateConfirmText : "Request a NammaYatri Ride",
        autoConfirmingLoaderColor : "#03B9F5",
        quoteListModelBackground : "#F5F9FE",
        quoteListModel : {
            backgroundColor : "#F5F9FE",
            textColor : "#2C2F3A",
            loaderColor : "#03B9F5"
        },
        profileBackground : "#00B8F5",
        profileName : "#FFFFFF",
        profileImage : "#012A72",
        feedbackBackground : "#D3D3D3",
        sideBarList : ["MyRides", "Favorites", "HelpAndSupport", "Language", "About"],
        otpBackground : "#F5F9FE",
        otpTextColor : "#101010",
        rateCardColor : "#00B8F5",
        nyBrandingVisibility: true,
        fontType : "System",
        merchantBrandingVisibility : false,
        confirmPickUpLocationBorder : "#101010",
        terminateBtnConfig : {
            visibility: true, 
            title : "Paytm",
            imageUrl : "ny_ic_chevron_left_double,https://assets.juspay.in/beckn/mobilitypaytm/mobilitypaytmcommon/ny_ic_chevron_left_double.png"
        }
    })
}