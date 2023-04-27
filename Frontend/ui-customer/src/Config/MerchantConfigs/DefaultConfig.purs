module MerchantConfigs.DefaultConfig where

import Config.Types
import Styles.Colors as Color

config :: AppConfig
config =
  {
    primaryTextColor :  "#FFFFFF",
    primaryBackground : "#03B9F5",
    merchantId : "PAYTM",
    searchLocationTheme : Color.black900,
    estimateConfirmText : "(getString REQUEST_RIDE)",
    autoConfirmingLoaderColor : "#03B9F5",
    quoteListModelBackground : Color.black900,
    quoteListModel : {
      backgroundColor : Color.black900,
      textColor : Color.black900,
      loaderColor : Color.black900
    }
  } 