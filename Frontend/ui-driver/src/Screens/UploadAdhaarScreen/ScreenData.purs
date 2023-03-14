module Screens.UploadAdhaarScreen.ScreenData where

import Screens.Types (UploadAdhaarScreenState)

initData :: UploadAdhaarScreenState
initData = {
      data: {
        imageFront : "",
        imageBack : "",
        imageName : "image.jpg"
      },
      props : {
        openRegistrationModal : false,
        clickedButtonType : ""
      }
    }
