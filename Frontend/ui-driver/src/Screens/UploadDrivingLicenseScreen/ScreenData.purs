module Screens.UploadDrivingLicenseScreen.ScreenData where
import Data.Maybe
import Screens.Types (UploadDrivingLicenseState)
initData :: UploadDrivingLicenseState
initData = {
      data: {
        imageFront : ""
      , imageBack : ""
      , imageNameFront : "imagefront.jpg"
      , imageNameBack : "imageback.jpg"
      , driver_license_number : ""
      , reEnterDriverLicenseNumber : ""
      , dob : ""
      , dobView : ""
      , imageIDFront : ""
      , imageIDBack : ""
      , rcVerificationStatus : ""
      , errorMessage : ""
      , dateOfIssue : Nothing
      , dateOfIssueView : ""
      , imageFrontUrl : ""
      },
      props: {
        openRegistrationModal : false
      , openLicenseManual : false    
      , input_data : ""   
      , clickedButtonType : "" 
      , openGenericMessageModal : false
      , errorVisibility : false
      , openDateOfIssueManual: false
      }
    }