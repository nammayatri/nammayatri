module Screens.AddVehicleDetailsScreen.ScreenData where
import Data.Maybe
import Screens.Types
initData :: AddVehicleDetailsScreenState
initData = {
    data: {
      vehicle_type : "",
      vehicle_model_name : "",
      vehicle_color : "",
      vehicle_registration_number : "",
      reEnterVehicleRegistrationNumber : "",
      rc_base64 : "",
      vehicle_rc_number : "",
      referral_mobile_number : "",
      rcImageID : "",
      errorMessage : "",
      dateOfRegistration : Nothing,
      dateOfRegistrationView : ""
    },
    props: {
      rcAvailable : false,
      vehicleTypes : [Sedan, Hatchback, SUV, Auto],
      openSelectVehicleTypeModal : false,
      openRegistrationModal : false,
      rc_name : "",
      input_data : "",
      enable_upload : true,
      openRCManual : false,
      openReferralMobileNumber : false,
      isValidState : false,
      btnActive : false,
      referralViewstatus : false,
      isEdit : false,
      isValid : false,
      limitExceedModal : false,
      errorVisibility : false,
      openRegistrationDateManual : false
    }
}