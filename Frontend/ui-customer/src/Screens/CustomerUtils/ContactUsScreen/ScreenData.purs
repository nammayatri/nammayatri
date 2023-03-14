module Screens.ContactUsScreen.ScreenData where

import Screens.Types(ContactUsScreenState)

initData :: ContactUsScreenState
initData = {
  data: {
    email : "",
    subject : "",
    description : "",
    bookingId : ""
  },
  props: {
    btnActive : false,
    isSubmitted : false
  }
}
