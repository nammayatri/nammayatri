module Components.InAppOtpModal.Controller where

data Action = OnSelection String Int
            | OnClickDone String
            | AfterRender
            | OnClickBack String
            | OnclickTextBox Int
            | BackPressed


----------------------------------------------- InAppOtpModalState ---------------------------------------------
type InAppOtpModalState = { 
  keyList :: Array Keys,
  text :: String,
  pattern :: String,
  fontSize :: Int,
  focusIndex :: Int,
  otpIncorrect :: Boolean,
  otpAttemptsExceeded :: Boolean
}
 
type Keys = {
  keys :: Array String 
}