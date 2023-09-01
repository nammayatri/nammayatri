module Resources.Accessibility.EN where

import Prelude
import Resources.Accessibility.Types (AccessibilityHintText(..))

getEnglishAccessibilityHints :: AccessibilityHintText -> String
getEnglishAccessibilityHints key = case key of  
    SEND_MESSAGE -> "Send message"
    BACK -> "Back"
    DRIVER_NAME -> "Driver name"
    VEHICLE_NUMBER -> "Vehicle number"
    CALL -> "Call"
    BUTTON -> "Button"