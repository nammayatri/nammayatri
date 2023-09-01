module Resources.Accessibility.Types where

import Prelude

data ElementType = Button 
                 | Link 
                 | Image 
                 | TextField 
                 | RadioButton 
                 | CheckBox 
                 | Dropdown
                 | Star
                 | RatingIndicator

data ElementState = Selected
                  | Unselected
                  | Disabled
                  | NoState
                 


data AccessibilityHintText = BACK
                           | DRIVER_NAME
                           | VEHICLE_NUMBER
                           | CALL
                           | SEND_MESSAGE
                           | BUTTON