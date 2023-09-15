module Components.CommonComponentConfig where

import Prelude
import Common.Types.App as Common
import Screens.Types as ST
import Components.PopUpModal as PopUpModal
import Components.SelectListModal as SelectListModal
import PrestoDOM.Types.DomAttributes (Corners(..)) as PTD
import Helpers.Utils (getAssetStoreLink, getCommonAssetStoreLink)
import PrestoDOM (Gravity(..), Margin(..), Visibility(..), Padding(..), Length(..))
import Data.Maybe as Mb
import Data.Array as DA
import Data.String as DS
import Engineering.Helpers.Commons as EHC
import Language.Strings (getString)
import Language.Types (STR(..))
import Common.Styles.Colors as Color
import MerchantConfig.Types 

type ContentConfig = 
   { primaryText :: String,
     secondaryText :: String,
     imageUrl :: String,
     listViewArray :: Array String
   }


accessibilityPopUpConfig :: Mb.Maybe ST.DisabilityT -> PopUpModal.Config
accessibilityPopUpConfig selectedDisability = 
   let 
     config = PopUpModal.config
     popupData = getAccessibilityPopupData selectedDisability
     config' = config
       {
         gravity = CENTER,
         margin = MarginHorizontal 24 24 ,
         buttonLayoutMargin = Margin 16 0 16 20 ,
         primaryText {
           text = popupData.primaryText
         , margin = Margin 16 24 16 4 
         , padding = Padding 0 0 0 0},
         secondaryText {
           text = popupData.secondaryText
         , padding = PaddingHorizontal 16 16
         , margin = MarginVertical 8 8
         , gravity = LEFT},
         option1 {
           text = getString GOT_IT
         , background = Color.black900
         , color = Color.yellow900
         },
         option2 {
           visibility = false
         },
         backgroundClickable = false,
         cornerRadius = (PTD.Corners 15.0 true true true true),
         coverImageConfig {
           imageUrl = popupData.imageUrl
         , visibility = VISIBLE
         , height = V 160
         , width = MATCH_PARENT
         , margin = Margin 16 20 16 0
         }
         , listViewArray = popupData.listViewArray
       }
   in config'
  

getAccessibilityPopupData :: Mb.Maybe ST.DisabilityT -> ContentConfig
getAccessibilityPopupData pwdtype  = 
   let accessibilityConfig' = accessibilityConfig Common.Config
   in case pwdtype of 
        Mb.Just disability -> case (disability.tag) of 
          "BLIND_LOW_VISION" -> accessibilityConfig'
                                                  { imageUrl = "ny_ic_blind_pickup," <> (getAssetStoreLink Common.FunctionCall) <> "ny_ic_blind_pickup.png",
                                                    listViewArray = [(getString VI_POINTER_1) , (getString VI_POINTER_2) , (getString GENERAL_DISABILITY_DESCRIPTION)]
                                                  } 
          "HEAR_IMPAIRMENT" ->     accessibilityConfig'
                                                  { imageUrl = "ny_ic_deaf_pickup," <> (getAssetStoreLink Common.FunctionCall) <> "ny_ic_deaf_pickup.png",
                                                    listViewArray = [(getString HI_POINTER_1) , (getString HI_POINTER_2) , (getString GENERAL_DISABILITY_DESCRIPTION)]
                                                  }
          "LOCOMOTOR_DISABILITY" -> accessibilityConfig'
                                                  { imageUrl = "ny_ic_locomotor_arrival," <> (getAssetStoreLink Common.FunctionCall) <> "ny_ic_locomotor_arrival.png",
                                                    listViewArray = [(getString PI_POINTER_1) , (getString PI_POINTER_2) , (getString GENERAL_DISABILITY_DESCRIPTION)]
                                                  }     
          _ ->     accessibilityConfig' 

        Mb.Nothing -> accessibilityConfig' 

accessibilityConfig :: Common.LazyCheck -> ContentConfig
accessibilityConfig _ = { primaryText : (getString ACCESSIBILITY_TEXT), secondaryText : (getString TO_CATER_YOUR_SPECIFIC_NEEDS), imageUrl : "ny_ic_disability_illustration," <> (getAssetStoreLink Common.FunctionCall) <> "ny_ic_disability_illustration.png", listViewArray : [(getString GENERAL_DISABILITY_DESCRIPTION)]}

accessibilityListConfig :: ST.DisabilityData -> AppConfig -> SelectListModal.Config
accessibilityListConfig disabilityData config = 
  SelectListModal.config
        { selectionOptions = getDisabilityList disabilityData.disabilityOptionList
        , primaryButtonTextConfig
          { secondText = getString SUBMIT
          , width = V (EHC.screenWidth unit - 40)

          }
        , activeIndex = Mb.Just disabilityData.specialAssistActiveIndex
        , activeReasonCode = case (disabilityData.disabilityOptionList DA.!! disabilityData.specialAssistActiveIndex) of 
                                Mb.Just activeOption -> Mb.Just activeOption.tag
                                _ -> Mb.Nothing
        , isLimitExceeded = false
        , isSelectButtonActive = if disabilityData.specialAssistActiveIndex == DA.length (disabilityData.disabilityOptionList) - 1 then (DS.length (disabilityData.editedDisabilityReason) >= 3) else true
        , headingTextConfig{
          text = (getString SPECIAL_ASSISTANCE)
        , gravity = LEFT
        }
        , primaryButtonVisibility = false
        , subHeadingTextConfig{
          gravity = LEFT
        , margin = MarginTop 36
        , padding = (PaddingHorizontal 24 24)
        , text = (getString SELECT_THE_CONDITION_THAT_IS_APPLICABLE)
        }
        , hint = if Mb.isNothing disabilityData.otherDisabilityReason then "Enter nature of condition" else ""
        , config = config
        , hideOthers = false
        , topLeftIcon = true
        , showBgColor = true
        , editTextBgColor = Color.white900
        , defaultText = Mb.fromMaybe "" disabilityData.otherDisabilityReason
        }

getDisabilityList :: Array ST.DisabilityT ->  Array Common.OptionButtonList
getDisabilityList =  map \item ->
    { description: item.description
    , reasonCode : item.tag
    , textBoxRequired : item.tag == "OTHER"
    , subtext :  Mb.Nothing
    }