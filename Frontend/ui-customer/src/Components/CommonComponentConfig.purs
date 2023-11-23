module Components.CommonComponentConfig where

import Prelude
import Common.Types.App as Common
import Screens.Types as ST
import Components.PopUpModal as PopUpModal
import Components.SelectListModal as SelectListModal
import PrestoDOM.Types.DomAttributes (Corners(..)) as PTD
import Helpers.Utils (fetchImage, FetchImageFrom(..))
import PrestoDOM (Gravity(..), Margin(..), Visibility(..), Padding(..), Length(..))
import Data.Maybe as Mb
import Data.Array as DA
import Data.String as DS
import Engineering.Helpers.Commons as EHC
import Language.Strings (getString)
import Language.Types (STR(..))
import Common.Styles.Colors as Color
import MerchantConfig.Types 
import JBridge as JB
import Engineering.Helpers.MobilityPrelude

type ContentConfig = 
   { primaryText :: String,
     secondaryText :: String,
     imageUrl :: String,
     videoUrl :: String, 
     mediaType :: String,
     listViewArray :: Array String,
     videoId :: String
   }


accessibilityPopUpConfig :: Mb.Maybe ST.DisabilityT -> PurpleRideConfig -> PopUpModal.Config
accessibilityPopUpConfig selectedDisability purpleRideConfig = 
   let 
     config = PopUpModal.config
     popupData = getAccessibilityPopupData selectedDisability purpleRideConfig
     config' = config
       {
         gravity = CENTER,
         margin = MarginHorizontal 24 24 ,
         buttonLayoutMargin = Margin 16 0 16 20 ,
         primaryText {
           text = popupData.primaryText
         , margin = Margin 16 16 16 4 
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
         , visibility = if (isStrEmpty popupData.videoUrl) || (not (JB.supportsInbuildYoutubePlayer unit)) then VISIBLE else GONE
         , height = V 160
         , width = MATCH_PARENT
         , margin = Margin 16 20 16 0
         }
         , listViewArray = popupData.listViewArray
          , coverVideoConfig {
            visibility = if popupData.videoUrl /= "" && (JB.supportsInbuildYoutubePlayer unit) then VISIBLE else GONE 
          , height = V 200
          , width = MATCH_PARENT
          , padding = Padding 16 16 16 0
          , mediaType = popupData.mediaType
          , mediaUrl = popupData.videoUrl
          , id = popupData.videoId
          }
       }
   in config'
  

getAccessibilityPopupData :: Mb.Maybe ST.DisabilityT -> PurpleRideConfig -> ContentConfig
getAccessibilityPopupData pwdtype purpleRideConfig = 
   let accessibilityConfig' = accessibilityConfig Common.Config
   in case pwdtype of 
        Mb.Just disability -> case (disability.tag) of 
          "BLIND_LOW_VISION" -> accessibilityConfig'
                                                  { imageUrl = fetchImage FF_ASSET "ny_ic_blind_pickup",
                                                    videoUrl = purpleRideConfig.visualImpairmentVideo,
                                                    mediaType = "VideoLink",
                                                    listViewArray = [(getString VI_POINTER_1) , (getString VI_POINTER_2) , (getString GENERAL_DISABILITY_DESCRIPTION)],
                                                    videoId = "VisualImpairmentCoverVideo"
                                                  } 
          "HEAR_IMPAIRMENT" ->     accessibilityConfig'
                                                  { imageUrl = fetchImage FF_ASSET "ny_ic_deaf_pickup",
                                                    videoUrl = purpleRideConfig.hearingImpairmentVideo,
                                                    mediaType = "PortraitVideoLink",
                                                    listViewArray = [(getString HI_POINTER_1) , (getString HI_POINTER_2) , (getString GENERAL_DISABILITY_DESCRIPTION)],
                                                    videoId = "HearingImpairmentCoverVideo"
                                                  }
          "LOCOMOTOR_DISABILITY" -> accessibilityConfig'
                                                  { imageUrl = fetchImage FF_ASSET "ny_ic_locomotor_arrival",
                                                    videoUrl = purpleRideConfig.physicalImpairmentVideo,
                                                    mediaType = "PortraitVideoLink",
                                                    listViewArray = [(getString PI_POINTER_1) , (getString PI_POINTER_2) , (getString GENERAL_DISABILITY_DESCRIPTION)],
                                                    videoId = "PhysicalImpairmentCoverVideo"
                                                  }     
          _ ->     accessibilityConfig' 

        Mb.Nothing -> accessibilityConfig' 

accessibilityConfig :: Common.LazyCheck -> ContentConfig
accessibilityConfig _ = { primaryText : (getString ACCESSIBILITY_TEXT), secondaryText : (getString TO_CATER_YOUR_SPECIFIC_NEEDS), imageUrl : fetchImage FF_ASSET "ny_ic_disability_illustration", videoUrl : "", mediaType : "" ,listViewArray : [(getString GENERAL_DISABILITY_DESCRIPTION)], videoId : "GeneralDisabilityCoverVideo"}

accessibilityListConfig :: ST.DisabilityData -> String -> AppConfig -> SelectListModal.Config
accessibilityListConfig disabilityData otherDisability config = 
  let 
    selectedDisability = (disabilityData.disabilityOptionList DA.!! disabilityData.specialAssistActiveIndex)
    selectedDisabilityTag = case selectedDisability of 
              Mb.Just ( disability) -> disability.tag 
              Mb.Nothing -> ""
  in SelectListModal.config
        { 
          selectionOptions = getDisabilityList disabilityData.disabilityOptionList
          , primaryButtonTextConfig
            { secondText = getString SUBMIT
            , width = V (EHC.screenWidth unit - 40)

            }
          , activeIndex = Mb.Just disabilityData.specialAssistActiveIndex
          , activeReasonCode = case (disabilityData.disabilityOptionList DA.!! disabilityData.specialAssistActiveIndex) of 
                                  Mb.Just activeOption -> Mb.Just activeOption.tag
                                  _ -> Mb.Nothing
          , isLimitExceeded = false
          , isSelectButtonActive = if selectedDisabilityTag == "OTHER" then (DS.length (otherDisability) >= 3) else true
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
          , hint = if isStrEmpty disabilityData.editedDisabilityReason then "Enter nature of condition" else ""
          , config = config
          , hideOthers = false
          , topLeftIcon = true
          , showBgColor = true
          , editTextBgColor = Color.white900
          , defaultText = disabilityData.editedDisabilityReason
        }

getDisabilityList :: Array ST.DisabilityT ->  Array Common.OptionButtonList
getDisabilityList =  map \item ->
    { description: item.description
    , reasonCode : item.tag
    , textBoxRequired : item.tag == "OTHER"
    , subtext :  Mb.Nothing
    }