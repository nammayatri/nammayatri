module Screens.AcknowledgementScreen.ScreenData where

import Common.Types.App as Common
import Data.Maybe as Maybe
import PrestoDOM as PrestoDOM
import Screens.Types (AcknowledgementScreenState, IllustrationType(..))

initData :: AcknowledgementScreenState
initData =
  { data:
      { illustrationAsset: ""
      , title: Maybe.Nothing
      , description: Maybe.Nothing
      , primaryButtonText: Maybe.Nothing
      , orderId: Maybe.Nothing
      , amount: ""
      }
  , props:
      { primaryButtonVisibility: PrestoDOM.VISIBLE
      , paymentStatus: Common.Success
      , illustrationType: Lottie
      }
  }
