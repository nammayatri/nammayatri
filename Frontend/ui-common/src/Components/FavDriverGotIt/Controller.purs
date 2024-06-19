module Components.FavDriverGotIt.Controller where

import Components.PrimaryButton as PrimaryButton
import Data.Maybe
import Common.Types.App (LazyCheck(..), FeedbackAnswer)
import Language.Strings (getString)
import Language.Types (STR(..))
import MerchantConfig.Types
import Components.PrimaryButton as PrimaryButto
import PrestoDOM (Accessiblity(..), Visibility(..))

data Action = GotIt PrimaryButton.Action

type FavDriverConfig = 
  { primaryButtonConfig :: PrimaryButton.Config
  , showProfileImg :: Boolean
  , title :: String 
  , accessibility :: Accessiblity
  , closeImgVisible :: Visibility
  }

favdriverConfig :: FavDriverConfig
favdriverConfig = {
  primaryButtonConfig : PrimaryButton.config,
  showProfileImg : false, 
  title : "",
  accessibility : DISABLE,
  closeImgVisible : GONE
}