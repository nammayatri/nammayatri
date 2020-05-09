module Epass.External.FCM.Types where

import           Data.Aeson
import           Data.Map
import qualified Data.Text.Encoding as T
import           EulerHS.Prelude
import           Servant

data SubmitNotification =
  SubmitNotification
    { _token        :: Text
    , _notification :: Notification
    , _name         :: Text
    , __data        :: Map Text Text
    }
  deriving (Show, Generic)

instance ToJSON SubmitNotification where
  toJSON = genericToJSON $ stripAllLensPrefixOptions {omitNothingFields = True}

data Notification =
  Notification
    { title :: Text
    , body  :: Text
    , image :: Maybe Text
    } deriving (Generic, ToJSON, Show)

newtype SubmitNotificationResp =
  SubmitNotificationResp
    {
      name :: Text
    } deriving (Generic, FromJSON, Show)

newtype FAuth =
  FAuth
    { bearer :: Text
    } deriving (Generic, ToJSON)

instance ToHttpApiData FAuth where
  toHeader FAuth{..}= T.encodeUtf8 $ "Bearer " <> bearer

