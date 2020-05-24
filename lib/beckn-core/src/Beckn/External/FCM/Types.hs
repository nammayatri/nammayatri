{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Beckn.External.FCM.Types where

import Beckn.Utils.TH
import EulerHS.Prelude

newtype FCMRecipientToken = FCMRecipientToken
  { getFCMRecipientToken :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''FCMRecipientToken

newtype FCMAuthToken = FCMAuthToken
  { getFCMAuthToken :: Text
  }
  deriving (Generic, Show)

deriveIdentifierInstances ''FCMAuthToken