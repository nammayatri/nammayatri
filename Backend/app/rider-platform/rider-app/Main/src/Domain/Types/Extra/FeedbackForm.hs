{-# LANGUAGE ApplicativeDo #-}

module Domain.Types.Extra.FeedbackForm where

import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Utils.Dhall (FromDhall)

-- Extra code goes here --
newtype CacheFeedbackFormConfig = CacheFeedbackFormConfig
  { configsExpTime :: Seconds
  }
  deriving (Generic, FromDhall)

type HasCacheFeedbackFormConfig r = HasField "cacheFeedbackFormConfig" r CacheFeedbackFormConfig
