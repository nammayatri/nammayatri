{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.OnSearchEvent where

import Kernel.Prelude
import Kernel.Types.Id

data OnSearchEvent = OnSearchEvent
  { id :: Id OnSearchEvent,
    bppId :: Text,
    messageId :: Text,
    errorCode :: Maybe Text,
    errorType :: Maybe Text,
    errorMessage :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)
