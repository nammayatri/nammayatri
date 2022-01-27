{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.OnSearchEvent where

import Beckn.Prelude
import Beckn.Types.Id

data OnSearchEvent = OnSearchEvent
  { id :: Id OnSearchEvent,
    bppId :: Text,
    transactionId :: Text,
    errorCode :: Maybe Text,
    errorType :: Maybe Text,
    errorMessage :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Generic, Show)
