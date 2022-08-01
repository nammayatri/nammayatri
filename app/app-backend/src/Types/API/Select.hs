{-# LANGUAGE DerivingVia #-}

module Types.API.Select where

import Beckn.Prelude
import Domain.Types.Quote

newtype SelectListRes = SelectListRes
  { selectedQuotes :: [QuoteAPIEntity]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
