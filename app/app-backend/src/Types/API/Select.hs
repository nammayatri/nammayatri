{-# LANGUAGE DerivingVia #-}

module Types.API.Select where

import Beckn.Prelude
import Domain.Types.SelectedQuote

newtype SelectListRes = SelectListRes
  { selectedQuotes :: [SelectedQuoteAPIEntity]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
