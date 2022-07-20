{-# LANGUAGE DerivingVia #-}

module Types.API.FarePolicy.FareProduct where

import Beckn.Prelude
import Domain.Types.FarePolicy.FareProduct

newtype ListFareProductsRes = ListFareProductsRes
  { list :: [FareProductAPIEntity]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema, ToJSON, FromJSON)

data UpdateFareProductReq = UpdateFareProductReq
  { enabled :: Bool,
    fareProductType :: FareProductType
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
