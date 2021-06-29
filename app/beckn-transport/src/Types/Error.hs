{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Error (module Types.Error) where

import Beckn.Types.Error as Types.Error
import Beckn.Types.Error.BaseError.APIError
import EulerHS.Prelude

data FarePolicyError
  = NoFarePolicy
  | CantCalculateDistance
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instanceExceptionWithParent 'APIException ''FarePolicyError

instance IsBaseError FarePolicyError where
  toMessage NoFarePolicy = Just "No fare policy matches passed data."
  toMessage _ = Nothing

instance IsAPIError FarePolicyError where
  toErrorCode NoFarePolicy = "NO_FARE_POLICY"
  toErrorCode CantCalculateDistance = "CANT_CALCULATE_DISTANCE"
  toHttpCode NoFarePolicy = E400
  toHttpCode CantCalculateDistance = E500

data AllocationError
  = EmptyDriverPool
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''AllocationError

instance IsBaseError AllocationError

instance IsAPIError AllocationError where
  toErrorCode EmptyDriverPool = "EMPTY_DRIVER_POOL"

data DriverInformationError
  = DriverInfoNotFound
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''DriverInformationError

instance IsBaseError DriverInformationError

instance IsAPIError DriverInformationError where
  toErrorCode DriverInfoNotFound = "DRIVER_INFORMATON_NOT_FOUND"

data ProductsError
  = ProductsNotFound
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''ProductsError

instance IsBaseError ProductsError

instance IsAPIError ProductsError where
  toErrorCode = \case
    ProductsNotFound -> "PRODUCTS_NOT_FOUND"
  toHttpCode = \case
    ProductsNotFound -> E500
