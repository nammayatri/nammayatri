{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Error (module Types.Error) where

import Beckn.Types.Error as Types.Error
import Beckn.Types.Error.BaseError.HTTPError
import Beckn.Types.Error.BaseError.HTTPError.APIError
import EulerHS.Prelude

data FarePolicyError
  = NoFarePolicy
  | CantCalculateDistance
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

instanceExceptionWithParent 'APIException ''FarePolicyError

instance IsBaseError FarePolicyError where
  toMessage NoFarePolicy = Just "No fare policy matches passed data."
  toMessage _ = Nothing

instance IsHTTPError FarePolicyError where
  toErrorCode NoFarePolicy = "NO_FARE_POLICY"
  toErrorCode CantCalculateDistance = "CANT_CALCULATE_DISTANCE"
  toHttpCode NoFarePolicy = E400
  toHttpCode CantCalculateDistance = E500

instance IsAPIError FarePolicyError

data AllocationError
  = EmptyDriverPool
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''AllocationError

instance IsBaseError AllocationError

instance IsHTTPError AllocationError where
  toErrorCode EmptyDriverPool = "EMPTY_DRIVER_POOL"

instance IsAPIError AllocationError

data DriverInformationError
  = DriverInfoNotFound
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''DriverInformationError

instance IsBaseError DriverInformationError

instance IsHTTPError DriverInformationError where
  toErrorCode DriverInfoNotFound = "DRIVER_INFORMATON_NOT_FOUND"

instance IsAPIError DriverInformationError

data ProductsError
  = ProductsNotFound
  deriving (Eq, Show)

instanceExceptionWithParent 'APIException ''ProductsError

instance IsBaseError ProductsError

instance IsHTTPError ProductsError where
  toErrorCode = \case
    ProductsNotFound -> "PRODUCTS_NOT_FOUND"
  toHttpCode = \case
    ProductsNotFound -> E500

instance IsAPIError ProductsError

newtype ShardMappingError = ShardMappingError Text
  deriving (Show, Typeable)

instance IsBaseError ShardMappingError where
  toMessage (ShardMappingError msg) = Just msg

instanceExceptionWithParent 'BaseException ''ShardMappingError
