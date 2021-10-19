{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Error (module Types.Error) where

import Beckn.Types.Error as Types.Error
import Beckn.Types.Error.BaseError.HTTPError
import EulerHS.Prelude

data FarePolicyError
  = NoFarePolicy
  | CantCalculateDistance
  deriving (Generic, Eq, Show, FromJSON, ToJSON, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FarePolicyError

instance IsBaseError FarePolicyError where
  toMessage NoFarePolicy = Just "No fare policy matches passed data."
  toMessage _ = Nothing

instance IsHTTPError FarePolicyError where
  toErrorCode NoFarePolicy = "NO_FARE_POLICY"
  toErrorCode CantCalculateDistance = "CANT_CALCULATE_DISTANCE"
  toHttpCode NoFarePolicy = E400
  toHttpCode CantCalculateDistance = E500

instance IsAPIError FarePolicyError

data FPDiscountError
  = FPDiscountDoesNotExist
  | FPDiscountAlreadyEnabled
  deriving (Generic, Eq, Show, FromJSON, ToJSON, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''FPDiscountError

instance IsBaseError FPDiscountError where
  toMessage = \case
    FPDiscountDoesNotExist -> Just "No discount matches passed data."
    FPDiscountAlreadyEnabled -> Just "Some discount is already enabled."

instance IsHTTPError FPDiscountError where
  toErrorCode = \case
    FPDiscountDoesNotExist -> "FARE_POLICY_DISCOUNT_DOES_NOT_EXIST"
    FPDiscountAlreadyEnabled -> "FARE_POLICY_DISCOUNT_ALREADY_ENABLED"
  toHttpCode = \case
    FPDiscountDoesNotExist -> E400
    FPDiscountAlreadyEnabled -> E400

instance IsAPIError FPDiscountError

data AllocationError
  = EmptyDriverPool
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''AllocationError

instance IsBaseError AllocationError

instance IsHTTPError AllocationError where
  toErrorCode EmptyDriverPool = "EMPTY_DRIVER_POOL"

instance IsAPIError AllocationError

data DriverInformationError
  = DriverInfoNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DriverInformationError

instance IsBaseError DriverInformationError

instance IsHTTPError DriverInformationError where
  toErrorCode DriverInfoNotFound = "DRIVER_INFORMATON_NOT_FOUND"

instance IsAPIError DriverInformationError

data ProductsError
  = ProductsNotFound
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''ProductsError

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

data DriverError
  = DriverAccountDisabled
  deriving (Eq, Show, IsBecknAPIError)

instanceExceptionWithParent 'HTTPException ''DriverError

instance IsBaseError DriverError where
  toMessage DriverAccountDisabled = Just "Driver account has been disabled. He can't go online and receive ride offers in this state."

instance IsHTTPError DriverError where
  toErrorCode = \case
    DriverAccountDisabled -> "DRIVER_ACCOUNT_DISABLED"
  toHttpCode = \case
    DriverAccountDisabled -> E403

instance IsAPIError DriverError
