{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-orphans #-}

module Domain.Types.FleetConfig where

import Data.Aeson
import qualified Data.Text
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL as BS
import qualified Database.Beam.Postgres as BP
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data FleetType = PUBLIC | TRUSTED | PRIVATE
  deriving stock (Show, Read, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

instance BS.HasSqlValueSyntax be Text => BS.HasSqlValueSyntax be FleetType where
  sqlValueSyntax = BS.sqlValueSyntax . Data.Text.pack . show

instance B.HasSqlEqualityCheck BP.Postgres FleetType

instance BS.FromBackendRow BP.Postgres FleetType where
  fromBackendRow = do
    textVal <- BS.fromBackendRow :: BS.FromBackendRowM BP.Postgres Text
    case textVal of
      "PUBLIC" -> pure PUBLIC
      "TRUSTED" -> pure TRUSTED
      "PRIVATE" -> pure PRIVATE
      _ -> pure PUBLIC -- Default to PUBLIC for any other value

instance Ord FleetType where
  compare PUBLIC PUBLIC = EQ
  compare PUBLIC TRUSTED = LT
  compare PUBLIC PRIVATE = LT
  compare TRUSTED PUBLIC = GT
  compare TRUSTED TRUSTED = EQ
  compare TRUSTED PRIVATE = LT
  compare PRIVATE PUBLIC = GT
  compare PRIVATE TRUSTED = GT
  compare PRIVATE PRIVATE = EQ

data FleetConfig = FleetConfig
  { allowAutomaticRoundTripAssignment :: Kernel.Prelude.Bool,
    allowEndingMidRoute :: Kernel.Prelude.Bool,
    allowStartRideFromQR :: Kernel.Prelude.Bool,
    directlyStartFirstTripAssignment :: Kernel.Prelude.Bool,
    endRideDistanceThreshold :: Kernel.Types.Common.HighPrecMeters,
    fleetOwnerId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    rideEndApproval :: Kernel.Prelude.Bool,
    unlinkDriverAndVehicleOnTripTermination :: Kernel.Prelude.Bool,
    merchantId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant),
    merchantOperatingCityId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity),
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime,
    fleetType :: FleetType
  }
  deriving (Generic, Show, Read, Eq, FromJSON, ToJSON, ToSchema)
