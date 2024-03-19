{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.Cac where

import Data.Time.Clock (diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import qualified Domain.Types.DriverPoolConfig as DPC
import qualified Domain.Types.FarePolicy as FP
import qualified Domain.Types.GoHomeConfig as GHC
import qualified Domain.Types.Merchant.DriverIntelligentPoolConfig as DIPC
import qualified Domain.Types.Merchant.TransporterConfig as TC
import qualified EulerHS.Language as L
import EulerHS.Types (OptionEntity)
import Kernel.Prelude
import Kernel.Types.App (MonadFlow)
import qualified System.Environment as Se

data GoHomeConfig = GoHomeConfig Text
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data TransporterConfig = TransporterConfig Text
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data DriverPoolConfig = DriverPoolConfig Text String String Int
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data DriverIntelligentPoolConfig = DriverIntelligentPoolConfig Text
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data FarePolicy = FarePolicy Text
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data LastUpdatedGoHomeConfig = LastUpdatedGoHomeConfig
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data LastUpdatedTransporterConfig = LastUpdatedTransporterConfig
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data LastUpdatedDriverPoolConfig = LastUpdatedDriverPoolConfig
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data LastUpdatedDriverIntelligentPoolConfig = LastUpdatedDriverIntelligentPoolConfig
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data LastUpdatedFarePolicy = LastUpdatedFarePolicy
  deriving stock (Generic, Typeable, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

instance OptionEntity LastUpdatedGoHomeConfig UTCTime

instance OptionEntity LastUpdatedTransporterConfig UTCTime

instance OptionEntity LastUpdatedDriverPoolConfig UTCTime

instance OptionEntity LastUpdatedDriverIntelligentPoolConfig UTCTime

instance OptionEntity LastUpdatedFarePolicy UTCTime

instance OptionEntity GoHomeConfig GHC.GoHomeConfig

instance OptionEntity TransporterConfig TC.TransporterConfig

instance OptionEntity DriverPoolConfig DPC.DriverPoolConfig

instance OptionEntity DriverIntelligentPoolConfig DIPC.DriverIntelligentPoolConfig

instance OptionEntity FarePolicy FP.FarePolicy

updateConfig :: (MonadFlow m, (OptionEntity a UTCTime)) => a -> m Bool
updateConfig val = do
  updateInt <- liftIO (Se.lookupEnv "CAC_UPDATE_INTERVAL") <&> maybe 10 read
  maybe
    (pure True)
    ( \lastUpdated' -> do
        currentTime <- liftIO getCurrentTime
        let isGreater = nominalDiffTimeToSeconds (diffUTCTime currentTime lastUpdated') / 1e12 >= updateInt
        when isGreater do
          L.setOption val =<< liftIO getCurrentTime
        pure isGreater
    )
    =<< L.getOption val
