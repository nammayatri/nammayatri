{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverModuleCompletion where

import Data.Aeson
import qualified Domain.Types.LmsModule
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DriverModuleCompletion = DriverModuleCompletion
  { completedAt :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    completionId :: Kernel.Types.Id.Id Domain.Types.DriverModuleCompletion.DriverModuleCompletion,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    entitiesCompleted :: [Domain.Types.DriverModuleCompletion.ModuleCompletionEntity],
    expiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    moduleId :: Kernel.Types.Id.Id Domain.Types.LmsModule.LmsModule,
    ratingAtTheTimeOfCompletion :: Kernel.Prelude.Maybe Kernel.Types.Common.Centesimal,
    startedAt :: Kernel.Prelude.UTCTime,
    status :: Domain.Types.DriverModuleCompletion.ModuleCompletionStatus,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

data ModuleCompletionEntity = QUIZ | VIDEO deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

data ModuleCompletionStatus = MODULE_NOT_YET_STARTED | MODULE_ONGOING | MODULE_COMPLETED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ModuleCompletionEntity)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ModuleCompletionStatus)
