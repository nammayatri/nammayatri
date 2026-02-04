{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.EDCMachineMapping where

import Data.Aeson
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data EDCMachineMapping = EDCMachineMapping
  { clientId :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    createdBy :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    id :: Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping,
    isActive :: Kernel.Prelude.Bool,
    machineName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantChannelId :: Kernel.Prelude.Text,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    merchantKey :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    paytmMid :: Kernel.Prelude.Text,
    personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    terminalId :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)
