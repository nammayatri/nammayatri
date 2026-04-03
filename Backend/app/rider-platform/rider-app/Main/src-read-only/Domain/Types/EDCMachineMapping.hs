{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.EDCMachineMapping where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Tools.Beam.UtilsTH



data EDCMachineMapping
    = EDCMachineMapping {clientId :: Kernel.Prelude.Text,
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
                         updatedAt :: Kernel.Prelude.UTCTime}
    deriving Generic



