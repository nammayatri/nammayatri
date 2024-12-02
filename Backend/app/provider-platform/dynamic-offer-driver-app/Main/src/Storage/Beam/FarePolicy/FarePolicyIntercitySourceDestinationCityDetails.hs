{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FarePolicy.FarePolicyIntercitySourceDestinationCityDetails where

import qualified Database.Beam as B
import Domain.Types.Common ()
-- import Kernel.External.Encryption
import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.FarePolicy.FarePolicyIntercitySourceDestinationCityDetails as Domain
import Domain.Types.UtilsTH
import Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id as KTI
import Tools.Beam.UtilsTH

data FarePolicyIntercitySourceDestinationCityDetailsT f = FarePolicyIntercitySourceDestinationCityDetailsT
  { destinationCity :: (B.C f Kernel.Prelude.Text),
    farePolicyId :: (B.C f Kernel.Prelude.Text),
    sourceCity :: (B.C f Kernel.Prelude.Text),
    stateEntryPermitCharges :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    tollCharges :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney))
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyIntercitySourceDestinationCityDetailsT where
  data PrimaryKey FarePolicyIntercitySourceDestinationCityDetailsT f
    = FarePolicyIntercitySourceDestinationCityDetailsId
        (B.C f Kernel.Prelude.Text)
        (B.C f Kernel.Prelude.Text)
        (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey FarePolicyIntercitySourceDestinationCityDetailsT {..} = FarePolicyIntercitySourceDestinationCityDetailsId farePolicyId sourceCity destinationCity

type FarePolicyIntercitySourceDestinationCityDetails = FarePolicyIntercitySourceDestinationCityDetailsT Identity

type FullFarePolicyIntercitySourceDestinationCityDetails = (KTI.Id Domain.FarePolicy, Domain.FarePolicyIntercitySourceDestinationCityDetails)

$(enableKVPG (''FarePolicyIntercitySourceDestinationCityDetailsT) [('destinationCity), ('farePolicyId), ('sourceCity)] [])

$(mkTableInstances (''FarePolicyIntercitySourceDestinationCityDetailsT) "fare_policy_inter_city_source_destination_city_details")
$(mkCacParseInstanceList ''FarePolicyIntercitySourceDestinationCityDetails)
