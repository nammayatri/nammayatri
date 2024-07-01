{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AmbulanceDetails where

import qualified Domain.Types.AmbulanceDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.AmbulanceDetails as Beam
import qualified Storage.Queries.AmbulanceQuoteBreakup

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AmbulanceDetails.AmbulanceDetails -> m ())
create = createWithKV

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.AmbulanceDetails.AmbulanceDetails -> m (Maybe Domain.Types.AmbulanceDetails.AmbulanceDetails))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

instance FromTType' Beam.AmbulanceDetails Domain.Types.AmbulanceDetails.AmbulanceDetails where
  fromTType' (Beam.AmbulanceDetailsT {..}) = do
    ambulanceQuoteBreakupList' <- Storage.Queries.AmbulanceQuoteBreakup.findAllByQuoteIdT (Kernel.Types.Id.Id id)
    pure $
      Just
        Domain.Types.AmbulanceDetails.AmbulanceDetails
          { ambulanceQuoteBreakupList = ambulanceQuoteBreakupList',
            id = Kernel.Types.Id.Id id,
            maxEstimatedFare = Kernel.Utils.Common.mkPrice (Just currency) maxEstimatedFare,
            minEstimatedFare = Kernel.Utils.Common.mkPrice (Just currency) minEstimatedFare
          }

instance ToTType' Beam.AmbulanceDetails Domain.Types.AmbulanceDetails.AmbulanceDetails where
  toTType' (Domain.Types.AmbulanceDetails.AmbulanceDetails {..}) = do
    Beam.AmbulanceDetailsT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.maxEstimatedFare = (.amount) maxEstimatedFare,
        Beam.currency = (.currency) minEstimatedFare,
        Beam.minEstimatedFare = (.amount) minEstimatedFare
      }
