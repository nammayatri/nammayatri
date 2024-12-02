{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicy.FarePolicyIntercitySourceDestinationCityDetails where

import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicy.FarePolicyIntercitySourceDestinationCityDetails as DFP
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id as KTI
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicy.FarePolicyIntercitySourceDestinationCityDetails as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Beam.FullFarePolicyIntercitySourceDestinationCityDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Beam.FullFarePolicyIntercitySourceDestinationCityDetails] -> m ())
createMany = traverse_ create

-- findByPrimaryKey ::
--   (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
--   (Kernel.Prelude.Text -> KTI.Id DFP.FarePolicy -> Kernel.Prelude.Text -> m (Maybe Beam.FullFarePolicyIntercitySourceDestinationCityDetails))
-- findByPrimaryKey destinationCity farePolicyId sourceCity = do
--   findOneWithKV
--     [ Se.And
--         [ Se.Is Beam.destinationCity $ Se.Eq destinationCity,
--           Se.Is Beam.farePolicyId $ Se.Eq (getId farePolicyId),
--           Se.Is Beam.sourceCity $ Se.Eq sourceCity
--         ]
--     ]

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id DFP.FarePolicy -> m (Maybe Beam.FullFarePolicyIntercitySourceDestinationCityDetails)
findById' farePolicyId' = findOneWithKV [Se.Is Beam.farePolicyId $ Se.Eq (getId farePolicyId')]

findAll' ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DFP.FarePolicy ->
  m [Beam.FullFarePolicyIntercitySourceDestinationCityDetails]
findAll' farePolicyId = findAllWithOptionsKV [Se.Is Beam.farePolicyId $ Se.Eq (getId farePolicyId)] (Se.Asc Beam.sourceCity) Nothing Nothing

delete :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> m ()
delete farePolicyId = deleteWithKV [Se.Is Beam.farePolicyId $ Se.Eq (getId farePolicyId)]

instance FromTType' Beam.FarePolicyIntercitySourceDestinationCityDetails Beam.FullFarePolicyIntercitySourceDestinationCityDetails where
  fromTType' (Beam.FarePolicyIntercitySourceDestinationCityDetailsT {..}) = do
    pure $
      Just
        ( KTI.Id farePolicyId,
          DFP.FarePolicyIntercitySourceDestinationCityDetails {..}
        )

instance ToTType' Beam.FarePolicyIntercitySourceDestinationCityDetails Beam.FullFarePolicyIntercitySourceDestinationCityDetails where
  toTType' (KTI.Id farePolicyId, DFP.FarePolicyIntercitySourceDestinationCityDetails {..}) =
    Beam.FarePolicyIntercitySourceDestinationCityDetailsT {..}
