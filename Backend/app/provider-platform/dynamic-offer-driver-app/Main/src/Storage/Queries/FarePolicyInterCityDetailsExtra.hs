{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicyInterCityDetailsExtra where

import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.FarePolicy.FarePolicyInterCityDetailsPricingSlabs as FPICDPS
import qualified Domain.Types.FarePolicyInterCityDetails as DTFPICD
import qualified Domain.Types.FarePolicyInterCityDetailsPricingSlabs as DSLICDPS
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicyInterCityDetails as BeamFPRD
import qualified Storage.Beam.FarePolicyInterCityDetailsPricingSlabs as BeamFPICDPS
import qualified Storage.Queries.FarePolicyInterCityDetailsPricingSlabs as QueriesFPICDPS
import Storage.Queries.OrphanInstances.FarePolicyInterCityDetails

type FullFarePolicyInterCityDetails = (KTI.Id Domain.FarePolicy, Domain.FPInterCityDetails)

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m (Maybe FullFarePolicyInterCityDetails)
findById' fpId@(KTI.Id farePolicyId') = do
  mResult :: Maybe DTFPICD.FarePolicyInterCityDetails <- findOneWithKV [Se.Is BeamFPRD.farePolicyId $ Se.Eq farePolicyId']
  case mResult of
    Nothing -> pure Nothing
    Just DTFPICD.FarePolicyInterCityDetails {..} -> do
      fullFPICDPS <- QueriesFPICDPS.findAll' fpId
      fPICDPS <- fromMaybeM (InternalError "No pricing slab found for intercity") (NE.nonEmpty fullFPICDPS)
      pure . Just $
        ( fpId,
          Domain.FPInterCityDetails
            { pricingSlabs = snd <$> fPICDPS,
              ..
            }
        )

toDSLType :: FullFarePolicyInterCityDetails -> DTFPICD.FarePolicyInterCityDetails
toDSLType (KTI.Id farePolicyId, Domain.FPInterCityDetails {..}) =
  DTFPICD.FarePolicyInterCityDetails {..}

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FullFarePolicyInterCityDetails -> m ()
create farePolicyInterCityDetails = do
  mapM_ (\FPICDPS.FPInterCityDetailsPricingSlabs {..} -> QueriesFPICDPS.create DSLICDPS.FarePolicyInterCityDetailsPricingSlabs {farePolicyId = KTI.getId (fst farePolicyInterCityDetails), ..}) (NE.toList (snd farePolicyInterCityDetails).pricingSlabs)
  createWithKV $ toDSLType farePolicyInterCityDetails

delete :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m ()
delete farePolicyId = do
  QueriesFPICDPS.delete farePolicyId
  deleteWithKV [Se.Is BeamFPRD.farePolicyId $ Se.Eq (KTI.getId farePolicyId)]

fromTTypeFarePolicyInterCityDetails ::
  BeamFPRD.FarePolicyInterCityDetails ->
  NonEmpty QueriesFPICDPS.FullFarePolicyInterCityDetailsPricingSlabs ->
  FullFarePolicyInterCityDetails
fromTTypeFarePolicyInterCityDetails BeamFPRD.FarePolicyInterCityDetailsT {..} fPICDPS =
  ( KTI.Id farePolicyId,
    Domain.FPInterCityDetails
      { pricingSlabs = snd <$> fPICDPS,
        waitingChargeInfo =
          ((,) <$> waitingCharge <*> freeWatingTime) <&> \(waitingCharge', freeWaitingTime') ->
            Domain.WaitingChargeInfo
              { waitingCharge = waitingCharge',
                freeWaitingTime = freeWaitingTime'
              },
        ..
      }
  )
