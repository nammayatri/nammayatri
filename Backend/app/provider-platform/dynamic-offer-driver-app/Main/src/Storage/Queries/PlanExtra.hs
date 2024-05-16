{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PlanExtra where

import qualified Domain.Types.Merchant.MerchantOperatingCity as DMOC
import Domain.Types.Plan
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Plan as BeamP
import Storage.Queries.OrphanInstances.Plan

-- Extra code goes here --

findByMerchantOpCityIdAndPaymentModeWithServiceName ::
  KvDbFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  PaymentMode ->
  ServiceNames ->
  Maybe Bool ->
  m [Plan]
findByMerchantOpCityIdAndPaymentModeWithServiceName (Id merchantOpCityId) paymentMode serviceName mbIsDeprecated = do
  findAllWithKV
    [ Se.And
        ( [ Se.Is BeamP.merchantOpCityId $ Se.Eq merchantOpCityId,
            Se.Is BeamP.paymentMode $ Se.Eq paymentMode,
            Se.Is BeamP.serviceName $ Se.Eq serviceName
          ]
            <> ( case mbIsDeprecated of
                   Nothing -> []
                   Just isDeprecated -> [Se.Is BeamP.isDeprecated $ Se.Eq isDeprecated]
               )
        )
    ]

fetchAllPlan :: KvDbFlow m r => m [Plan]
fetchAllPlan = findAllWithKV [Se.Is BeamP.id $ Se.Not $ Se.Eq $ getId ""]
