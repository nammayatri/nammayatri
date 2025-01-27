module Storage.Queries.PlanExtra where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Plan
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Plan as BeamP
import Storage.Queries.OrphanInstances.Plan ()

-- Extra code goes here --

findByMerchantOpCityIdAndPaymentModeWithServiceName ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
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

fetchAllPlan :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => m [Plan]
fetchAllPlan = findAllWithKV [Se.Is BeamP.id $ Se.Not $ Se.Eq $ getId ""]

fetchAllPlanByMerchantOperatingCityMbServiceName :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DMOC.MerchantOperatingCity -> Maybe ServiceNames -> m [Plan]
fetchAllPlanByMerchantOperatingCityMbServiceName merchantOpCityId mbServiceName = do
  findAllWithKV
    ( [ Se.Is BeamP.id $ Se.Not $ Se.Eq $ getId "",
        Se.Is BeamP.merchantOpCityId $ Se.Eq merchantOpCityId.getId
      ]
        <> maybe [] (\serviceName -> [Se.Is BeamP.serviceName $ Se.Eq serviceName]) mbServiceName
    )
