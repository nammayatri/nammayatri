{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.AppDynamicLogic (module Lib.Yudhishthira.Storage.Queries.AppDynamicLogic, module ReExport) where

import qualified Data.Aeson
import qualified Data.String.Conversions
import qualified Data.Text
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogic as Beam
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow
import Lib.Yudhishthira.Storage.Queries.AppDynamicLogicExtra as ReExport
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogic
import qualified Sequelize as Se

create :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic -> m ())
create = createWithKV

createMany :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => ([Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic] -> m ())
createMany = traverse_ create

findByMerchantOpCityAndDomain ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m [Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic])
findByMerchantOpCityAndDomain limit offset merchantOperatingCityId domain = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.domain $ Se.Eq domain
        ]
    ]
    (Se.Asc Beam.order)
    limit
    offset

findByPrimaryKey ::
  (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) =>
  (Lib.Yudhishthira.Types.LogicDomain -> Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Data.Text.Text -> m (Maybe Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic))
findByPrimaryKey domain merchantOperatingCityId name = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.name $ Se.Eq name
        ]
    ]

updateByPrimaryKey :: (Lib.Yudhishthira.Storage.Beam.BeamFlow.BeamFlow m r) => (Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic -> m ())
updateByPrimaryKey (Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.description description,
      Se.Set Beam.logic ((Data.String.Conversions.cs . Data.Aeson.encode) logic),
      Se.Set Beam.order order,
      Se.Set Beam.timeBounds (Kernel.Prelude.Just timeBounds),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.domain $ Se.Eq domain, Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId), Se.Is Beam.name $ Se.Eq name]]
