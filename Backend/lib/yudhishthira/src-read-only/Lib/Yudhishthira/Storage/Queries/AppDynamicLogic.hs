{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Yudhishthira.Storage.Queries.AppDynamicLogic where

import qualified Data.Aeson
import qualified Data.String.Conversions
import qualified Data.Text
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import qualified Kernel.Types.TimeBound
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogic as Beam
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow
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

instance FromTType' Beam.AppDynamicLogic Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic where
  fromTType' (Beam.AppDynamicLogicT {..}) = do
    pure $
      Just
        Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic
          { description = description,
            domain = domain,
            logic = (Kernel.Prelude.fromMaybe Data.Aeson.Null . Data.Aeson.decode . Data.String.Conversions.cs) logic,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            name = name,
            order = order,
            timeBounds = Kernel.Prelude.fromMaybe Kernel.Types.TimeBound.Unbounded timeBounds,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AppDynamicLogic Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic where
  toTType' (Lib.Yudhishthira.Types.AppDynamicLogic.AppDynamicLogic {..}) = do
    Beam.AppDynamicLogicT
      { Beam.description = description,
        Beam.domain = domain,
        Beam.logic = (Data.String.Conversions.cs . Data.Aeson.encode) logic,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.name = name,
        Beam.order = order,
        Beam.timeBounds = Kernel.Prelude.Just timeBounds,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
