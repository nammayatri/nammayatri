{-# OPTIONS_GHC -Wno-orphans #-}

module Lib.Yudhishthira.Storage.Queries.AppDynamicLogicRolloutExtra where

import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.Yudhishthira.Storage.Beam.AppDynamicLogicRollout as Beam
import qualified Lib.Yudhishthira.Storage.Beam.BeamFlow as BeamFlow
import Lib.Yudhishthira.Storage.Queries.OrphanInstances.AppDynamicLogicRollout ()
import qualified Lib.Yudhishthira.Types
import qualified Lib.Yudhishthira.Types.AppDynamicLogicRollout
import Sequelize as Se

-- Extra code goes here --

-- | Deletes non-base rollout rows that are active (RUNNING or NULL status).
-- Preserves: base version, DISCARDED (aborted), CONCLUDED (demoted bases), REVERTED rows.
deleteActiveNonBaseRollouts :: BeamFlow.BeamFlow m r => Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m ()
deleteActiveNonBaseRollouts cityId domain =
  deleteWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId cityId),
          Se.Is Beam.domain $ Se.Eq domain,
          Se.Or [Se.Is Beam.isBaseVersion (Se.Eq (Just False)), Se.Is Beam.isBaseVersion $ Se.Eq Nothing],
          Se.Or [Se.Is Beam.experimentStatus (Se.Eq (Just Lib.Yudhishthira.Types.RUNNING)), Se.Is Beam.experimentStatus $ Se.Eq Nothing]
        ]
    ]

delete :: BeamFlow.BeamFlow m r => Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity -> Lib.Yudhishthira.Types.LogicDomain -> m ()
delete cityId domain = deleteWithKV [Se.And [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId cityId), Se.Is Beam.domain $ Se.Eq domain]]

deleteByPrimaryKey ::
  (BeamFlow.BeamFlow m r) =>
  (Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout -> m ())
deleteByPrimaryKey (Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout {..}) = do
  deleteWithKV
    [ Se.And
        [ Se.Is Beam.domain $ Se.Eq domain,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
          Se.Is Beam.timeBounds $ Se.Eq timeBounds,
          Se.Is Beam.version $ Se.Eq version
        ]
    ]

findByCityAndDomainAndIsBase ::
  (BeamFlow.BeamFlow m r) =>
  ( Kernel.Types.Id.Id Lib.Yudhishthira.Types.MerchantOperatingCity ->
    Lib.Yudhishthira.Types.LogicDomain ->
    m (Maybe Lib.Yudhishthira.Types.AppDynamicLogicRollout.AppDynamicLogicRollout)
  )
findByCityAndDomainAndIsBase merchantOperatingCityId domain = do
  listToMaybe
    <$> findAllWithKV
      [ Se.And
          [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId),
            Se.Is Beam.domain $ Se.Eq domain,
            Se.Is Beam.isBaseVersion $ Se.Eq (Just True)
          ]
      ]
