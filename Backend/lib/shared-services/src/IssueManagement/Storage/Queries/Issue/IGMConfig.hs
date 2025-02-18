{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Storage.Queries.Issue.IGMConfig where

import IssueManagement.Common
import IssueManagement.Domain.Types.Issue.IGMConfig as DIGMConfig
import qualified IssueManagement.Storage.Beam.Issue.IGMConfig as Beam
import IssueManagement.Storage.BeamFlow
import IssueManagement.Tools.UtilsTH
import qualified Kernel.Types.Id

create :: BeamFlow m r => (IGMConfig -> m ())
create = createWithKV

createMany :: BeamFlow m r => ([IGMConfig] -> m ())
createMany = traverse_ create

findByMerchantId :: BeamFlow m r => Kernel.Types.Id.Id Merchant -> m (Maybe IGMConfig)
findByMerchantId (Kernel.Types.Id.Id merchantId) = do findOneWithKV [Is Beam.merchantId $ Eq merchantId]

findByPrimaryKey :: BeamFlow m r => (Kernel.Types.Id.Id IGMConfig -> m (Maybe IGMConfig))
findByPrimaryKey (Kernel.Types.Id.Id id) = do findOneWithKV [And [Is Beam.id $ Eq id]]

updateByPrimaryKey :: BeamFlow m r => (IGMConfig -> m ())
updateByPrimaryKey (IGMConfig {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Set Beam.expectedResolutionTime expectedResolutionTime,
      Set Beam.expectedResponseTime expectedResponseTime,
      Set Beam.groEmail groEmail,
      Set Beam.groName groName,
      Set Beam.groPhone groPhone,
      Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Set Beam.createdAt createdAt,
      Set Beam.updatedAt _now
    ]
    [And [Is Beam.id $ Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.IGMConfig IGMConfig where
  fromTType' (Beam.IGMConfigT {..}) = do
    pure $
      Just
        IGMConfig
          { expectedResolutionTime = expectedResolutionTime,
            expectedResponseTime = expectedResponseTime,
            groEmail = groEmail,
            groName = groName,
            groPhone = groPhone,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.IGMConfig IGMConfig where
  toTType' (IGMConfig {..}) = do
    Beam.IGMConfigT
      { Beam.expectedResolutionTime = expectedResolutionTime,
        Beam.expectedResponseTime = expectedResponseTime,
        Beam.groEmail = groEmail,
        Beam.groName = groName,
        Beam.groPhone = groPhone,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
