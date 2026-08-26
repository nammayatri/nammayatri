{-# LANGUAGE InstanceSigs #-}

module IssueManagement.Storage.Beam.Issue.IssueApiIntegration where

import qualified Database.Beam as B
import Database.Beam.MySQL ()
import GHC.Generics (Generic)
import qualified IssueManagement.Domain.Types.Issue.IssueApiIntegration as DAI
import IssueManagement.Tools.UtilsTH hiding (Generic)

data IssueApiIntegrationT f = IssueApiIntegrationT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    name :: B.C f Text,
    description :: B.C f (Maybe Text),
    kind :: B.C f DAI.IntegrationKind,
    method :: B.C f DAI.ApiMethod,
    urlTemplate :: B.C f Text,
    headersJson :: B.C f (Maybe Text),
    bodyTemplate :: B.C f (Maybe Text),
    timeoutMs :: B.C f Int,
    responseFieldsJson :: B.C f Text,
    isActive :: B.C f Bool,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table IssueApiIntegrationT where
  data PrimaryKey IssueApiIntegrationT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey :: IssueApiIntegrationT column -> B.PrimaryKey IssueApiIntegrationT column
  primaryKey = Id . id

type IssueApiIntegration = IssueApiIntegrationT Identity

$(enableKVPG ''IssueApiIntegrationT ['id] [])

$(mkTableInstancesGenericSchema ''IssueApiIntegrationT "issue_api_integration")
