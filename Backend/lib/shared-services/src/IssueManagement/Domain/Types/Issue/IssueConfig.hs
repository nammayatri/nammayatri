module IssueManagement.Domain.Types.Issue.IssueConfig where

import qualified Data.Aeson as A
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField
import EulerHS.Prelude hiding (id)
import qualified IssueManagement.Common as Common
import IssueManagement.Domain.Types.Issue.IssueMessage
import Kernel.Prelude
import Kernel.Types.Id

data IssueConfig = IssueConfig
  { id :: Id IssueConfig,
    merchantOperatingCityId :: Id Common.MerchantOperatingCity,
    autoMarkIssueClosedDuration :: Double,
    onAutoMarkIssueClsMsgs :: [Id IssueMessage],
    onCreateIssueMsgs :: [Id IssueMessage],
    onIssueReopenMsgs :: [Id IssueMessage],
    onKaptMarkIssueResMsgs :: [Id IssueMessage],
    merchantId :: Id Common.Merchant,
    messageTransformationConfig :: Maybe MessageTransformationConfig,
    reopenCount :: Int,
    onIssueCloseMsgs :: [Id IssueMessage],
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Generic, Read, Eq, Ord, ToJSON, FromJSON, ToSchema)

data MessageTransformationConfig = MessageTransformationConfig
  { merchantName :: Maybe Text,
    merchantNameWTranslations :: Maybe [Common.Translation],
    supportEmail :: Maybe Text
  }
  deriving (Show, Generic, Read, Eq, Ord, ToJSON, FromJSON, ToSchema)

fromFieldMessageTransformationConfig ::
  Field ->
  Maybe ByteString ->
  Conversion MessageTransformationConfig
fromFieldMessageTransformationConfig f mbValue = do
  value <- fromField f mbValue
  case A.fromJSON value of
    A.Success a -> pure a
    _ -> returnError ConversionFailed f "Conversion failed"

instance FromField MessageTransformationConfig where
  fromField = fromFieldMessageTransformationConfig

instance HasSqlValueSyntax be A.Value => HasSqlValueSyntax be MessageTransformationConfig where
  sqlValueSyntax = sqlValueSyntax . A.toJSON

instance BeamSqlBackend be => B.HasSqlEqualityCheck be MessageTransformationConfig

instance FromBackendRow Postgres MessageTransformationConfig
