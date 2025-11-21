{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.DocStatus where

import qualified Data.Aeson as A
import Data.Aeson.Types
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.DocumentVerificationConfig as DVC
import GHC.Generics (Generic)
import Prelude

-- | Status of a document in the verification process
data DocStatusEnum
  = DOC_PENDING -- Document is pending verification
  | DOC_SUCCESS -- Document verified successfully
  | DOC_FAILED -- Document verification failed
  | DOC_CONSENT_DENIED -- User denied consent for this document
  | DOC_PULL_REQUIRED -- Document requires pull operation with user input
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToSchema)

instance ToJSON DocStatusEnum where
  toJSON = String . docStatusEnumToText

instance FromJSON DocStatusEnum where
  parseJSON = withText "DocStatusEnum" $ \t ->
    case textToDocStatusEnum t of
      Just s -> pure s
      Nothing -> fail $ "Unknown DocStatusEnum: " ++ T.unpack t

-- | Convert DocStatusEnum to Text (for JSON serialization)
docStatusEnumToText :: DocStatusEnum -> T.Text
docStatusEnumToText DOC_PENDING = "PENDING"
docStatusEnumToText DOC_SUCCESS = "SUCCESS"
docStatusEnumToText DOC_FAILED = "FAILED"
docStatusEnumToText DOC_CONSENT_DENIED = "CONSENT_DENIED"
docStatusEnumToText DOC_PULL_REQUIRED = "PULL_REQUIRED"

-- | Alias for compatibility (same as docStatusEnumToText)
docStatusToText :: DocStatusEnum -> T.Text
docStatusToText = docStatusEnumToText

-- | Convert Text to DocStatusEnum (for JSON deserialization)
textToDocStatusEnum :: T.Text -> Maybe DocStatusEnum
textToDocStatusEnum "PENDING" = Just DOC_PENDING
textToDocStatusEnum "SUCCESS" = Just DOC_SUCCESS
textToDocStatusEnum "FAILED" = Just DOC_FAILED
textToDocStatusEnum "CONSENT_DENIED" = Just DOC_CONSENT_DENIED
textToDocStatusEnum "PULL_REQUIRED" = Just DOC_PULL_REQUIRED
textToDocStatusEnum _ = Nothing

-- | Status information for a single document
data DocumentStatus = DocumentStatus
  { status :: DocStatusEnum,
    responseCode :: Maybe T.Text,
    responseDescription :: Maybe T.Text
  }
  deriving (Show, Read, Eq, Ord, Generic, ToJSON, FromJSON)
  deriving anyclass (ToSchema)

-- | Convert DocumentType to Text for JSON keys
-- Uses the existing ToJSON instance to ensure consistency with JSON serialization
documentTypeToText :: DVC.DocumentType -> T.Text
documentTypeToText dt = case A.toJSON dt of
  A.String t -> t
  _ -> T.pack $ show dt -- Fallback to show if ToJSON doesn't produce a String

-- | Convert Text to DocumentType for JSON keys
-- Uses the existing FromJSON instance for consistency
textToDocumentType :: T.Text -> Maybe DVC.DocumentType
textToDocumentType textValue = case A.fromJSON (A.String textValue) of
  A.Success dt -> Just dt
  A.Error _ -> Nothing

-- | ToJSONKey instance for DocumentType (using existing ToJSON)
instance ToJSONKey DVC.DocumentType where
  toJSONKey = toJSONKeyText documentTypeToText

-- | FromJSONKey instance for DocumentType (using existing FromJSON)
instance FromJSONKey DVC.DocumentType where
  fromJSONKey = FromJSONKeyText $ \t ->
    maybe (error $ "Unknown DocumentType: " ++ T.unpack t) id (textToDocumentType t)

-- | Map of document types to their verification status
-- Stored as JSON in the database (similar to ExotelMapping)
-- Uses strongly-typed DocumentType as keys instead of Text
newtype DocStatusMap = DocStatusMap
  { unDocStatusMap :: Map.Map DVC.DocumentType DocumentStatus
  }
  deriving (Show, Read, Eq, Ord, Generic)
  deriving anyclass (ToSchema)

instance ToJSON DocStatusMap where
  toJSON (DocStatusMap hm) = A.toJSON hm

instance FromJSON DocStatusMap where
  parseJSON v = DocStatusMap <$> parseJSON v

-- Database integration (following ExotelMapping pattern)
fromFieldDocStatus ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion DocStatusMap
fromFieldDocStatus f mbValue = do
  value <- fromField f mbValue
  case A.fromJSON value of
    Success a -> pure a
    _ -> DPSF.returnError DPSF.ConversionFailed f "Conversion failed"

instance HasSqlValueSyntax be A.Value => HasSqlValueSyntax be DocStatusMap where
  sqlValueSyntax = sqlValueSyntax . A.toJSON

instance FromField DocStatusMap where
  fromField = fromFieldDocStatus

instance BeamSqlBackend be => B.HasSqlEqualityCheck be DocStatusMap

instance FromBackendRow Postgres DocStatusMap

-- | Helper functions for working with DocStatusMap

-- | Create an empty DocStatusMap
emptyDocStatusMap :: DocStatusMap
emptyDocStatusMap = DocStatusMap Map.empty

-- | Update status for a specific document type
updateDocStatus ::
  DVC.DocumentType ->
  DocStatusEnum ->
  Maybe T.Text ->
  Maybe T.Text ->
  DocStatusMap ->
  DocStatusMap
updateDocStatus docType status' respCode respDesc (DocStatusMap m) =
  let docStatus = DocumentStatus status' respCode respDesc
   in DocStatusMap $ Map.insert docType docStatus m

-- | Get status for a specific document type
getDocStatus :: DVC.DocumentType -> DocStatusMap -> Maybe DocumentStatus
getDocStatus docType (DocStatusMap m) = Map.lookup docType m

-- | Create DocStatusMap from a list of (DocumentType, DocumentStatus) pairs
fromList :: [(DVC.DocumentType, DocumentStatus)] -> DocStatusMap
fromList = DocStatusMap . Map.fromList

-- | Convert DocStatusMap to a list of (DocumentType, DocumentStatus) pairs
toList :: DocStatusMap -> [(DVC.DocumentType, DocumentStatus)]
toList (DocStatusMap m) = Map.toList m
