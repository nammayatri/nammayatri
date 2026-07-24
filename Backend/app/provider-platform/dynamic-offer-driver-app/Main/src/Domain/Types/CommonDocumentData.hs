{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.CommonDocumentData where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import qualified Domain.Types.DocumentVerificationConfig as DVC
import GHC.Generics (Generic)
import Sequelize.SQLObject (SQLObject (..), ToSQLObject (..))
import Prelude

data TaxDetailsDocumentData = TaxDetailsDocumentData
  { identifierNumber :: T.Text,
    expiryDate :: Maybe UTCTime
  }
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

data BusinessDocumentData = BusinessDocumentData
  { identifierNumber :: T.Text,
    expiryDate :: Maybe UTCTime
  }
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

data VehicleFrontDocumentData = VehicleFrontDocumentData
  { rcId :: T.Text
  }
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

data VehicleFrontInteriorDocumentData = VehicleFrontInteriorDocumentData
  { rcId :: T.Text
  }
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

data VehicleBackInteriorDocumentData = VehicleBackInteriorDocumentData
  { rcId :: T.Text
  }
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

data CommonDocumentData
  = TaxDetailsData TaxDetailsDocumentData
  | BusinessLicenseData BusinessDocumentData
  | VehicleFrontData VehicleFrontDocumentData
  | VehicleFrontInteriorData VehicleFrontInteriorDocumentData
  | VehicleBackInteriorData VehicleBackInteriorDocumentData
  | GenericData A.Value
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

instance {-# OVERLAPPING #-} ToSQLObject CommonDocumentData where
  convertToSQLObject = SQLObjectValue . TE.decodeUtf8 . BSL.toStrict . A.encode

parseCommonDocumentDataByType :: DVC.DocumentType -> T.Text -> Either T.Text CommonDocumentData
parseCommonDocumentDataByType docType raw = case docType of
  DVC.TAXDetails -> typedOrReject TaxDetailsData
  DVC.BusinessLicense -> typedOrReject BusinessLicenseData
  DVC.VehicleFront -> typedOrReject VehicleFrontData
  DVC.VehicleFrontInterior -> typedOrReject VehicleFrontInteriorData
  DVC.VehicleBackInterior -> typedOrReject VehicleBackInteriorData
  _ -> Right (genericFromText raw)
  where
    typedOrReject :: A.FromJSON a => (a -> CommonDocumentData) -> Either T.Text CommonDocumentData
    typedOrReject mk = case A.eitherDecodeStrict (TE.encodeUtf8 raw) of
      Right a -> Right (mk a)
      Left err -> Left ("documentData does not match the expected shape for " <> T.pack (show docType) <> ": " <> T.pack err)
    genericFromText t = case A.eitherDecodeStrict (TE.encodeUtf8 t) of
      Right v -> GenericData v
      Left _ -> GenericData (A.String t)

parseCommonDocumentDataSafe :: DVC.DocumentType -> T.Text -> CommonDocumentData
parseCommonDocumentDataSafe docType raw = case parseCommonDocumentDataByType docType raw of
  Right a -> a
  Left _ -> case A.eitherDecodeStrict (TE.encodeUtf8 raw) of
    Right v -> GenericData v
    Left _ -> GenericData (A.String raw)

renderCommonDocumentData :: CommonDocumentData -> T.Text
renderCommonDocumentData = \case
  TaxDetailsData d -> encodeText d
  BusinessLicenseData d -> encodeText d
  VehicleFrontData d -> encodeText d
  VehicleFrontInteriorData d -> encodeText d
  VehicleBackInteriorData d -> encodeText d
  GenericData (A.String t) -> t
  GenericData v -> encodeText v
  where
    encodeText :: A.ToJSON a => a -> T.Text
    encodeText = TE.decodeUtf8 . BSL.toStrict . A.encode
