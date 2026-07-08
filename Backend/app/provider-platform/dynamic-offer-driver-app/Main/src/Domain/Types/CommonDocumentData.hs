{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.CommonDocumentData where

import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import qualified Database.PostgreSQL.Simple.FromField as DPSF
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverPanCard as DPan
import GHC.Generics (Generic)
import qualified Kernel.Types.Beckn.Context as Context
import Sequelize.SQLObject (SQLObject (..), ToSQLObject (..))
import Prelude

data DLDocumentMetadata = DLDocumentMetadata
  { driverLicenseNumber :: T.Text,
    driverDateOfBirth :: Maybe UTCTime,
    dateOfExpiry :: UTCTime
  }
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

data AadhaarDocumentMetadata = AadhaarDocumentMetadata
  { aadhaarNumber :: Maybe T.Text,
    nameOnCard :: Maybe T.Text,
    dateOfBirth :: Maybe T.Text,
    address :: Maybe T.Text
  }
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

data PanDocumentMetadata = PanDocumentMetadata
  { panNumber :: T.Text,
    panDocType :: Maybe DPan.PanType,
    driverDob :: Maybe UTCTime
  }
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

data LocalAddressProofDocumentMetadata = LocalAddressProofDocumentMetadata
  { state :: Maybe Context.IndianState,
    proofDocumentType :: Maybe DI.AddressDocumentType,
    address :: Maybe T.Text
  }
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

newtype GSTDocumentMetadata = GSTDocumentMetadata
  { gstNumber :: T.Text
  }
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

data RCDocumentMetadata = RCDocumentMetadata
  { fitnessExpiry :: UTCTime,
    vehicleNumberPlate :: T.Text,
    vehicleVariant :: Maybe T.Text,
    vehicleManufacturer :: Maybe T.Text,
    vehicleModel :: Maybe T.Text,
    vehicleModelYear :: Maybe Int,
    vehicleColor :: Maybe T.Text
  }
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

data VehiclePUCDocumentMetadata = VehiclePUCDocumentMetadata
  { pucNumber :: Maybe T.Text,
    pucExpiry :: UTCTime
  }
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

data VehicleFitnessCertificateDocumentMetadata = VehicleFitnessCertificateDocumentMetadata
  { fitnessExpiry :: UTCTime,
    applicationNumber :: T.Text,
    rcNumber :: T.Text
  }
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

data VehicleInsuranceDocumentMetadata = VehicleInsuranceDocumentMetadata
  { policyNumber :: T.Text,
    insuranceExpiry :: UTCTime,
    insuranceProvider :: T.Text,
    rcNumber :: T.Text
  }
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

data VehiclePermitDocumentMetadata = VehiclePermitDocumentMetadata
  { permitNumber :: T.Text,
    permitExpiry :: UTCTime,
    regionCovered :: T.Text,
    rcNumber :: T.Text
  }
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

data UDYAMDocumentMetadata = UDYAMDocumentMetadata
  { udyamNumber :: Maybe T.Text,
    tdsRate :: Maybe Double
  }
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

data TANDocumentMetadata = TANDocumentMetadata
  { documentId :: T.Text,
    tdsRate :: Maybe Double
  }
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

data LDCDocumentMetadata = LDCDocumentMetadata
  { documentId :: T.Text,
    tdsRate :: Maybe Double
  }
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

data TaxDetailsDocumentData = TaxDetailsDocumentData
  { identifierNumber :: T.Text,
    expiryDate :: Maybe UTCTime
  }
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

data CommonDocumentData
  = DLData DLDocumentMetadata
  | AadhaarData AadhaarDocumentMetadata
  | PanData PanDocumentMetadata
  | LocalAddressProofData LocalAddressProofDocumentMetadata
  | GSTData GSTDocumentMetadata
  | RCData RCDocumentMetadata
  | VehiclePUCData VehiclePUCDocumentMetadata
  | VehicleFitnessCertificateData VehicleFitnessCertificateDocumentMetadata
  | VehicleInsuranceData VehicleInsuranceDocumentMetadata
  | VehiclePermitData VehiclePermitDocumentMetadata
  | UDYAMData UDYAMDocumentMetadata
  | TANData TANDocumentMetadata
  | LDCData LDCDocumentMetadata
  | TaxDetailsData TaxDetailsDocumentData
  | GenericData A.Value
  deriving (Show, Eq, Ord, Generic, A.ToJSON, A.FromJSON, ToSchema)

fromFieldCommonDocumentData ::
  DPSF.Field ->
  Maybe ByteString ->
  DPSF.Conversion CommonDocumentData
fromFieldCommonDocumentData f mbValue = do
  value <- fromField f mbValue
  case A.fromJSON value of
    A.Success a -> pure a
    _ -> DPSF.returnError DPSF.ConversionFailed f "Conversion failed"

instance HasSqlValueSyntax be A.Value => HasSqlValueSyntax be CommonDocumentData where
  sqlValueSyntax = sqlValueSyntax . A.toJSON

instance FromField CommonDocumentData where
  fromField = fromFieldCommonDocumentData

instance BeamSqlBackend be => B.HasSqlEqualityCheck be CommonDocumentData

instance FromBackendRow Postgres CommonDocumentData

instance {-# OVERLAPPING #-} ToSQLObject CommonDocumentData where
  convertToSQLObject = SQLObjectValue . T.pack . show . A.encode

parseCommonDocumentDataByType :: DVC.DocumentType -> T.Text -> Either T.Text CommonDocumentData
parseCommonDocumentDataByType docType raw = case docType of
  DVC.DriverLicense -> typedOrReject DLData
  DVC.PanCard -> typedOrReject PanData
  DVC.AadhaarCard -> typedOrReject AadhaarData
  DVC.LocalResidenceProof -> typedOrReject LocalAddressProofData
  DVC.GSTCertificate -> typedOrReject GSTData
  DVC.VehicleRegistrationCertificate -> typedOrReject RCData
  DVC.VehiclePUC -> typedOrReject VehiclePUCData
  DVC.VehicleFitnessCertificate -> typedOrReject VehicleFitnessCertificateData
  DVC.VehicleInsurance -> typedOrReject VehicleInsuranceData
  DVC.VehiclePermit -> typedOrReject VehiclePermitData
  DVC.UDYAMCertificate -> typedOrReject UDYAMData
  DVC.TANCertificate -> typedOrReject TANData
  DVC.LDCCertificate -> typedOrReject LDCData
  DVC.TAXDetails -> typedOrReject TaxDetailsData
  _ -> Right (genericFromText raw)
  where
    typedOrReject :: A.FromJSON a => (a -> CommonDocumentData) -> Either T.Text CommonDocumentData
    typedOrReject mk = case A.eitherDecodeStrict (TE.encodeUtf8 raw) of
      Right a -> Right (mk a)
      Left err -> Left ("documentData does not match the expected shape for " <> T.pack (show docType) <> ": " <> T.pack err)
    genericFromText t = case A.eitherDecodeStrict (TE.encodeUtf8 t) of
      Right v -> GenericData v
      Left _ -> GenericData (A.String t)

renderCommonDocumentData :: CommonDocumentData -> T.Text
renderCommonDocumentData = \case
  DLData d -> encodeText d
  AadhaarData d -> encodeText d
  PanData d -> encodeText d
  LocalAddressProofData d -> encodeText d
  GSTData d -> encodeText d
  RCData d -> encodeText d
  VehiclePUCData d -> encodeText d
  VehicleFitnessCertificateData d -> encodeText d
  VehicleInsuranceData d -> encodeText d
  VehiclePermitData d -> encodeText d
  UDYAMData d -> encodeText d
  TANData d -> encodeText d
  LDCData d -> encodeText d
  TaxDetailsData d -> encodeText d
  GenericData (A.String t) -> t
  GenericData v -> encodeText v
  where
    encodeText :: A.ToJSON a => a -> T.Text
    encodeText = TE.decodeUtf8 . BSL.toStrict . A.encode
