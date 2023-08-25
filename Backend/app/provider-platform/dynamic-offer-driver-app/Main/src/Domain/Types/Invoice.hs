module Domain.Types.Invoice where

import Data.Aeson
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as DT
import qualified Database.Beam as B
import Database.Beam.Backend
import Database.Beam.Postgres
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import qualified Domain.Types.DriverFee as DF
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney, fromFieldEnum)
import Kernel.Types.Id
import Servant.API

data Invoice = Invoice
  { id :: Id Invoice,
    invoiceShortId :: Text,
    driverFeeId :: Id DF.DriverFee,
    invoiceStatus :: InvoiceStatus,
    maxMandateAmount :: Maybe HighPrecMoney,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, ToSchema)

data InvoiceStatus = ACTIVE_INVOICE | INACTIVE | SUCCESS | FAILED | EXPIRED deriving (Show, Read, Eq, Generic, FromJSON, ToJSON, ToSchema, Ord)

instance FromField InvoiceStatus where
  fromField = fromFieldEnum

instance HasSqlValueSyntax be String => HasSqlValueSyntax be InvoiceStatus where
  sqlValueSyntax = autoSqlValueSyntax

instance BeamSqlBackend be => B.HasSqlEqualityCheck be InvoiceStatus

instance FromBackendRow Postgres InvoiceStatus

instance IsString InvoiceStatus where
  fromString = show

instance FromHttpApiData InvoiceStatus where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = BF.first T.pack . eitherDecode . BSL.fromStrict

instance ToHttpApiData InvoiceStatus where
  toUrlPiece = DT.decodeUtf8 . toHeader
  toQueryParam = toUrlPiece
  toHeader = BSL.toStrict . encode
