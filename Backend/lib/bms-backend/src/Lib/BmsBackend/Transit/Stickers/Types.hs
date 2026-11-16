module Lib.BmsBackend.Transit.Stickers.Types where

import Data.Aeson
import Data.Char (toLower)
import Data.Map.Strict (Map)
import Data.OpenApi (ToSchema (..), fromAesonOptions, genericDeclareNamedSchema)
import Kernel.Prelude hiding (error)
import Kernel.Types.HideSecrets (HideSecrets (..))
import Lib.BmsBackend.Transit.Common (ListResponse, omitNothing)

enumOptions :: Int -> Options
enumOptions prefix = defaultOptions {constructorTagModifier = map toLower . drop prefix}

data EcLevel = EcL | EcM | EcQ | EcH
  deriving (Generic, Show, Eq)

ecLevelOptions :: Options
ecLevelOptions = defaultOptions {constructorTagModifier = drop 2}

instance ToJSON EcLevel where
  toJSON = genericToJSON ecLevelOptions

instance FromJSON EcLevel where
  parseJSON = genericParseJSON ecLevelOptions

instance ToSchema EcLevel where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions ecLevelOptions)

data TextAlign = AlignLeft | AlignCenter | AlignRight
  deriving (Generic, Show, Eq)

instance ToJSON TextAlign where
  toJSON = genericToJSON (enumOptions 5)

instance FromJSON TextAlign where
  parseJSON = genericParseJSON (enumOptions 5)

instance ToSchema TextAlign where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (enumOptions 5))

data FontStyle = StyleNormal | StyleItalic
  deriving (Generic, Show, Eq)

instance ToJSON FontStyle where
  toJSON = genericToJSON (enumOptions 5)

instance FromJSON FontStyle where
  parseJSON = genericParseJSON (enumOptions 5)

instance ToSchema FontStyle where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (enumOptions 5))

data TextTransform = TransformNone | TransformUpper | TransformLower
  deriving (Generic, Show, Eq)

instance ToJSON TextTransform where
  toJSON = genericToJSON (enumOptions 9)

instance FromJSON TextTransform where
  parseJSON = genericParseJSON (enumOptions 9)

instance ToSchema TextTransform where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (enumOptions 9))

data StickerFormat = FormatPng | FormatPdf
  deriving (Generic, Show, Eq)

instance ToJSON StickerFormat where
  toJSON = genericToJSON (enumOptions 6)

instance FromJSON StickerFormat where
  parseJSON = genericParseJSON (enumOptions 6)

instance ToSchema StickerFormat where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions (enumOptions 6))

data QrBox = QrBox
  { x :: Double,
    y :: Double,
    size :: Double,
    ecc :: EcLevel,
    margin :: Int,
    dark :: Text,
    light :: Text,
    lightTransparent :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data TextBox = TextBox
  { id :: Text,
    label :: Text,
    content :: Text,
    x :: Double,
    y :: Double,
    align :: TextAlign,
    fontFamily :: Text,
    fontWeight :: Int,
    fontStyle :: FontStyle,
    fontSize :: Double,
    letterSpacing :: Double,
    color :: Text,
    maxWidth :: Double,
    transform :: TextTransform,
    zeroAsO :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data StickerLayout = StickerLayout
  { qr :: QrBox,
    texts :: [TextBox]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data QrContent
  = QrSigned {baseUrl :: Text, kid :: Text, stickerSlug :: Text}
  | QrText {text :: Text, stickerSlug :: Text}
  deriving (Generic, Show)

qrContentOptions :: Options
qrContentOptions =
  defaultOptions
    { constructorTagModifier = map toLower . drop 2,
      sumEncoding = TaggedObject {tagFieldName = "mode", contentsFieldName = "contents"}
    }

instance ToJSON QrContent where
  toJSON = genericToJSON qrContentOptions

instance FromJSON QrContent where
  parseJSON = genericParseJSON qrContentOptions

instance ToSchema QrContent where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions qrContentOptions)

data UploadedFile = UploadedFile
  { fileName :: Text,
    dataBase64 :: Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data StickerDesignCreate = StickerDesignCreate
  { organizationId :: Maybe Text,
    name :: Text,
    description :: Maybe Text,
    printWidthIn :: Double,
    layout :: StickerLayout,
    qrContent :: QrContent,
    background :: UploadedFile
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON StickerDesignCreate where
  toJSON = genericToJSON omitNothing

instance HideSecrets StickerDesignCreate where
  hideSecrets = identity

data StickerDesignUpdate = StickerDesignUpdate
  { name :: Text,
    description :: Maybe Text,
    printWidthIn :: Double,
    layout :: StickerLayout,
    qrContent :: QrContent,
    background :: Maybe UploadedFile
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON StickerDesignUpdate where
  toJSON = genericToJSON omitNothing

instance HideSecrets StickerDesignUpdate where
  hideSecrets = identity

data StickerDesignSummary = StickerDesignSummary
  { id :: Text,
    organizationId :: Text,
    name :: Text,
    description :: Text,
    bgWidth :: Int,
    bgHeight :: Int,
    printWidthIn :: Double,
    batches :: Int,
    stickersMade :: Int,
    lastBatchAt :: Maybe UTCTime,
    updatedBy :: Maybe Text,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data StickerDesign = StickerDesign
  { id :: Text,
    organizationId :: Text,
    name :: Text,
    description :: Text,
    bgMime :: Text,
    bgWidth :: Int,
    bgHeight :: Int,
    printWidthIn :: Double,
    layout :: StickerLayout,
    qrContent :: QrContent,
    createdBy :: Maybe Text,
    updatedBy :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data FleetRow = FleetRow
  { fleet :: Text,
    dep :: Maybe Text,
    fields :: Map Text Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON FleetRow where
  toJSON = genericToJSON omitNothing

data StickerPreviewReq = StickerPreviewReq
  { designId :: Maybe Text,
    organizationId :: Maybe Text,
    background :: Maybe UploadedFile,
    printWidthIn :: Double,
    layout :: StickerLayout,
    qrContent :: QrContent,
    sample :: FleetRow
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON StickerPreviewReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets StickerPreviewReq where
  hideSecrets = identity

data StickerGenerateReq = StickerGenerateReq
  { rows :: [FleetRow],
    formats :: [StickerFormat],
    groupByDep :: Bool,
    fileName :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON StickerGenerateReq where
  toJSON = genericToJSON omitNothing

instance HideSecrets StickerGenerateReq where
  hideSecrets = identity

data StickerBatch = StickerBatch
  { id :: Text,
    designId :: Text,
    designName :: Text,
    fleetCount :: Int,
    fleetsSample :: [Text],
    formats :: [Text],
    createdBy :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype FleetFileReq = FleetFileReq
  { file :: UploadedFile
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

instance HideSecrets FleetFileReq where
  hideSecrets = identity

data FleetFileRead = FleetFileRead
  { columns :: [Text],
    fleetColumn :: Text,
    depColumn :: Maybe Text,
    rows :: [FleetRow],
    skippedBlank :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data StickerFontUpload = StickerFontUpload
  { organizationId :: Maybe Text,
    family :: Text,
    weight :: Int,
    style :: FontStyle,
    file :: UploadedFile
  }
  deriving (Generic, Show, FromJSON, ToSchema)

instance ToJSON StickerFontUpload where
  toJSON = genericToJSON omitNothing

instance HideSecrets StickerFontUpload where
  hideSecrets = identity

data StickerFont = StickerFont
  { id :: Text,
    organizationId :: Maybe Text,
    family :: Text,
    weight :: Int,
    style :: FontStyle,
    builtin :: Bool,
    fileName :: Text,
    createdBy :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data StickerSigning = StickerSigning
  { kid :: Maybe Text,
    ready :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data StickerFile = StickerFile
  { fileName :: Text,
    mimeType :: Text,
    dataBase64 :: Text,
    stickerCount :: Maybe Int
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

type StickerDesignSummaryList = ListResponse StickerDesignSummary
