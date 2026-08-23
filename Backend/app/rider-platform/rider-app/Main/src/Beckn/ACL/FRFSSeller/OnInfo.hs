module Beckn.ACL.FRFSSeller.OnInfo
  ( SellerEntityInfo (..),
    GstInfo (..),
    PanInfo (..),
    parseSellerEntityInfo,
    buildOnInfoReq,
  )
where

import qualified Beckn.ACL.FRFSSeller.OnSearch as OnSearch
import qualified BecknV2.FRFS.Types as Spec
import qualified Data.Aeson as A
import Kernel.Prelude
import Kernel.Types.TimeRFC339 (UTCTimeRFC3339 (..))
import Kernel.Utils.JSON (constructorsWithSnakeCase)

data SellerEntityInfo = SellerEntityInfo
  { gst :: GstInfo,
    pan :: PanInfo,
    nameOfAuthorisedSignatory :: Text,
    addressOfAuthorisedSignatory :: Text,
    emailId :: Text,
    mobileNo :: Double,
    country :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON SellerEntityInfo where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON SellerEntityInfo where
  toJSON = genericToJSON constructorsWithSnakeCase

data GstInfo = GstInfo
  { legalEntityName :: Text,
    businessAddress :: Text,
    cityCode :: [Text],
    gstNo :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON GstInfo where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON GstInfo where
  toJSON = genericToJSON constructorsWithSnakeCase

data PanInfo = PanInfo
  { nameAsPerPan :: Text,
    panNo :: Text,
    dateOfIncorporation :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON PanInfo where
  parseJSON = genericParseJSON constructorsWithSnakeCase

instance ToJSON PanInfo where
  toJSON = genericToJSON constructorsWithSnakeCase

parseSellerEntityInfo :: Maybe A.Value -> Maybe SellerEntityInfo
parseSellerEntityInfo mbValue = do
  value <- mbValue
  case A.fromJSON value of
    A.Success info -> Just info
    A.Error _ -> Nothing

buildOnInfoReq :: OnSearch.SellerIdentity -> UTCTime -> Spec.Context -> SellerEntityInfo -> Spec.OnInfoReq
buildOnInfoReq self now ctx info =
  Spec.OnInfoReq
    { onInfoReqContext = mkContext self now ctx,
      onInfoReqMessage = Spec.OnInfoMessage {onInfoMessageInfo = mkInfo info}
    }

mkContext :: OnSearch.SellerIdentity -> UTCTime -> Spec.Context -> Spec.Context
mkContext self now ctx =
  ctx{Spec.contextAction = Just "on_info",
      Spec.contextBppId = Just self.subscriberId,
      Spec.contextBppUri = Just self.subscriberUrl,
      Spec.contextTimestamp = Just (UTCTimeRFC3339 now),
      Spec.contextTtl = Just self.callbackTtl,
      Spec.contextVersion = Just self.contextVersion
     }

mkInfo :: SellerEntityInfo -> Spec.Info
mkInfo info =
  Spec.Info
    { infoType = "BPP",
      infoEntity =
        Spec.Entity
          { entityGst =
              Spec.EntityGst
                { entityGstLegalEntityName = info.gst.legalEntityName,
                  entityGstBusinessAddress = info.gst.businessAddress,
                  entityGstCityCode = info.gst.cityCode,
                  entityGstGstNo = info.gst.gstNo
                },
            entityPan =
              Spec.EntityPan
                { entityPanNameAsPerPan = info.pan.nameAsPerPan,
                  entityPanPanNo = info.pan.panNo,
                  entityPanDateOfIncorporation = info.pan.dateOfIncorporation
                },
            entityNameOfAuthorisedSignatory = info.nameOfAuthorisedSignatory,
            entityAddressOfAuthorisedSignatory = info.addressOfAuthorisedSignatory,
            entityEmailId = info.emailId,
            entityMobileNo = info.mobileNo,
            entityCountry = info.country
          }
    }
