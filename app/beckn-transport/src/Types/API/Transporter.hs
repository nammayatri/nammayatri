module Types.API.Transporter where

import Beckn.TypeClass.Transform
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Types.Predicate
import qualified Beckn.Types.Storage.Location as SL
import qualified Beckn.Types.Storage.Organization as SO
import qualified Beckn.Utils.Predicates as P
import Beckn.Utils.Validation
import EulerHS.Prelude hiding (id, state)
import qualified Storage.Queries.Location as QL
import Types.API.Registration

data TransporterReq = TransporterReq
  { name :: Text,
    description :: Maybe Text,
    mobileNumber :: Text,
    mobileCountryCode :: Text,
    gstin :: Maybe Text,
    headCount :: Maybe Int,
    locationType :: Maybe SL.LocationType,
    lat :: Maybe Double,
    long :: Maybe Double,
    ward :: Maybe Text,
    district :: Text,
    city :: Text,
    state :: Maybe Text,
    country :: Text,
    pincode :: Maybe Text,
    address :: Maybe Text
  }
  deriving (Generic, FromJSON)

validateTransporterReq :: Validate TransporterReq
validateTransporterReq TransporterReq {..} =
  sequenceA_
    [ validateField "name" name $ MinLength 3 `And` P.name,
      validateField "description" description $ InMaybe $ MinLength 3 `And` P.name,
      validateField "address" address $
        InMaybe $
          NotEmpty `And` star (P.alphanum \/ anyOf " ,./"),
      validateField "mobileNumber" mobileNumber P.mobileNumber
    ]

instance DBFlow m r => CreateTransform TransporterReq SO.Organization m where
  createTransform req = do
    oid <- generateGUID
    let shortId = ShortId $ getId oid
    now <- getCurrentTime
    location <- transformToLocation req
    QL.createFlow location
    return $
      SO.Organization
        { SO.id = oid,
          SO.name = req.name,
          SO.shortId = shortId,
          SO.description = req.description,
          SO.mobileNumber = Just req.mobileNumber,
          SO.mobileCountryCode = Just req.mobileCountryCode,
          SO.gstin = req.gstin,
          SO.locationId = Just location.id,
          SO._type = SO.PROVIDER,
          SO.domain = Just SO.MOBILITY,
          SO.fromTime = Nothing,
          SO.toTime = Nothing,
          SO.headCount = req.headCount,
          SO.apiKey = Nothing,
          SO.callbackUrl = Nothing,
          SO.status = SO.PENDING_VERIFICATION,
          SO.verified = False,
          SO.enabled = True,
          SO.createdAt = now,
          SO.updatedAt = now,
          SO.callbackApiKey = Nothing,
          SO.info = Nothing
        }

transformToLocation :: DBFlow m r => TransporterReq -> m SL.Location
transformToLocation req = do
  locId <- generateGUID
  now <- getCurrentTime
  return $
    SL.Location
      { SL.id = locId,
        SL.locationType = fromMaybe SL.PINCODE $ req.locationType,
        SL.lat = req.lat,
        SL.long = req.long,
        SL.ward = req.ward,
        SL.district = Just $ req.district,
        SL.city = Just $ req.city,
        SL.state = req.state,
        SL.country = Just $ req.country,
        SL.pincode = req.pincode,
        SL.address = req.address,
        SL.bound = Nothing,
        SL.point = SL.Point,
        SL.createdAt = now,
        SL.updatedAt = now
      }

data TransporterRes = TransporterRes
  { user :: UserInfoRes,
    organization :: SO.Organization
  }
  deriving (Generic, ToJSON)

newtype TransporterRec = TransporterRec
  { organization :: SO.Organization
  }
  deriving (Generic, ToJSON)

data UpdateTransporterReq = UpdateTransporterReq
  { name :: Maybe Text,
    description :: Maybe Text,
    headCount :: Maybe Int,
    enabled :: Maybe Bool
  }
  deriving (Generic, Show, FromJSON)

instance DBFlow m r => ModifyTransform UpdateTransporterReq SO.Organization m where
  modifyTransform req org = do
    now <- getCurrentTime
    return $
      org{SO.name = fromMaybe (org.name) (req.name),
          SO.description = (req.description) <|> (org.description),
          SO.headCount = (req.headCount) <|> (org.headCount),
          SO.enabled = fromMaybe (org.enabled) (req.enabled),
          SO.updatedAt = now
         }
