module Beckn.Types.API.Pass where

import           Beckn.Types.App
import           Beckn.Types.Common
import           Beckn.Types.Storage.Pass
import           Data.Swagger
import           EulerHS.Prelude
import           Data.Time.LocalTime
import qualified Beckn.Types.Storage.Comment         as SCM
import qualified Beckn.Types.Storage.Customer        as SC
import qualified Beckn.Types.Storage.Document        as SD
import qualified Beckn.Types.Storage.Organization    as SO
import qualified Beckn.Types.Storage.Tag             as ST


data PassRes =
  PassRes
    { _pass :: PassInfo
    } deriving (Generic, ToSchema)

data UpdatePassReq =
  UpdatePassReq
    { _action       :: Maybe Status
    , _CustomerId   :: Maybe CustomerId
    , _fromLocation :: Maybe Location
    , _toLocation   :: Maybe Location
    } deriving (Generic, ToSchema)

instance FromJSON UpdatePassReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data ListPassReq =
  ListPassReq
    { _identifierType :: PassIDType
    , _identifier     :: Text
    , _limit          :: Int
    , _offset         :: Int
    , __type          ::  PassType
    } deriving (Generic, ToSchema)

instance FromJSON ListPassReq where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

data ListPassRes =
  ListPassRes
    { passes :: [PassInfo]
    } deriving (Generic, ToJSON, ToSchema)

data PassInfo = PassInfo
  {   _id                   :: PassId
    , _ShortId              :: Text
    , _TenantOrganizationId :: (Maybe TenantOrganizationId)
    , _status               :: Status
    , _fromDate             :: LocalTime
    , _toDate               :: LocalTime
    , _passType             :: PassType
    , _PassApplicationId    :: PassApplicationId
    , _CreatedBy            :: CustomerId
    , _info                 :: Text
    , _createdAt            :: LocalTime
    , _updatedAt            :: LocalTime

    , _fromLocation         :: Location
    , _toLocation           :: Location
    , _Organization         :: (Maybe SO.Organization)
    , _Customer             :: (Maybe SC.Customer)
    , _Comments             :: [SCM.Comment]
    , _Tags                 :: [ST.Tag]
    , _Documents            :: [SD.Document]
  } deriving (Generic, ToSchema)

instance ToJSON PassInfo where
  toJSON = genericToJSON stripLensPrefixOptions

instance ToJSON PassRes where
  toJSON = genericToJSON stripLensPrefixOptions