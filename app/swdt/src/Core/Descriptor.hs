module Core.Descriptor where 

-- import Beckn.Types.App (BaseUrl)
import Beckn.Types.Core.Migration.Image (Image (..))
import Beckn.Utils.JSON (stripPrefixUnderscoreIfAny)
import Data.OpenApi (ToSchema)
-- import EulerHS.Prelude


import Data.Aeson
import Data.Text
import GHC.Generics
 

data Descriptor = Descriptor
  { name :: Maybe Text,
    code :: Maybe Text,
    symbol :: Maybe Text,
    short_desc :: Maybe Text,
    long_desc :: Maybe Text,
    images :: Maybe [Image]
    --            audio :: Maybe BaseUrl,
    --             _3d_render :: Maybe BaseUrl
  }
  deriving (Generic, Show, Eq, ToSchema)

instance FromJSON Descriptor where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON Descriptor where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny { omitNothingFields = True }

emptyDescriptor :: Descriptor
emptyDescriptor =
  Descriptor
    { name = Nothing,
      code = Nothing,
      symbol = Nothing,
      short_desc = Nothing,
      long_desc = Nothing,
      images = Nothing
    --   audio = Nothing,
    --   _3d_render = Nothing
    }
