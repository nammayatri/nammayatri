module Types.API.External.Core.Person where
  
import           Data.Text
import           EulerHS.Prelude
import           Types.API.External.Core.Contact

data Person =
  Person
    { _title :: Text -- "Mr", "Mrs", "Miss", "Dr"
    , _first_name :: Text
    , _middle_name :: Text
    , _last_name :: Text
    , _full_name :: Text
    , _image :: Image
    , _dob :: Text
    , _gender :: Text -- male, female
    , _contact :: Contact
    }
  deriving (Generic, Show)

instance FromJSON Person where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Person where
  toJSON = genericToJSON stripAllLensPrefixOptions

data Image =
  Image
    { _format :: Text -- "url", "encoded"
    , _data :: Text
    }
  deriving (Generic, Show)

instance FromJSON Image where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Image where
  toJSON = genericToJSON stripAllLensPrefixOptions
