module Beckn.Types.External.Core.Error where
  
import           Data.Text
import           EulerHS.Prelude

data Error =
  Error
    { _t_error :: Maybe TError --"ISO 3166-1 ALPHA-2", "ISO 3166-1 ALPHA-3", "ISO 3166-1 NUMERIC)"
    , _r_error :: RError
    , _f_error :: Maybe [FError]
    , _d_error :: Maybe Text -- it may changes
    }
      deriving (Generic, Show)

instance FromJSON Error where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON Error where
  toJSON = genericToJSON stripAllLensPrefixOptions

data TError =
  TError 
    { _t_protocol :: Text
    , _t_version :: Text
    , _t_version :: Text
    }
      deriving (Generic, Show)

instance FromJSON TError where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON TError where
  toJSON = genericToJSON stripAllLensPrefixOptions

data RError =
  RError 
    { _rcode :: Text
    , _rmessage :: Text
    }
      deriving (Generic, Show) 

instance FromJSON RError where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON RError where
  toJSON = genericToJSON stripAllLensPrefixOptions

data FError =
  FError 
    { _fpath :: Text
    , _fcode :: Text
    , _fmessage :: Text
    }
      deriving (Generic, Show) 

instance FromJSON FError where
  parseJSON = genericParseJSON stripAllLensPrefixOptions

instance ToJSON FError where
  toJSON = genericToJSON stripAllLensPrefixOptions
