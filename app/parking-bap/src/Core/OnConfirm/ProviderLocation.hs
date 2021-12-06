module Core.OnConfirm.ProviderLocation where

import Beckn.Prelude

data Address = Address
  { door :: Text,
    name :: Text,
    building :: Text,
    street :: Text,
    locality :: Text,
    state :: Text,
    country :: Text,
    area_code :: Text
  }
  deriving (Generic, FromJSON)

data Country = Country
  { name :: Text,
    code :: Text
  }
  deriving (Generic, FromJSON)

data City = City
  { name :: Text,
    code :: Text
  }
  deriving (Generic, FromJSON)

data ProviderLocation = ProviderLocation
  { id :: Text,
    gps :: Text,
    address :: Address,
    station_code :: Text,
    city :: City,
    country :: Country
  }
  deriving (Generic, FromJSON)