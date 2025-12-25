module Dashboard.Common.MediaFileDocument
  ( MediaFileDocument,
    MediaFileDocumentType (..),
  )
where

import Data.Aeson
import Kernel.Prelude
import Kernel.Utils.TH (mkHttpInstancesForEnum)

data MediaFileDocument

data MediaFileDocumentType
  = VehicleVideo
  deriving stock (Show, Generic)
  deriving anyclass (ToSchema, ToParamSchema)

-- Type with single constructor will be serialized as empty list by default, that's why manual instances required
instance ToJSON MediaFileDocumentType where
  toJSON VehicleVideo = String "VehicleVideo"

instance FromJSON MediaFileDocumentType where
  parseJSON (String "VehicleVideo") = pure VehicleVideo
  parseJSON _ = fail "VehicleVideo expected"

$(mkHttpInstancesForEnum ''MediaFileDocumentType)
