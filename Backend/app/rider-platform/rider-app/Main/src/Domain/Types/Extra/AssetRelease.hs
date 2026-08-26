{-# LANGUAGE DeriveAnyClass #-}

module Domain.Types.Extra.AssetRelease where

import Data.Aeson
import qualified Data.Text as T
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Utils.TH

data AssetType = PUBLIC_TRANSPORT_DATA
  deriving (Show, Eq, Ord, Read, Generic, ToSchema, ToParamSchema)

instance ToJSON AssetType where
  toJSON = String . T.pack . show

instance FromJSON AssetType where
  parseJSON = withText "AssetType" $ \t ->
    maybe (fail $ "Invalid AssetType: " <> T.unpack t) pure (readMaybe (T.unpack t))

$(Kernel.Utils.TH.mkHttpInstancesForEnum ''AssetType)

$(Kernel.Utils.TH.mkHttpInstancesForListOfEnums ''AssetType)

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnum ''AssetType)
