{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.Yudhishthira where

import qualified Data.Aeson as A
import Domain.Types.Person
import qualified Domain.Types.Person as SP
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Version
import qualified Lib.Yudhishthira.Types.Application as YA
import qualified Lib.Yudhishthira.Types.Common as YTC
import qualified Lib.Yudhishthira.TypesTH as YTH

data LoginTagData = LoginTagData
  { id :: Id Person,
    gender :: Maybe SP.Gender,
    clientSdkVersion :: Maybe Version,
    clientBundleVersion :: Maybe Version,
    clientReactNativeVersion :: Maybe Text,
    clientConfigVersion :: Maybe Version,
    clientDevice :: Maybe Device
  }
  deriving (Show, ToJSON, FromJSON, Generic)

$(YTH.generateGenericDefault ''LoginTagData)

instance YTC.LogicInputLink YA.ApplicationEvent where
  getLogicInputDef a =
    case a of
      YA.Login -> fmap A.toJSON . listToMaybe $ YTH.genDef (Proxy @LoginTagData)
      _ -> Nothing
