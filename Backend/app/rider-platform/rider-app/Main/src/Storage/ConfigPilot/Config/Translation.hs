{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.Translation (TranslationDimensions (..)) where

import qualified Domain.Types.Translations as DT
import qualified Kernel.External.Types as KET
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.Queries.TranslationsExtra as SQ

data TranslationDimensions = TranslationDimensions
  { merchantOperatingCityId :: Maybe Text,
    messageKey :: Text,
    language :: Maybe KET.Language
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'TranslationRider where
  type DimensionsFor 'TranslationRider = TranslationDimensions
  configTypeValue = TranslationRider
  sConfigType = STranslationRider

instance ConfigDimensions TranslationDimensions where
  type ConfigTypeOf TranslationDimensions = 'TranslationRider
  type ConfigValueTypeOf TranslationDimensions = Maybe DT.Translations
  getConfigType _ = TranslationRider
  getConfigList a =
    listToMaybe
      <$> LCP.resolveConfigList
        a
        (LYT.RIDER_CONFIG Translation)
        (Id (fromMaybe "" a.merchantOperatingCityId))
        (SQ.findAllByMessageKey a.messageKey)
        [ LCP.DimMatcher (.language) (Just . (.language)) (==)
        ]
        (Just [LCP.DimMatcher (const (Just KET.ENGLISH)) (Just . (.language)) (==)])
