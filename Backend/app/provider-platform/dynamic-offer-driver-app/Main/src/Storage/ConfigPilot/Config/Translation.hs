{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.ConfigPilot.Config.Translation (TranslationDimensions (..)) where

import Data.List (sortOn)
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

instance ConfigTypeInfo 'TranslationDriver where
  type DimensionsFor 'TranslationDriver = TranslationDimensions
  configTypeValue = TranslationDriver
  sConfigType = STranslationDriver

instance ConfigDimensions TranslationDimensions where
  type ConfigTypeOf TranslationDimensions = 'TranslationDriver
  type ConfigValueTypeOf TranslationDimensions = Maybe DT.Translations
  getConfigType _ = TranslationDriver
  configDBFetch a = Just (listToMaybe . forCity a <$> SQ.findAllByMessageKey a.messageKey)
  getConfigList a =
    listToMaybe
      <$> LCP.resolveConfigList
        a
        (LYT.DRIVER_CONFIG Translation)
        (Id (fromMaybe "" a.merchantOperatingCityId))
        (forCity a <$> SQ.findAllByMessageKey a.messageKey)
        [ LCP.DimMatcher (.language) (Just . (.language)) (==)
        ]
        (Just [LCP.DimMatcher (const (Just KET.ENGLISH)) (Just . (.language)) (==)])

-- | The city's own rows and the shared ones (no city), the city's own first.
forCity :: TranslationDimensions -> [DT.Translations] -> [DT.Translations]
forCity dims = sortOn (isNothing . (.merchantOperatingCityId)) . filter (\r -> isNothing r.merchantOperatingCityId || r.merchantOperatingCityId == cityId)
  where
    cityId = Id <$> dims.merchantOperatingCityId
