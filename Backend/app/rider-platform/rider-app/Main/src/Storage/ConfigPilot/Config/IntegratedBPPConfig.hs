{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.ConfigPilot.Config.IntegratedBPPConfig (IntegratedBPPConfigDimensions (..)) where

import qualified BecknV2.OnDemand.Enums as BOE
import qualified Domain.Types.IntegratedBPPConfig as DT
import Kernel.Prelude
import Kernel.Types.Id
import qualified Lib.ConfigPilot.Interface.Getter as LCP
import Lib.ConfigPilot.Interface.Types
import qualified Lib.Yudhishthira.Types as LYT
import Lib.Yudhishthira.Types.ConfigPilot (ConfigType (..))
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.IntegratedBPPConfig as SQ

data IntegratedBPPConfigDimensions = IntegratedBPPConfigDimensions
  { merchantOperatingCityId :: Text,
    configId :: Maybe Text,
    agencyKey :: Maybe Text,
    domain :: Maybe Text,
    vehicleCategory :: Maybe BOE.VehicleCategory,
    platformType :: Maybe DT.PlatformType
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

instance ConfigTypeInfo 'IntegratedBPPConfig where
  type DimensionsFor 'IntegratedBPPConfig = IntegratedBPPConfigDimensions
  configTypeValue = IntegratedBPPConfig
  sConfigType = SIntegratedBPPConfig

instance ConfigDimensions IntegratedBPPConfigDimensions where
  type ConfigTypeOf IntegratedBPPConfigDimensions = 'IntegratedBPPConfig
  type ConfigValueTypeOf IntegratedBPPConfigDimensions = [DT.IntegratedBPPConfig]
  getConfigType _ = IntegratedBPPConfig
  getConfigList a =
    let fetch = case (a.configId, a.agencyKey, nonEmpty a.merchantOperatingCityId, a.domain, a.vehicleCategory, a.platformType) of
          (Just cId, _, _, _, _, _) -> maybeToList <$> SQ.findById (Id cId)
          (_, Just ak, _, _, _, _) -> maybeToList <$> SQ.findByAgencyId ak
          (_, _, Just mocId, Just d, Just vc, Just pt) -> SQ.findAllByDomainAndCityAndVehicleCategory d (Id mocId) vc pt
          (_, _, Nothing, Just d, Just vc, Just pt) -> SQ.findAllByPlatformAndVehicleCategory d vc pt
          _ -> pure []
     in LCP.resolveConfigList
          a
          (LYT.RIDER_CONFIG IntegratedBPPConfig)
          (Id a.merchantOperatingCityId)
          fetch
          [ LCP.DimMatcher (.configId) (\c -> Just c.id.getId) (==),
            LCP.DimMatcher (.agencyKey) (\c -> Just c.agencyKey) (==),
            LCP.DimMatcher (.domain) (\c -> Just c.domain) (==),
            LCP.DimMatcher (.vehicleCategory) (\c -> Just c.vehicleCategory) (==),
            LCP.DimMatcher (.platformType) (\c -> Just c.platformType) (==)
          ]
          Nothing
    where
      nonEmpty "" = Nothing
      nonEmpty x = Just x
