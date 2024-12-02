{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Cac.FarePolicy.FarePolicyIntercitySourceDestinationCityDetails where

import Data.Text as Text
import qualified Domain.Types.FarePolicy as DFP
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id as KTI
import Kernel.Utils.Common
import qualified Storage.Beam.FarePolicy.FarePolicyIntercitySourceDestinationCityDetails as BeamFPICSDPS
import Storage.Queries.FarePolicy.FarePolicyIntercitySourceDestinationCityDetails ()
import Utils.Common.CacUtils

findFarePolicyIntercitySourceDestinationCityDetailsFromCAC :: (CacheFlow m r, EsqDBFlow m r) => [(CacContext, Value)] -> String -> Id DFP.FarePolicy -> Int -> m [BeamFPICSDPS.FullFarePolicyIntercitySourceDestinationCityDetails]
findFarePolicyIntercitySourceDestinationCityDetailsFromCAC context tenant id toss = do
  res :: (Maybe [BeamFPICSDPS.FarePolicyIntercitySourceDestinationCityDetails]) <- getConfigListFromCac context tenant toss FarePolicyIntercitySourceDestinationCityDetails (Text.unpack id.getId)
  let config = mapM fromCacType (fromMaybe [] res)
  catMaybes <$> config

instance FromCacType BeamFPICSDPS.FarePolicyIntercitySourceDestinationCityDetails BeamFPICSDPS.FullFarePolicyIntercitySourceDestinationCityDetails where
  fromCacType = fromTType'
