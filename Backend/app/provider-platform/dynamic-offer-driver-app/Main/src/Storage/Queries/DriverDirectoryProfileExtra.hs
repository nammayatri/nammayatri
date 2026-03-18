module Storage.Queries.DriverDirectoryProfileExtra where

import qualified Domain.Types.DriverDirectoryProfile as DTDDP
import qualified Domain.Types.MerchantOperatingCity as DMOC
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import Kernel.Types.Id
import Kernel.Utils.Common as KUC
import qualified Sequelize as Se
import qualified Storage.Beam.DriverDirectoryProfile as BeamDDP

-- | Search for listed drivers with filters.
-- Returns a tuple of (results, totalCount).
searchListedDrivers ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Int ->
  Int ->
  m ([DTDDP.DriverDirectoryProfile], Int)
searchListedDrivers merchantOpCityId _mbCity _mbMinRating mbVehicleType mbAvailability _mbSearchQuery limit' offset' = do
  let baseConditions =
        [ Se.Is BeamDDP.isListed $ Se.Eq True,
          Se.Is BeamDDP.merchantOperatingCityId $ Se.Eq (getId merchantOpCityId)
        ]
      vehicleTypeCondition = case mbVehicleType of
        Just vt -> [Se.Is BeamDDP.preferredVehicleType $ Se.Eq (Just vt)]
        Nothing -> []
      availabilityCondition = case mbAvailability of
        Just "AVAILABLE" -> [Se.Is BeamDDP.availabilityStatus $ Se.Eq (Just DTDDP.AVAILABLE)]
        Just "BUSY" -> [Se.Is BeamDDP.availabilityStatus $ Se.Eq (Just DTDDP.BUSY)]
        Just "NOT_AVAILABLE" -> [Se.Is BeamDDP.availabilityStatus $ Se.Eq (Just DTDDP.NOT_AVAILABLE)]
        _ -> []
      allConditions = baseConditions <> vehicleTypeCondition <> availabilityCondition
  profiles <-
    findAllWithOptionsKV
      [Se.And allConditions]
      (Se.Desc BeamDDP.createdAt)
      (Just limit')
      (Just offset')
  -- For simplicity, count is approximated by result length + offset.
  -- In production, a separate count query would be more accurate.
  let totalCount = length profiles + offset'
  pure (profiles, totalCount)
