module Domain.Action.Dashboard.AppManagement.SeatLayout
  ( upsertSeatLayout,
    getSeatLayout,
    deleteSeatLayout,
    listSeatLayout,
  )
where

import qualified API.Types.Dashboard.AppManagement.SeatLayout as API
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Domain.Types.Seat (Seat (..))
import qualified Domain.Types.Seat as DSeat
import Domain.Types.SeatLayout (SeatLayout (..))
import qualified Domain.Types.SeatLayout as DSeatLayout
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Id (Id, getId)
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Seat as CQSeat
import qualified Storage.Queries.Seat as QSeat
import qualified Storage.Queries.SeatLayout as QSeatLayout
import qualified Storage.Queries.VehicleSeatLayoutMapping as QVehicleSeatLayoutMapping
import Tools.Error

-- Seat type inference helpers

inferSeatType :: Int -> Int -> [(Int, Int)] -> Maybe DSeat.SeatType
inferSeatType targetRow targetCol seatPositions = do
  cols <- NE.nonEmpty [col | (row, col) <- seatPositions, row == targetRow]

  let minCol = minimum cols
      maxCol = maximum cols

      hasLeft = (targetRow, targetCol - 1) `elem` seatPositions
      hasRight = (targetRow, targetCol + 1) `elem` seatPositions

  pure $
    if
        | targetCol == minCol || targetCol == maxCol -> DSeat.WINDOW
        | hasLeft && hasRight -> DSeat.MIDDLE
        | otherwise -> DSeat.AISLE

-- Validation

validateUpsertReq :: DSeatLayout.SeatLayout -> [API.SeatDef] -> Environment.Flow ()
validateUpsertReq layout seats = do
  when (layout.rows < 1 || layout.columns < 1) $ throwError InvalidSeatLayoutDimensions
  let positions = [(s.row, s.col) | s <- seats]
      labels = [s.label | s <- seats]
  forM_ seats $ \seat -> do
    when (seat.row < 1 || seat.row > layout.rows) $ throwError InvalidSeatPosition
    when (seat.col < 1 || seat.col > layout.columns) $ throwError InvalidSeatPosition
  when (length (nub positions) /= length positions) $ throwError DuplicateSeatPosition
  when (length (nub labels) /= length labels) $ throwError DuplicateSeatLabel
  -- Sleeper seats occupy the next row; validate no overlap
  forM_ seats $ \seat -> do
    when (seat.seatType == Just DSeat.SLEEPER_LOWER || seat.seatType == Just DSeat.SLEEPER_UPPER) $ do
      when ((seat.row + 1 > layout.rows || (seat.row + 1, seat.col) `elem` positions)) $ throwError InvalidSleeperSeatPosition

guardNotInUse :: Id DSeatLayout.SeatLayout -> Environment.Flow ()
guardNotInUse layoutId = do
  mappings <- QVehicleSeatLayoutMapping.findAllBySeatLayoutId (Just 1) (Just 0) layoutId
  when (not $ null mappings) $ throwError SeatLayoutInUseCannotModify

verifyOwnership ::
  DSeatLayout.SeatLayout ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  Environment.Flow ()
verifyOwnership layout merchantId merchantOperatingCityId = do
  when (layout.merchantId /= merchantId) $ throwError SeatLayoutNotFound
  when (layout.merchantOperatingCityId /= merchantOperatingCityId) $ throwError SeatLayoutNotFound

-- Build functions

buildSeatLayout ::
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  API.SeatLayoutUpsertReq ->
  UTCTime ->
  DSeatLayout.SeatLayout
buildSeatLayout merchantId merchantOperatingCityId req now =
  DSeatLayout.SeatLayout
    { id = req.seatLayoutId,
      name = req.name,
      rows = req.rows,
      columns = req.columns,
      merchantId = merchantId,
      merchantOperatingCityId = merchantOperatingCityId,
      createdAt = now,
      updatedAt = now
    }

buildSeat ::
  Id DSeatLayout.SeatLayout ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  [(Int, Int)] ->
  API.SeatDef ->
  UTCTime ->
  DSeat.Seat
buildSeat layoutId merchantId merchantOperatingCityId allPositions seatDef now =
  let inferredType = inferSeatType seatDef.row seatDef.col allPositions
      finalType = seatDef.seatType <|> inferredType
   in DSeat.Seat
        { id = Kernel.Types.Id.Id $ getId layoutId <> "-" <> show seatDef.row <> "-" <> show seatDef.col,
          seatLabel = seatDef.label,
          rowNo = seatDef.row,
          colNo = seatDef.col,
          seatType = finalType,
          isBookable = fromMaybe True seatDef.isBookable,
          isLadiesOnly = seatDef.isLadiesOnly,
          seatLayoutId = layoutId,
          merchantId = merchantId,
          merchantOperatingCityId = merchantOperatingCityId,
          directionDegrees = seatDef.directionDegrees,
          minStopsRequired = seatDef.minStopsRequired,
          createdAt = now,
          updatedAt = now
        }

-- Upsert helpers

doCreate ::
  Domain.Types.Merchant.Merchant ->
  Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  API.SeatLayoutUpsertReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
doCreate merchant merchantOperatingCity req = do
  mbExisting <- QSeatLayout.findById req.seatLayoutId
  when (isJust mbExisting) $ throwError SeatLayoutAlreadyExists
  now <- getCurrentTime
  let seatLayout = buildSeatLayout merchant.id merchantOperatingCity.id req now
  let allPositions = [(s.row, s.col) | s <- req.seats]
  validateUpsertReq seatLayout req.seats
  let seats = map (\seatDef -> buildSeat seatLayout.id merchant.id merchantOperatingCity.id allPositions seatDef now) req.seats
  result <- withTryCatch "upsertSeatLayout-create" $ do
    QSeatLayout.create seatLayout
    QSeat.createMany seats
    return Kernel.Types.APISuccess.Success
  case result of
    Left err -> do
      logError $ "SeatLayout create failed for " <> getId req.seatLayoutId <> ", cleaning up: " <> show err
      QSeatLayout.deleteById req.seatLayoutId
      CQSeat.invalidateCacheByLayoutId req.seatLayoutId
      throwError (InvalidRequest $ "Failed to create seat layout: " <> show err)
    Right res -> return res

doUpdate ::
  Domain.Types.Merchant.Merchant ->
  Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  DSeatLayout.SeatLayout ->
  API.SeatLayoutUpsertReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
doUpdate merchant merchantOperatingCity existingLayout req = do
  verifyOwnership existingLayout merchant.id merchantOperatingCity.id
  guardNotInUse existingLayout.id
  oldSeats <- QSeat.findAllByLayoutId existingLayout.id
  now <- getCurrentTime
  let updatedLayout =
        existingLayout
          { name = req.name,
            rows = req.rows,
            columns = req.columns,
            updatedAt = now
          }
  let allPositions = [(s.row, s.col) | s <- req.seats]
  validateUpsertReq updatedLayout req.seats
  let seats = map (\seatDef -> buildSeat existingLayout.id merchant.id merchantOperatingCity.id allPositions seatDef now) req.seats
  result <- withTryCatch "upsertSeatLayout-update" $ do
    QSeat.deleteBySeatLayoutId existingLayout.id
    QSeatLayout.updateByPrimaryKey updatedLayout
    QSeat.createMany seats
    CQSeat.invalidateCacheByLayoutId existingLayout.id
    return Kernel.Types.APISuccess.Success
  case result of
    Left err -> do
      logError $ "SeatLayout update failed for " <> getId existingLayout.id <> ", rolling back: " <> show err
      QSeatLayout.updateByPrimaryKey existingLayout
      QSeat.createMany oldSeats
      CQSeat.invalidateCacheByLayoutId existingLayout.id
      throwError (InvalidRequest $ "Failed to update seat layout: " <> show err)
    Right res -> return res

-- API handlers

upsertSeatLayout ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  API.SeatLayoutUpsertReq ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
upsertSeatLayout merchantShortId opCity req = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  mbExisting <- QSeatLayout.findById req.seatLayoutId
  case mbExisting of
    Nothing -> doCreate merchant merchantOperatingCity req
    Just existingLayout -> doUpdate merchant merchantOperatingCity existingLayout req

getSeatLayout ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id DSeatLayout.SeatLayout ->
  Environment.Flow API.SeatLayoutDetailResp
getSeatLayout merchantShortId opCity seatLayoutId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  seatLayout <- QSeatLayout.findById seatLayoutId >>= fromMaybeM SeatLayoutNotFound
  verifyOwnership seatLayout merchant.id merchantOperatingCity.id
  seats <- CQSeat.findAllByLayoutId seatLayoutId
  return $
    API.SeatLayoutDetailResp
      { id = seatLayout.id,
        name = seatLayout.name,
        rows = seatLayout.rows,
        columns = seatLayout.columns,
        seats = map toSeatItem seats
      }
  where
    toSeatItem seat =
      API.SeatItem
        { id = seat.id,
          row = seat.rowNo,
          col = seat.colNo,
          label = seat.seatLabel,
          seatType = seat.seatType,
          isBookable = seat.isBookable,
          isLadiesOnly = seat.isLadiesOnly,
          directionDegrees = seat.directionDegrees,
          minStopsRequired = seat.minStopsRequired
        }

deleteSeatLayout ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id DSeatLayout.SeatLayout ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteSeatLayout merchantShortId opCity seatLayoutId = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  seatLayout <- QSeatLayout.findById seatLayoutId >>= fromMaybeM SeatLayoutNotFound
  verifyOwnership seatLayout merchant.id merchantOperatingCity.id
  guardNotInUse seatLayoutId
  QSeat.deleteBySeatLayoutId seatLayoutId
  QSeatLayout.deleteById seatLayoutId
  CQSeat.invalidateCacheByLayoutId seatLayoutId
  return Kernel.Types.APISuccess.Success

listSeatLayout ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Environment.Flow [API.SeatLayoutListItem]
listSeatLayout merchantShortId opCity limit offset = do
  merchant <- findMerchantByShortId merchantShortId
  merchantOperatingCity <-
    CQMOC.findByMerchantShortIdAndCity merchantShortId opCity
      >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchantShortId: " <> merchantShortId.getShortId <> " ,city: " <> show opCity)
  seatLayouts <- QSeatLayout.findAllByMerchantIdAndMerchantOperatingCityId limit offset merchant.id merchantOperatingCity.id
  mapM toListItem seatLayouts
  where
    toListItem layout = do
      seats <- CQSeat.findAllByLayoutId layout.id
      return $
        API.SeatLayoutListItem
          { id = layout.id,
            name = layout.name,
            rows = layout.rows,
            columns = layout.columns,
            seatCount = length seats
          }
