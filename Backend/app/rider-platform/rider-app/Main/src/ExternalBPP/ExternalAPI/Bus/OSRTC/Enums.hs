module ExternalBPP.ExternalAPI.Bus.OSRTC.Enums where

import Data.Aeson.Types (Parser)
import Kernel.Prelude

-- Conversion to and from Int ID that works for most of the Enums.
-- Doesn't work for PGPaymentStatus or Quota because of different numbering order
-- Most of these enums will be used in parsing or sending, so fromJSON and toJSON are unnecessary
toOSRTCId :: Enum a => a -> Int
toOSRTCId = (+ 1) . fromEnum

fromOSRTCId :: forall a. (Enum a, Bounded a) => Int -> Maybe a
fromOSRTCId n
  | n >= 1 && n <= fromEnum (maxBound :: a) + 1 = Just $ toEnum (n - 1)
  | otherwise = Nothing

-- TicketCategory: Adult(1), Child(2), Men(3), Women(4)
data OSRTCTicketCategory = Adult | Child | Men | Women
  deriving (Show, Eq, Ord, Read, Generic, ToSchema, Enum, Bounded)

instance FromJSON OSRTCTicketCategory where
  parseJSON v = do
    n <- parseJSON v :: Parser Int
    case fromOSRTCId n of
      Just t -> pure t
      Nothing -> fail $ "OSRTCTicketCategory: invalid id " <> show n

instance ToJSON OSRTCTicketCategory where
  toJSON = toJSON . toOSRTCId

-- Gender: Male(1), Female(2), ThirdGender(3)
data OSRTCGender = Male | Female | ThirdGender
  deriving (Show, Eq, Ord, Read, Generic, ToSchema, Enum, Bounded)

instance FromJSON OSRTCGender where
  parseJSON v = do
    n <- parseJSON v :: Parser Int
    case fromOSRTCId n of
      Just t -> pure t
      Nothing -> fail $ "OSRTCGender: invalid id " <> show n

instance ToJSON OSRTCGender where
  toJSON = toJSON . toOSRTCId

-- SeatType: Seater(1), Sleeper(2), SemiSleeper(3)
data OSRTCSeatType = Seater | Sleeper | SemiSleeper
  deriving (Show, Eq, Ord, Read, Generic, ToSchema, Enum, Bounded)

instance FromJSON OSRTCSeatType where
  parseJSON v = do
    n <- parseJSON v :: Parser Int
    case fromOSRTCId n of
      Just t -> pure t
      Nothing -> fail $ "OSRTCSeatType: invalid id " <> show n

instance ToJSON OSRTCSeatType where
  toJSON = toJSON . toOSRTCId

-- BerthType: LowerBerth(1), UpperBerth(2)
data OSRTCBerthType = LowerBerth | UpperBerth
  deriving (Show, Eq, Ord, Read, Generic, ToSchema, Enum, Bounded)

instance FromJSON OSRTCBerthType where
  parseJSON v = do
    n <- parseJSON v :: Parser Int
    case fromOSRTCId n of
      Just t -> pure t
      Nothing -> fail $ "OSRTCBerthType: invalid id " <> show n

instance ToJSON OSRTCBerthType where
  toJSON = toJSON . toOSRTCId

-- PGPaymentStatus: Pending(0), PGFailed(1), PGSuccess(2), RefundInitiate(3)
-- str* fields (strStatusCode, strBankStatus) expect string encoding per OSRTC's naming convention;
-- use fromEnum to derive intPGPaymentStatusID from the same enum.
data OSRTCPGPaymentStatus = PENDING | FAILED | SUCCESS | REFUND_INITIATE
  deriving (Show, Eq, Ord, Read, Generic, ToSchema, Enum, Bounded, FromJSON, ToJSON)

-- LayoutComponent: EntryDoor(1), ExitDoor(2), PassengerSeat(3), BlockedSpace(4),
--   ConductorSeat(5), DriverSeat(6), EmergencyWindow(7), EmergencyDoor(8), BlockedSeat(9)
data OSRTCLayoutComponent
  = EntryDoor
  | ExitDoor
  | PassengerSeat
  | BlockedSpace
  | ConductorSeat
  | DriverSeat
  | EmergencyWindow
  | EmergencyDoor
  | BlockedSeat
  deriving (Show, Eq, Ord, Read, Generic, ToSchema, Enum, Bounded)

instance FromJSON OSRTCLayoutComponent where
  parseJSON v = do
    n <- parseJSON v :: Parser Int
    case fromOSRTCId n of
      Just t -> pure t
      Nothing -> fail $ "OSRTCLayoutComponent: invalid id " <> show n

instance ToJSON OSRTCLayoutComponent where
  toJSON = toJSON . toOSRTCId

-- SeatStatus: Available(1), Selected(2), Booked(3), SingleLady(4),
--   ReservedForLadies(5), Blocked(6), Conductor(7), TempBooked(8),
--   Reserved(9), Unavailable(10)
data OSRTCSeatStatus
  = SeatAvailable
  | SeatSelected
  | SeatBooked
  | SeatSingleLady
  | SeatReservedForLadies
  | SeatBlocked
  | SeatConductor
  | SeatTempBooked
  | SeatReserved
  | SeatUnavailable
  deriving (Show, Eq, Ord, Read, Generic, ToSchema, Enum, Bounded)

instance FromJSON OSRTCSeatStatus where
  parseJSON v = do
    n <- parseJSON v :: Parser Int
    case fromOSRTCId n of
      Just t -> pure t
      Nothing -> fail $ "OSRTCSeatStatus: invalid id " <> show n

instance ToJSON OSRTCSeatStatus where
  toJSON = toJSON . toOSRTCId

-- Quota: GN(General), VI(VIP), HC(Handicapped), SC(SeniorCitizen),
--   UR(Unreserved), SL(SingleLady), SJ(Soldier), SR(SingleLadyReserve)
-- No Bounded derivation: OSRTC IDs are non-contiguous (1,3,4,5,6,9,10,11),
-- so [minBound..maxBound] would crash via toEnum on the gaps.
data OSRTCQuota = GN | VI | HC | SC | UR | SL | SJ | SR
  deriving (Show, Eq, Ord, Read, Generic, ToSchema, FromJSON, ToJSON)

-- | Total Int -> OSRTCQuota conversion. Use this instead of `toEnum` whenever
-- the input may be unvalidated upstream data — OSRTC IDs are non-contiguous
-- so `toEnum` on a gap value (2, 7, 8, …) would crash.
fromOSRTCQuotaId :: Int -> Maybe OSRTCQuota
fromOSRTCQuotaId 1 = Just GN
fromOSRTCQuotaId 3 = Just VI
fromOSRTCQuotaId 4 = Just HC
fromOSRTCQuotaId 5 = Just SC
fromOSRTCQuotaId 6 = Just UR
fromOSRTCQuotaId 9 = Just SL
fromOSRTCQuotaId 10 = Just SJ
fromOSRTCQuotaId 11 = Just SR
fromOSRTCQuotaId _ = Nothing

instance Enum OSRTCQuota where
  -- WARNING: partial. Only call with values from the OSRTC ID list above.
  -- For unvalidated input, use `fromOSRTCQuotaId` instead.
  toEnum 1 = GN
  toEnum 3 = VI
  toEnum 4 = HC
  toEnum 5 = SC
  toEnum 6 = UR
  toEnum 9 = SL
  toEnum 10 = SJ
  toEnum 11 = SR
  toEnum n = error $ "OSRTCQuota.toEnum: bad argument " <> show n <> " (use fromOSRTCQuotaId for unvalidated input)"
  fromEnum GN = 1
  fromEnum VI = 3
  fromEnum HC = 4
  fromEnum SC = 5
  fromEnum UR = 6
  fromEnum SL = 9
  fromEnum SJ = 10
  fromEnum SR = 11
