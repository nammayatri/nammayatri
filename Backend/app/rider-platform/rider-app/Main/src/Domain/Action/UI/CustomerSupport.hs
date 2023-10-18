{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.CustomerSupport
  ( OrderResp (..),
    OrderDetails (..),
    OrderInfo (..),
    LoginReq (..),
    LoginRes (..),
    LogoutRes (..),
    login,
    logout,
    listOrder,
  )
where

import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Location as DBLoc
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.Person as SP
import qualified Domain.Types.RegistrationToken as SR
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption (decrypt, getDbHash)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Booking as QRB
import Storage.Queries.Person as Person
import qualified Storage.Queries.RegistrationToken as RegistrationToken
import Tools.Error

newtype OrderResp = OrderResp {order :: OrderDetails}
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data OrderDetails = OrderDetails
  { id :: Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    startTime :: UTCTime,
    endTime :: Maybe UTCTime,
    fromLocation :: DBLoc.LocationAPIEntity,
    toLocation :: Maybe DBLoc.LocationAPIEntity,
    travellerName :: Maybe Text,
    travellerPhone :: Maybe Text,
    rideBooking :: DRB.BookingAPIEntity
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data OrderInfo = OrderInfo
  { person :: SP.Person,
    bookings :: [DRB.Booking]
  }
  deriving (Generic)

data LoginReq = LoginReq
  { email :: Text,
    password :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data LoginRes = LoginRes
  { auth_token :: Text,
    message :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype LogoutRes = LogoutRes {message :: Text}
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

login :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => LoginReq -> m LoginRes
login LoginReq {..} = do
  person <- Person.findByEmailAndPassword email password >>= fromMaybeM (PersonNotFound email)
  unless (person.role == SP.CUSTOMER_SUPPORT) $ throwError Unauthorized
  token <- generateToken person
  pure $ LoginRes token "Logged in successfully"

generateToken :: EsqDBFlow m r => SP.Person -> m Text
generateToken SP.Person {..} = do
  let personId = getId id
  let mkId = getId merchantId
  regToken <- createSupportRegToken personId mkId
  -- Clean Old Login Session
  -- FIXME We should also cleanup old token from Redis
  _ <- RegistrationToken.deleteByPersonId id
  _ <- RegistrationToken.create regToken
  pure $ regToken.token

logout :: (CacheFlow m r, EsqDBFlow m r) => (Id SP.Person, Id Merchant.Merchant) -> m LogoutRes
logout (personId, _) = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  unless (person.role == SP.CUSTOMER_SUPPORT) $ throwError Unauthorized
  -- FIXME We should also cleanup old token from Redis
  _ <- RegistrationToken.deleteByPersonId person.id
  pure $ LogoutRes "Logged out successfully"

createSupportRegToken :: MonadFlow m => Text -> Text -> m SR.RegistrationToken
createSupportRegToken entityId merchantId = do
  rtid <- L.generateGUID
  token <- L.generateGUID
  now <- getCurrentTime
  return $
    SR.RegistrationToken
      { id = Id rtid,
        token = token,
        attempts = 1, -- Token
        authMedium = SR.EMAIL,
        authType = SR.PASSWORD,
        authValueHash = "CUSTOMER_SESSIONTOKEN",
        verified = True,
        authExpiry = 0,
        tokenExpiry = 30, -- Need to Make this Configuable
        entityId = entityId,
        merchantId = merchantId,
        entityType = SR.CUSTOMER,
        createdAt = now,
        updatedAt = now,
        info = Nothing
      }

listOrder :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => Id SP.Person -> Maybe Text -> Maybe Text -> Maybe Integer -> Maybe Integer -> m [OrderResp]
listOrder personId mRequestId mMobile mlimit moffset = do
  supportP <- B.runInReplica $ Person.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  unless (supportP.role == SP.CUSTOMER_SUPPORT) $
    throwError AccessDenied
  OrderInfo {person, bookings} <- case (mRequestId, mMobile) of
    (Just bookingId, _) -> getByRequestId bookingId supportP.merchantId
    (_, Just mobileNumber) -> getByMobileNumber mobileNumber supportP.merchantId
    (_, _) -> throwError $ InvalidRequest "You should pass SearchRequestId or mobile number."
  traverse (buildBookingToOrder person) bookings
  where
    getByMobileNumber number merchantId = do
      let limit_ = maybe 10 (`min` 10) mlimit
      mobileNumberHash <- getDbHash number
      person <-
        B.runInReplica $
          Person.findByRoleAndMobileNumberAndMerchantIdWithoutCC SP.USER mobileNumberHash merchantId
            >>= fromMaybeM (PersonDoesNotExist number)
      bookings <-
        B.runInReplica $ QRB.findAllByPersonIdLimitOffset (person.id) (Just limit_) moffset
      return $ OrderInfo person bookings
    getByRequestId bookingId merchantId = do
      (booking :: DRB.Booking) <-
        B.runInReplica $
          QRB.findByIdAndMerchantId (Id bookingId) merchantId
            >>= fromMaybeM (BookingDoesNotExist bookingId)
      let requestorId = booking.riderId
      person <-
        B.runInReplica $
          Person.findById requestorId
            >>= fromMaybeM (PersonDoesNotExist requestorId.getId)
      return $ OrderInfo person [booking]

buildBookingToOrder :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => SP.Person -> DRB.Booking -> m OrderResp
buildBookingToOrder SP.Person {firstName, lastName, mobileNumber} booking = do
  let mbToLocation = case booking.bookingDetails of
        DRB.RentalDetails _ -> Nothing
        DRB.OneWayDetails details -> Just details.toLocation
        DRB.DriverOfferDetails details -> Just details.toLocation
        DRB.OneWaySpecialZoneDetails details -> Just details.toLocation
  rbStatus <- DRB.buildBookingAPIEntity booking
  decMobNum <- mapM decrypt mobileNumber
  let details =
        OrderDetails
          { id = getId booking.id,
            createdAt = booking.createdAt,
            updatedAt = booking.updatedAt,
            startTime = booking.startTime,
            endTime = Nothing,
            fromLocation = DBLoc.makeLocationAPIEntity booking.fromLocation,
            toLocation = DBLoc.makeLocationAPIEntity <$> mbToLocation,
            travellerName = firstName <> lastName,
            travellerPhone = decMobNum,
            rideBooking = rbStatus
          }
  pure $ OrderResp {order = details}
