{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Types.UI.FollowRide where
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth
import Data.OpenApi (ToSchema)
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Domain.Types.Booking
import qualified Data.Text



data ContactsDetail
    = ContactsDetail {personId :: Kernel.Types.Id.Id Domain.Types.Person.Person, updateTime :: Kernel.Prelude.UTCTime}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data EmergencyContactsStatusRes
    = EmergencyContactsStatusRes {details :: [ContactsDetail]}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data FollowRideCustomerDetailsRes
    = FollowRideCustomerDetailsRes {bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking, customerName :: Data.Text.Text, customerPhone :: Kernel.Prelude.Maybe Data.Text.Text}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data Followers
    = Followers {bookingId :: Kernel.Types.Id.Id Domain.Types.Booking.Booking,
                 mobileNumber :: Data.Text.Text,
                 name :: Kernel.Prelude.Maybe Data.Text.Text,
                 personId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
                 priority :: Kernel.Prelude.Int}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)
data ShareRideReq
    = ShareRideReq {emergencyContactNumbers :: [Data.Text.Text]}
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)



