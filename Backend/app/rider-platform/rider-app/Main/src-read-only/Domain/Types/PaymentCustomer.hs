{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.PaymentCustomer where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.External.Payment.Interface.Types
import qualified Domain.Types.Extra.MerchantPaymentMethod
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Tools.Beam.UtilsTH



data PaymentCustomer
    = PaymentCustomer {clientAuthToken :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
                       clientAuthTokenExpiry :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
                       customerId :: Kernel.External.Payment.Interface.Types.CustomerId,
                       defaultPaymentMethodId :: Kernel.Prelude.Maybe Kernel.External.Payment.Interface.Types.PaymentMethodId,
                       paymentMode :: Kernel.Prelude.Maybe Domain.Types.Extra.MerchantPaymentMethod.PaymentMode,
                       personId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
                       createdAt :: Kernel.Prelude.UTCTime,
                       updatedAt :: Kernel.Prelude.UTCTime}
    deriving (Generic, ( ToJSON), ( FromJSON), ( Show))



