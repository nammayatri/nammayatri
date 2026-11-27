-- | What the driver app provides to the permit library.
module SharedLogic.Permit
  ( resolveCity,
    mkPermitHost,
  )
where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment (Flow)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Lib.Permit.Otp (PermitHost (..))
import SharedLogic.Merchant (findMerchantByShortId)
import qualified SharedLogic.OTP as SOTP
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

resolveCity :: ShortId DM.Merchant -> Context.City -> Flow (Id DM.Merchant, Id DMOC.MerchantOperatingCity)
resolveCity merchantShortId city = do
  merchant <- findMerchantByShortId merchantShortId
  cityId <- CQMOC.getMerchantOpCityId Nothing merchant (Just city)
  pure (merchant.id, cityId)

-- | Permit applicants are not drivers or any other 'Person'. The person id that OTP
-- sending asks for is used only to tag its logs.
mkPermitHost :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow (PermitHost Flow)
mkPermitHost merchantId cityId = do
  smsCfg <- asks (.smsCfg)
  pure
    PermitHost
      { sendOtpSms = \countryCode mobile code ->
          SOTP.sendOTP SOTP.SMS code (Id "permit-applicant") merchantId cityId (Just countryCode) (Just mobile) Nothing Nothing,
        -- The platform's existing test setting: when set, no SMS is sent.
        fakeOtp = show <$> smsCfg.useFakeSms
      }
