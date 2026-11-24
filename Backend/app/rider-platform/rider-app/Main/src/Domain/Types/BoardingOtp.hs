-- | A shuttle ticket's boarding OTP. Its Show is redacted: whole booking records are logged in several places
-- (`show booking`), and the code is a boarding credential that must not reach the logs.
module Domain.Types.BoardingOtp
  ( BoardingOtp (..),
  )
where

import Kernel.Prelude
import qualified Prelude as P

newtype BoardingOtp = BoardingOtp {getBoardingOtp :: Text}
  deriving (Eq, Generic)

instance Show BoardingOtp where
  showsPrec _ _ = P.showString "BoardingOtp <redacted>"
