module Domain.Action.UI.Faq (getFaq) where

import qualified API.Types.UI.Faq
import qualified Domain.Types.Faq
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Types (Language (..))
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Faq as QFaq
import qualified Storage.Queries.Person as QPerson
import Tools.Error

getFaq ::
  ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
    Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
  ) ->
  Kernel.Prelude.Maybe Kernel.Prelude.Text ->
  Environment.Flow [API.Types.UI.Faq.FaqAPIEntity]
getFaq (mbPersonId, _merchantId, merchantOpCityId) mbCategory = do
  personId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  let language = fromMaybe ENGLISH person.language
  faqs <- QFaq.findAllByMerchantOperatingCityIdAndLanguage merchantOpCityId language
  -- Fall back to English FAQs when none exist in the driver's language.
  faqsInLanguage <-
    if null faqs && language /= ENGLISH
      then QFaq.findAllByMerchantOperatingCityIdAndLanguage merchantOpCityId ENGLISH
      else pure faqs
  let filteredFaqs = maybe faqsInLanguage (\category -> filter (\faq -> faq.category == Just category) faqsInLanguage) mbCategory
  pure $ map toFaqAPIEntity (sortOn (.createdAt) filteredFaqs)

toFaqAPIEntity :: Domain.Types.Faq.Faq -> API.Types.UI.Faq.FaqAPIEntity
toFaqAPIEntity faq =
  API.Types.UI.Faq.FaqAPIEntity
    { faqGroupId = faq.faqGroupId,
      category = faq.category,
      question = faq.question,
      answer = faq.answer
    }
