module Storage.Queries.FullEntityBuilders where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Domain.Types.Quote as Quote
import qualified Storage.Queries.Quote.QuoteTerms as QQuoteTerms
import qualified Storage.Queries.Quote.RentalQuote as QRentalQuote
import Storage.Tabular.Quote as Quote
import Storage.Tabular.Quote.RentalQuote (RentalQuoteT (..))

buildFullQuote :: Transactionable m => QuoteT -> DTypeBuilder m (Maybe (SolidType FullQuoteT))
buildFullQuote quoteT@QuoteT {..} = runMaybeT $ do
  quoteTermsT <- lift . QQuoteTerms.findAllByQuoteId' $ Id id
  quoteDetails <- case distanceToNearestDriver of
    Nothing -> do
      rentalQuoteT@RentalQuoteT {..} <- MaybeT $ QRentalQuote.findByQuoteId' (Id id)
      return $ Quote.RentalDetailsT rentalQuoteT
    Just _ -> return Quote.OneWayDetailsT
  return $ extractSolidType @Quote (quoteT, quoteTermsT, quoteDetails)
