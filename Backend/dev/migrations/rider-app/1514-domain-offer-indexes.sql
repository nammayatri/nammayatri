-- Indexes and unique constraints for domain offer tables

-- Offer: findAllActiveByMerchant
CREATE INDEX IF NOT EXISTS idx_offer_merchant_city_active ON atlas_app.offer (merchant_id, merchant_operating_city_id, is_active);
ALTER TABLE atlas_app.offer ADD CONSTRAINT unique_offer_code UNIQUE (offer_code);

-- PersonOfferStats: one stats row per (offer, person)
CREATE INDEX IF NOT EXISTS idx_person_offer_stats_person ON atlas_app.person_offer_stats (person_id);
ALTER TABLE atlas_app.person_offer_stats ADD CONSTRAINT unique_person_offer_stats_offer_person UNIQUE (offer_id, person_id);

-- PersonDailyOfferStats: one stats row per (person, date)
CREATE INDEX IF NOT EXISTS idx_person_daily_offer_stats_date ON atlas_app.person_daily_offer_stats (date);
CREATE INDEX IF NOT EXISTS idx_person_daily_offer_stats_date_payout ON atlas_app.person_daily_offer_stats (date, payout_status);
ALTER TABLE atlas_app.person_daily_offer_stats ADD CONSTRAINT unique_person_daily_offer_stats_person_date UNIQUE (person_id, date);
