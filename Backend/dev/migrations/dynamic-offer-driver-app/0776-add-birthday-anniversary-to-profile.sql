ALTER TABLE atlas_driver_offer_bpp.driver_profile_questions
    ADD COLUMN birthday DATE,
    ADD COLUMN anniversary DATE;

CREATE INDEX idx_driver_profile_birthday_month_day
    ON atlas_driver_offer_bpp.driver_profile_questions(
        EXTRACT(MONTH FROM birthday),
        EXTRACT(DAY FROM birthday)
    )
    WHERE birthday IS NOT NULL;

CREATE INDEX idx_driver_profile_anniversary_month_day
    ON atlas_driver_offer_bpp.driver_profile_questions(
        EXTRACT(MONTH FROM anniversary),
        EXTRACT(DAY FROM anniversary)
    )
    WHERE anniversary IS NOT NULL;
