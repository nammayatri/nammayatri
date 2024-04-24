BEGIN;  --should get executed in one transaction
UPDATE atlas_app.exophone SET call_service = 'Knowlarity' where primary_phone = '8035471715';
COMMIT;
