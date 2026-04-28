ALTER TABLE atlas_app.issue_category ADD COLUMN show_in_default BOOLEAN;

UPDATE atlas_app.issue_category
SET show_in_default = TRUE
WHERE id IN (
  '140f1a9b-9b5a-490a-be2f-f94d2a66bed3',
  'cdfa32e0-fd2d-436f-8d92-b92bf1b6d3c0',
  '1bb14f3c-95bd-4421-bfd4-e9c9b0689b29',
  '227b8d4c-16af-4b62-b52c-cae0e6eb58a7',
  '9dda7689-155e-40ca-ac25-43a676aacf04'
);