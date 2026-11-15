-- Efface les lieux D'ESSAI de « Lieux proposés » : ceux du jour 4 (essai sur
-- téléphone) et ceux du jour 6 (la relecture de sécurité, puis l'essai de fin
-- de journée), avec leurs photos et l'historique du bureau. Rien d'autre.
--
-- Choisis par leur NOM TAPÉ (proposed_name), qui doit commencer par
-- « Test jour 4 » ou « Test jour 6 » : le nom donné exprès aux lieux d'essai.
-- Un vrai lieu ne s'appelle pas ainsi. Le nom corrigé par le bureau ne compte
-- pas : c'est ce que la personne a tapé qui fait foi.
--
-- DEUX FOIS, le même fichier, depuis Backend/dev/local-stack :
--   1. à blanc (par défaut) : montre ce qui serait effacé, efface, puis
--      ANNULE tout (ROLLBACK). Rien ne change.
--        ssh ny "docker exec -i ny-postgres psql -v ON_ERROR_STOP=1 -U postgres -d atlas_dev" \
--          < investigations/erase-test-places.sql | tee ~/day6-erase-dry.log
--   2. pour de bon, après avoir lu le 1 : le même, avec -v go=yes
--        ssh ny "docker exec -i ny-postgres psql -v ON_ERROR_STOP=1 -v go=yes -U postgres -d atlas_dev" \
--          < investigations/erase-test-places.sql | tee ~/day6-erase.log
--
-- Il s'arrête et n'efface RIEN si plus de 25 lieux portent ce nom (le signe
-- que la règle attrape autre chose que nos essais).
--
-- Restent, exprès : les lignes du journal d'audit (place.approve, place.refuse,
-- place.edit, place.photo_view). Le journal ne s'efface jamais.

\set QUIET on
\pset footer off
\echo '=== les lieux d''essai trouvés (nom tapé « Test jour 4… » ou « Test jour 6… »)'
SELECT sp.place_id, sp.proposed_name, sp.status, sp.country, sp.author_side,
       (SELECT count(*) FROM movin.suggested_place_photo ph WHERE ph.suggested_place_id = sp.id) AS photos,
       (SELECT count(*) FROM movin.suggested_place_decision d WHERE d.place_id = sp.id) AS historique
  FROM movin.suggested_place sp
 WHERE sp.proposed_name LIKE 'Test jour 4%' OR sp.proposed_name LIKE 'Test jour 6%'
 ORDER BY sp.id;

\echo '=== tous les autres lieux proposés restent : leur nombre, avant'
SELECT count(*) AS autres_avant FROM movin.suggested_place
 WHERE NOT (proposed_name LIKE 'Test jour 4%' OR proposed_name LIKE 'Test jour 6%');

BEGIN;
CREATE TEMP TABLE _target ON COMMIT DROP AS
  SELECT sp.id FROM movin.suggested_place sp
   WHERE sp.proposed_name LIKE 'Test jour 4%' OR sp.proposed_name LIKE 'Test jour 6%';

DO $$
DECLARE n_target int; n_photo int; n_hist int; n_place int;
BEGIN
  SELECT count(*) INTO n_target FROM _target;
  IF n_target > 25 THEN
    RAISE EXCEPTION '% lieux portent un nom d''essai : plus de 25, rien n''est effacé', n_target;
  END IF;
  DELETE FROM movin.suggested_place_photo WHERE suggested_place_id IN (SELECT id FROM _target);
  GET DIAGNOSTICS n_photo = ROW_COUNT;
  DELETE FROM movin.suggested_place_decision WHERE place_id IN (SELECT id FROM _target);
  GET DIAGNOSTICS n_hist = ROW_COUNT;
  DELETE FROM movin.suggested_place WHERE id IN (SELECT id FROM _target);
  GET DIAGNOSTICS n_place = ROW_COUNT;
  RAISE NOTICE 'effacé : % lieu(x), % photo(s), % ligne(s) d''historique', n_place, n_photo, n_hist;
END $$;

\echo '=== dans la transaction : lieux d''essai restants (doit être 0) et autres lieux (doit être le même nombre)'
SELECT (SELECT count(*) FROM movin.suggested_place
         WHERE proposed_name LIKE 'Test jour 4%' OR proposed_name LIKE 'Test jour 6%') AS essais_restants,
       (SELECT count(*) FROM movin.suggested_place
         WHERE NOT (proposed_name LIKE 'Test jour 4%' OR proposed_name LIKE 'Test jour 6%')) AS autres_apres;

\if :{?go}
  COMMIT;
  \echo '=== ENREGISTRÉ (go=yes) : les lieux d''essai sont effacés'
\else
  ROLLBACK;
  \echo '=== À BLANC : tout est annulé, rien n''a changé. Relancer avec -v go=yes pour effacer.'
\endif
