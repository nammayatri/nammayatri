# The server's files, one by one — 2026-10-06

Item 2 of phase 0 of the backend restructuring plan: every file under
`/opt/ny/local-stack` on the server, with the first 12 characters of its
sha256, its date (UTC) and its size, compared with git on `algeria/osrm-routing`.
Taken read-only on 2026-10-06; the summary and the way back are in
`box-snapshot-2026-10-04.md`.

Not listed by name, because this repository is public: **25** files that are
certificates, keys, `.env`, the drivers' codes or trusted phones, or that carry
the server's address in their name. They are in the root-only archive
`/root/snapshots/2026-10-04/` on the server. Also left out: the logs, the
backups, `tiles-config/` (map styles and fonts, rebuilt by `tiles-prepare.sh`)
and `2023/` (the OSRM graph, rebuilt by `osrm-prepare.sh`).

`bin/` is gitignored, `MANIFEST.txt` included: the manifest lives on the laptop
and, since 2026-10-05, on the server. **The rider and driver apps do not run
from `bin/`** — see `bin/WHAT-RUNS.txt` and CLAUDE.md.

| | Files |
|---|---:|
| DIFFERS from git | 6 |
| identical to git | 87 |
| on the server only | 7 |
| leftover copies from past edits | 34 |
| bin/ (gitignored) | 6 |
| website build (from the website repo) | 23 |
| tracked in git, not on the server (laptop tools) | 43 |
| not listed (sensitive names) | 25 |

## DIFFERS from git (6)

| File | sha256 | Date | Bytes |
|---|---|---|---:|
| `.gitignore` | `bb3ea9c71c7f` | 2026-08-11 | 848 |
| `README.md` | `180aa4a0c4d0` | 2026-08-23 | 98,509 |
| `docker-compose.yml` | `5bde76da1b70` | 2026-10-03 | 30,632 |
| `geocoder/index.sql` | `f03c246efffe` | 2026-08-10 | 10,655 |
| `probe-two-country-rides.py` | `ab27df20c605` | 2026-09-13 | 11,989 |
| `setup.sh` | `a59f628f7e39` | 2026-08-09 | 36,444 |

## Identical to git (87)

| File | sha256 | Date | Bytes |
|---|---|---|---:|
| `Dockerfile.maps-shim` | `44f81b32325f` | 2026-08-10 | 809 |
| `Dockerfile.rider` | `2fbca1d6b56b` | 2026-08-05 | 3,874 |
| `account-deletion.sql` | `b3e4fef3ec6c` | 2026-08-30 | 3,908 |
| `algeria-geofences.sql` | `9ae24b60841b` | 2026-08-04 | 87,702 |
| `algeria-tariff.sql` | `b49fc903b518` | 2026-09-13 | 5,555 |
| `algerian-test-accounts.sh` | `9a065b672605` | 2026-10-01 | 7,954 |
| `apply-fcm.sh` | `daae5e740fa9` | 2026-08-18 | 5,823 |
| `apply-migration.sh` | `b2b181570c70` | 2026-08-23 | 3,739 |
| `apply-ratings.sh` | `cff2ca0ac55d` | 2026-08-17 | 4,639 |
| `apply-search-window.sh` | `abaa6eb97563` | 2026-08-20 | 5,335 |
| `apply-tariff.sh` | `2fc4e59bcd09` | 2026-09-13 | 3,714 |
| `apply-two-countries.sh` | `3c7277700e65` | 2026-09-13 | 8,196 |
| `auth-guard/server.js` | `eb590c4b5300` | 2026-10-03 | 85,256 |
| `auth-guard/sms-inbox.js` | `3af493def293` | 2026-09-29 | 9,317 |
| `auth-guard/whatsapp.js` | `a8a4e27992fc` | 2026-09-27 | 9,227 |
| `backup.sh` | `e76d1189d560` | 2026-10-04 | 20,915 |
| `dedupe-seed.sql` | `3ba70dc85c92` | 2026-08-09 | 4,055 |
| `demo-map/nginx.conf` | `8ee0e599986a` | 2026-08-04 | 809 |
| `demo-map/site/index.html` | `3b6e8aecb2c3` | 2026-08-05 | 15,988 |
| `demo-map/site/search.html` | `ec4e01b75fe3` | 2026-08-11 | 14,239 |
| `demo.ps1` | `32b88cbd48e9` | 2026-08-04 | 7,026 |
| `demo.sh` | `cdabbd08a9cf` | 2026-08-04 | 2,769 |
| `deploy-backend.sh` | `ea54457be0b9` | 2026-09-14 | 4,246 |
| `deploy-shims.sh` | `6fb8e60a100c` | 2026-10-01 | 2,559 |
| `driver-offer-vehicle.sql` | `f7d3a5c057dc` | 2026-08-23 | 3,530 |
| `driver-subscription-free-month.sql` | `60b3241dd308` | 2026-08-26 | 2,664 |
| `driver-subscription.sql` | `88f7e57453d4` | 2026-08-26 | 7,114 |
| `driver-wallet.sql` | `29a243c70dd6` | 2026-09-07 | 6,828 |
| `drivers-keepalive.sh` | `455a88444954` | 2026-10-01 | 4,406 |
| `edge/nginx.conf` | `79cf45d00a6a` | 2026-10-03 | 39,695 |
| `edge/proxy-common.inc` | `be16ba7023d9` | 2026-08-11 | 757 |
| `enrol-driver.sh` | `6032ebb854a5` | 2026-09-13 | 7,586 |
| `fleet-service.sh` | `372cda1a11d6` | 2026-10-03 | 4,673 |
| `geocoder-prepare.sh` | `66f39d273e35` | 2026-09-03 | 4,362 |
| `geocoder/append-country.sql` | `8d2fa441459c` | 2026-09-13 | 7,778 |
| `geocoder/extract.py` | `46be7f15728b` | 2026-08-10 | 9,437 |
| `geocoder/search.sql` | `37f289982eee` | 2026-08-10 | 8,606 |
| `make-test-driver.sh` | `7f67547b79af` | 2026-08-26 | 6,341 |
| `maps-shim/avatars.js` | `f23f72131137` | 2026-10-03 | 15,600 |
| `maps-shim/deletion.js` | `dad53c62c452` | 2026-08-30 | 7,669 |
| `maps-shim/driver-push.js` | `b026354ed658` | 2026-09-28 | 6,092 |
| `maps-shim/fleet.js` | `af3d8c0a645f` | 2026-09-27 | 7,978 |
| `maps-shim/identity.js` | `efb563aa2277` | 2026-08-27 | 3,419 |
| `maps-shim/number-change.js` | `4bf3a45ae077` | 2026-10-03 | 10,158 |
| `maps-shim/push-relay.js` | `00baa47a8037` | 2026-09-28 | 15,822 |
| `maps-shim/rating.js` | `56d7c57e2173` | 2026-09-27 | 7,736 |
| `maps-shim/restricted.js` | `d5c7dca47e0a` | 2026-09-14 | 12,547 |
| `maps-shim/server.js` | `722c0c6b3d8f` | 2026-10-03 | 39,890 |
| `maps-shim/subscription.js` | `b6f34fa725b1` | 2026-08-26 | 29,308 |
| `maps-shim/wallet.js` | `2148d0d071ef` | 2026-09-28 | 31,714 |
| `maps-two-countries.sh` | `913800c51dae` | 2026-09-13 | 8,277 |
| `mauritania-geofences.sql` | `815ea2d9c53f` | 2026-09-03 | 4,763 |
| `mauritania-tariff.sql` | `a316ee939bc2` | 2026-09-13 | 6,632 |
| `movin-bot.py` | `ec1317961862` | 2026-09-29 | 51,358 |
| `osrm-config.sql` | `f7b51204000a` | 2026-08-09 | 4,699 |
| `osrm-prepare.sh` | `d918d1c58d9b` | 2026-09-03 | 4,349 |
| `probe-account-deletion.py` | `c0cbed9acf2a` | 2026-08-30 | 6,038 |
| `probe-booking-flow.py` | `fe9646354de2` | 2026-08-16 | 15,830 |
| `probe-booking-timeouts.py` | `c298de3c72b6` | 2026-08-16 | 4,748 |
| `probe-cost-per-request.py` | `c182b6ffa1ef` | 2026-08-27 | 10,384 |
| `probe-dispatch-restriction.py` | `18fbcfb1f931` | 2026-08-26 | 9,384 |
| `probe-driver-offers.sql` | `352991df3b16` | 2026-08-20 | 2,938 |
| `probe-driver-pickup.sql` | `491497f4eceb` | 2026-08-20 | 4,221 |
| `probe-driver-wait.sql` | `b50b24d7e98d` | 2026-08-20 | 9,211 |
| `probe-fleet-nearby.py` | `fec01482c25f` | 2026-08-23 | 3,455 |
| `probe-load-followups.py` | `48025c1980e7` | 2026-08-27 | 8,453 |
| `probe-push.py` | `82fbf4b0f431` | 2026-08-18 | 5,887 |
| `probe-restricted-drivers.py` | `c2dce138c73d` | 2026-08-26 | 7,485 |
| `probe-rider-extras.py` | `d04394868cb5` | 2026-08-16 | 3,827 |
| `probe-search-window.py` | `a890b00a1e3c` | 2026-08-20 | 6,907 |
| `probe-service-time.py` | `e37689d9e611` | 2026-08-27 | 12,347 |
| `probe-shortlist.py` | `5141ceb8943d` | 2026-08-23 | 6,319 |
| `probe-storage-cost.py` | `504cf6e0b8a0` | 2026-08-27 | 6,262 |
| `probe-subscription-flow.py` | `3ed7886c26e3` | 2026-08-26 | 17,211 |
| `probe-subscription-live.py` | `3b1f4916df9c` | 2026-08-26 | 6,633 |
| `probe-subscription.sql` | `0e000c527186` | 2026-08-16 | 3,037 |
| `probe-trip-history.py` | `d438adc3deae` | 2026-08-17 | 7,723 |
| `probe-unused-routes.py` | `625b42e0a6aa` | 2026-08-17 | 4,386 |
| `probe-wallet-screens.py` | `be746fc31303` | 2026-09-07 | 10,172 |
| `ratings-average.sql` | `63609a94fd05` | 2026-08-17 | 4,658 |
| `search-request-chosen-drivers.sql` | `a6a6ba6efbff` | 2026-08-23 | 2,741 |
| `seed-mauritanian-fleet.sh` | `879a970c501e` | 2026-10-03 | 11,353 |
| `simulate-driver.py` | `8fe7bd8d80a7` | 2026-10-04 | 41,075 |
| `switch-domain.sh` | `2a7dcd28236d` | 2026-08-26 | 8,453 |
| `tiles-arabic.sh` | `40796b012573` | 2026-09-14 | 8,306 |
| `tiles-prepare.sh` | `f1333cb5e668` | 2026-09-03 | 4,416 |
| `two-countries-merchants.sql` | `1b5b4ee67ab3` | 2026-09-13 | 7,742 |

## On the server only (7)

| File | sha256 | Date | Bytes |
|---|---|---|---:|
| `demo-map/site/areas.geojson` | `0a5fe7e47b74` | 2026-08-07 | 34,468 |
| `geocoder/places.algeria.csv` | `b34f4652f894` | 2026-08-10 | 20,633,751 |
| `geocoder/places.csv` | `dd885d3c4db9` | 2026-09-03 | 1,629,947 |
| `geocoder/places.mauritania.csv` | `dd885d3c4db9` | 2026-09-03 | 1,629,947 |
| `movin-bot.state.json` | `d9916b8416cc` | 2026-10-06 | 3,371 |
| `server-state/.apt-check.json` | `ad729d32a5a5` | 2026-10-06 | 55 |
| `server-state/state.json` | `a701cdff84ac` | 2026-10-06 | 7,342 |

## Leftover copies from past edits (34)

| File | sha256 | Date | Bytes |
|---|---|---|---:|
| `auth-guard/server.js.bak-20260923-123139` | `7d1f6a27d377` | 2026-09-23 | 50,668 |
| `auth-guard/server.js.before-signup-20260917T164124Z` | `8db72537f82f` | 2026-09-14 | 48,979 |
| `auth-guard/server.js.before-sms` | `4ff1e7fcc54d` | 2026-09-06 | 26,396 |
| `docker-compose.yml.bak-20260831-123757` | `039b1ebb39fc` | 2026-08-26 | 25,688 |
| `docker-compose.yml.bak-20260831-125755` | `e5d3bb4e2243` | 2026-08-31 | 27,686 |
| `docker-compose.yml.bak-20260831-164131` | `2f22d4fa5695` | 2026-08-31 | 27,726 |
| `docker-compose.yml.bak-20260831-171809` | `f1203c69d7bf` | 2026-08-31 | 28,341 |
| `docker-compose.yml.bak-20260831-172801` | `f1203c69d7bf` | 2026-08-31 | 28,341 |
| `docker-compose.yml.bak-20260903-124949` | `745429afcae3` | 2026-09-03 | 25,720 |
| `docker-compose.yml.bak-20260903-125723` | `7bcfd878dafd` | 2026-09-03 | 29,073 |
| `docker-compose.yml.bak-20260906-122045` | `c5ec7be70308` | 2026-09-06 | 28,618 |
| `docker-compose.yml.bak-20260906-122053` | `e032461cf1f9` | 2026-09-06 | 31,971 |
| `docker-compose.yml.bak-20260910-115336` | `6351a7fa351e` | 2026-09-06 | 32,322 |
| `docker-compose.yml.bak-20260923-182941` | `4448509fddcd` | 2026-09-17 | 32,826 |
| `docker-compose.yml.before-dz-open` | `9cadd8848abb` | 2026-09-27 | 28,925 |
| `docker-compose.yml.before-numchg-20261003T123610Z` | `049b693408bf` | 2026-10-03 | 30,273 |
| `docker-compose.yml.before-sms` | `c098226e64b2` | 2026-09-06 | 29,113 |
| `docker-compose.yml.before-test-accounts` | `6315f2aed412` | 2026-09-27 | 29,158 |
| `docker-compose.yml.before-trusted-20261003T121859Z` | `57cd6b1808c3` | 2026-09-29 | 29,815 |
| `docker-compose.yml.before-whatsapp` | `51cfd434b425` | 2026-09-27 | 29,091 |
| `edge/nginx.conf.bak` | `36b3a5b3bb09` | 2026-09-10 | 29,762 |
| `edge/nginx.conf.bak-20260831-124544` | `cd6ca23960d4` | 2026-08-30 | 21,608 |
| `edge/nginx.conf.bak-20260831-125754` | `19a6e7b47b73` | 2026-08-31 | 22,600 |
| `edge/nginx.conf.bak-20260831-132547` | `c944bbcf4176` | 2026-08-31 | 24,066 |
| `edge/nginx.conf.bak-20260831-172805` | `823b2f8eb838` | 2026-08-31 | 25,630 |
| `edge/nginx.conf.bak-20260831-222750` | `fed76d14cdeb` | 2026-08-31 | 26,229 |
| `edge/nginx.conf.bak-20260901-155341` | `4ea52247ec50` | 2026-08-31 | 26,709 |
| `edge/nginx.conf.bak-20260907-113944` | `4cc6f59d6141` | 2026-09-01 | 27,550 |
| `edge/nginx.conf.bak-20260909-113651` | `36736e953fc5` | 2026-09-07 | 29,351 |
| `edge/nginx.conf.bak-20260914-161225` | `5284bbd2c721` | 2026-09-10 | 30,787 |
| `edge/nginx.conf.bak-20260923-123123` | `223c1e8af512` | 2026-09-23 | 32,413 |
| `edge/nginx.conf.before-api.movinapp.net` | `a34c0f47034a` | 2026-08-26 | 14,704 |
| `edge/nginx.conf.before-three-names-20260830` | `db4d29f6143b` | 2026-08-30 | 16,291 |
| `edge/nginx.conf.broken-130439` | `973f52da471b` | 2026-09-23 | 21,416 |

## Bin/ (gitignored) (6)

| File | sha256 | Date | Bytes |
|---|---|---|---:|
| `bin/MANIFEST.txt` | `06ccf4acef31` | 2026-10-05 | 1,528 |
| `bin/WHAT-RUNS.txt` | `8f8c6743f720` | 2026-10-05 | 552 |
| `bin/beckn-gateway-exe` | `c20525251fa4` | 2026-08-05 | 67,387,904 |
| `bin/dynamic-offer-driver-app-exe` | `246eafdae91d` | 2026-08-05 | 102,583,744 |
| `bin/mock-registry-exe` | `f9a3f3d8c515` | 2026-08-05 | 68,472,640 |
| `bin/rider-app-exe` | `17536b26c759` | 2026-08-05 | 92,374,080 |

## Website build (from the website repo) (23)

| File | sha256 | Date | Bytes |
|---|---|---|---:|
| `edge-web/admin/assets/index-Bo1SNTLV.css` | `93a48644346b` | 2026-10-02 | 28,667 |
| `edge-web/admin/assets/index-C3PQEBde.js` | `3ff451a427c4` | 2026-10-02 | 498,783 |
| `edge-web/admin/index.html` | `9494e1f2a4fa` | 2026-10-02 | 1,117 |
| `edge-web/site/404.html` | `fc49ee90d658` | 2026-10-02 | 9,625 |
| `edge-web/site/apple-touch-icon.png` | `9c67ec6013dc` | 2026-10-02 | 3,973 |
| `edge-web/site/assets/404-B-cqv4pV.css` | `d7f4f19f1589` | 2026-10-02 | 1,957 |
| `edge-web/site/assets/404-Ck7KJgBM.js` | `48a603bb40ed` | 2026-10-02 | 317 |
| `edge-web/site/assets/index-1-XEbNOc.css` | `3bd9b5607189` | 2026-10-02 | 22,981 |
| `edge-web/site/assets/index-CiZ8rA85.js` | `f8e306ec4c55` | 2026-10-02 | 16,271 |
| `edge-web/site/assets/legal-DCAcy748.css` | `18d4a977d6fb` | 2026-10-02 | 13,625 |
| `edge-web/site/assets/legal-DKMJWZ-q.js` | `dfee62087cb8` | 2026-10-02 | 1,787 |
| `edge-web/site/assets/mail-Be-XlNIA.js` | `8919c2ff3a5d` | 2026-10-02 | 279 |
| `edge-web/site/assets/theme-BPaswmz6.css` | `3bc7003343cb` | 2026-10-02 | 969 |
| `edge-web/site/assets/theme-DMEjGR8q.js` | `cdc77876f4cd` | 2026-10-02 | 50,579 |
| `edge-web/site/conditions.html` | `7b97b5150619` | 2026-10-02 | 23,800 |
| `edge-web/site/confidentialite.html` | `10b7707d32cf` | 2026-10-02 | 29,102 |
| `edge-web/site/cookies.html` | `57dcdbecdcf8` | 2026-10-02 | 18,290 |
| `edge-web/site/favicon.ico` | `852eb2daa557` | 2026-10-02 | 3,162 |
| `edge-web/site/icon.svg` | `c28ddc36ca18` | 2026-10-02 | 967 |
| `edge-web/site/index.html` | `95e3cf0f3095` | 2026-10-02 | 42,301 |
| `edge-web/site/og.png` | `5521147df7fd` | 2026-10-02 | 53,572 |
| `edge-web/site/robots.txt` | `74be8eb9b4bb` | 2026-10-02 | 285 |
| `edge-web/site/sitemap.xml` | `d7bd77ed95e1` | 2026-10-02 | 2,254 |

## Tracked in git, not on the server (43)

Laptop tools and probes, run from the laptop or copied to `/tmp` when needed.

- `cancellation-reasons.sql`
- `docs/box-snapshot-2026-10-04.md`
- `edge/add-place-labels-location.py`
- `geocoder/arabic-ambiguous-ids.sql`
- `geocoder/arabic-ambiguous.sql`
- `geocoder/arabic-composed.sql`
- `geocoder/arabic-gap.sql`
- `geocoder/arabic-jedrel-space.sql`
- `geocoder/arabic-known.sql`
- `geocoder/arabic-mixed.sql`
- `geocoder/arabic-names.sql`
- `geocoder/arabic-reviewed-streets.sql`
- `geocoder/arabic-reviewed.sql`
- `geocoder/arabic-sample.sql`
- `geocoder/arabic-search.sql`
- `geocoder/arabic-state.sql`
- `geocoder/arabic-todo.sql`
- `install-moosyl-key.sh`
- `passenger-rating.sql`
- `probe-agency-messages.py`
- `probe-arabic-labels.py`
- `probe-driver-rides.py`
- `probe-load.py`
- `probe-multipart.py`
- `probe-nearby-fleet.sql`
- `probe-place-language.py`
- `probe-plate-years.py`
- `probe-rating-routes.py`
- `probe-ride-report.py`
- `probe-rider-rating.py`
- `probe-vehicle-tags.sql`
- `tests/auth-guard-limits.test.js`
- `tests/auth-guard-number-change.test.js`
- `tests/auth-guard-signup.test.js`
- `tests/auth-guard-sms-in.test.js`
- `tests/auth-guard-sms-inbox.test.js`
- `tests/auth-guard-trusted.test.js`
- `tests/auth-guard-whatsapp.test.js`
- `tests/driver-push.test.js`
- `tests/movin-bot.test.py`
- `tests/number-change.test.js`
- `tests/push-relay.test.js`
- `tests/wallet-dispatch.test.js`
