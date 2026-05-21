-- Seed invoice_template rows for Lynx (Helsinki) on both BAP + BPP schemas.
-- These are the HTML bodies for the config-driven invoice PDF pipeline; the
-- companion JL rules (which produce the variable bindings consumed here) are
-- seeded separately via control-center's DynamicLogicPage post-deploy, not SQL.
-- Consumer: SharedLogic.RenderInvoiceFromTemplate.renderHtml (both apps).
--
-- BPP merchant_id (BRIDGE_FINLAND_PARTNER):  a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e
-- BPP merchant_operating_city_id (Helsinki): beabba6a-c817-43d2-93b2-a916f5cf2ceb
-- BAP merchant_id (BRIDGE_FINLAND):          b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f
-- BAP merchant_operating_city_id (Helsinki): f9903ef6-f595-428e-b5ac-e8816cbdf979
--
-- Idempotent: stable per-row UUIDs + ON CONFLICT (id) DO UPDATE. The natural
-- key (mocId, invoice_type, language) can't be used because invoice_type is
-- NULL for Generic rows, so we conflict on the PK instead.

------------------------------------------------------------
-- UNIQUE INDEX (NammaDSL extraIndexes doesn't emit SQL UNIQUE per spec gap)
------------------------------------------------------------
CREATE UNIQUE INDEX IF NOT EXISTS uniq_invoice_template_mocid_type_lang
  ON atlas_driver_offer_bpp.invoice_template
     (merchant_operating_city_id, invoice_type, language);

CREATE UNIQUE INDEX IF NOT EXISTS uniq_invoice_template_mocid_type_lang
  ON atlas_app.invoice_template
     (merchant_operating_city_id, invoice_type, language);

------------------------------------------------------------
-- BPP: Generic / ENGLISH
------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.invoice_template (
  id, merchant_id, merchant_operating_city_id, invoice_type, language,
  template, line_item_row_template, totals_line_row_template,
  created_at, updated_at
) VALUES (
  'bb000001-1234-4000-8000-000000000001',
  'a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e',
  'beabba6a-c817-43d2-93b2-a916f5cf2ceb',
  NULL,
  'ENGLISH',
  $template$<!DOCTYPE html><html><head>
<meta charset='UTF-8'>
<title>Invoice {#invoiceNumber#}</title>
<style>
* { margin:0; padding:0; box-sizing:border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif;
       color:#2d2d2d; background:#fff; padding:32px;
       -webkit-print-color-adjust:exact !important; print-color-adjust:exact !important; }
.wrap { max-width:820px; margin:0 auto; }
.header { width:100%; border-collapse:collapse; border:none; margin-bottom:28px; }
.header td { vertical-align:top; border:none; }
.header td.header-right-cell { text-align:right; border:none; }
.logo-wrap { display:block; }
.logo { height:44px; width:auto; margin-bottom:10px; display:block; }
.inv-number { font-size:15px; font-weight:700; color:#2d2d2d; margin-bottom:10px; }
.inv-date { font-size:13px; color:#2d2d2d; margin-top:2px; }
.parties { width:100%; border-collapse:collapse; border:none; margin-bottom:24px; padding:16px 0; }
.party-right { text-align:right; vertical-align:top; border:none; }
.party-left { vertical-align:top; width:50%; border:none; }
.party-lbl-plain { font-size:13px; color:#2d2d2d; margin-bottom:4px; }
.party-name { font-size:15px; font-weight:600; color:#2d2d2d; margin-bottom:3px; }
.party-addr { font-size:13px; color:#2d2d2d; line-height:1.6; }
.party-tax { font-size:12px; color:#2d2d2d; margin-top:3px; }
.from-location { font-size:13px; color:#2d2d2d; margin-bottom:20px; }
table.line-items { width:100%; border-collapse:collapse; margin-bottom:24px; border:2px solid #2d2d2d; }
table.line-items th { font-size:13px; font-weight:700; color:#2d2d2d; text-transform:none; letter-spacing:0;
     border:1px solid #2d2d2d; padding:8px 10px; text-align:left; background:#fff; }
table.line-items td { font-size:13px; padding:9px 10px; border:1px solid #2d2d2d; color:#2d2d2d; }
table.line-items th.n, table.line-items td.n { text-align:right; }
.totals { width:400px; margin-left:auto; padding-top:24px; border-collapse:collapse; border:none; }
.tot-row { width:100%; }
.tot-row td { font-size:13px; color:#2d2d2d; padding:3px 0; border:none; }
.tot-row td.tot-val { text-align:right; padding-left:24px; white-space:nowrap; border:none; }
.tot-row.grand td { font-size:15px; font-weight:700; padding-top:12px; border:none; }
.tot-row.payment td { font-size:12px; padding-top:4px; border:none; }
.payment-detail { font-style:italic; }
.hidden { display: none !important; }
@media print { body { padding:16px; } }
</style>
</head><body><div class='wrap'>

<table class='header'>
  <tr>
    <td>
      <div class="logo-wrap {#logoClass#}">
        <img src="{#logoUrl#}" alt="" class="logo">
      </div>
    </td>
    <td class='header-right-cell'>
      <div class='inv-number'>Invoice No. {#invoiceNumber#}</div>
      <div class='inv-date'>Date: {#issuedAt#}</div>
      <div class="inv-date {#dueDateRowClass#}">Due Date: {#dueAt#}</div>
    </td>
  </tr>
</table>

<table class="parties {#partiesTableClass#}">
  <tr>
    <td class="party-left {#leftPartyClass#}">
      <div class='party-lbl-plain'>Recipient:</div>
      <div class="party-name {#recipientNameClass#}" style="font-weight:700;color:#000000">{#recipientName#}</div>
      <div class="party-addr">{#recipientAddress#}</div>
    </td>
    <td class="party-right {#rightPartyClass#}">
      <div class="party-name {#rightNameClass#}">{#rightName#}</div>
      <div class="party-addr {#rightAddressClass#}">{#rightAddress#}</div>
      <div class="party-tax {#rightGstinClass#}">GSTIN {#rightGstin#}</div>
      <div class="party-tax {#rightVatNumberClass#}">VAT No. {#rightVatNumber#}</div>
    </td>
  </tr>
</table>

<table class='line-items'>
  <thead>
    <tr>
      <th>Title</th>
      <th class="n">Amount ({#currencyCode#})</th>
      <th class="n">VAT (%)</th>
      <th class="n">VAT ({#currencyCode#})</th>
      <th class="n">Total ({#currencyCode#})</th>
    </tr>
  </thead>
  <tbody>{#mainTableItemsHtml#}</tbody>
</table>

<table class="totals">
  <tr class="tot-row">
    <td>{#taxableLabel#}</td>
    <td class="tot-val">{#taxableSum#}</td>
  </tr>
  <tr class="tot-row {#vatRowClass#}">
    <td>VAT<span class="{#vatPctSuffixClass#}"> ({#vatPct#}%)</span></td>
    <td class="tot-val">{#taxSum#}</td>
  </tr>

  {#externalItemsHtml#}

  <tr class="tot-row grand">
    <td style="font-weight:700;color:#000000">Total incl. VAT ({#currencyCode#})</td>
    <td class="tot-val" style="font-weight:700;color:#000000">{#totalIncVat#}</td>
  </tr>

  {#adjustmentItemsHtml#}

  <tr class="tot-row grand {#invoicedValueRowClass#}">
    <td style="font-weight:700;color:#000000">Invoiced Value</td>
    <td class="tot-val" style="font-weight:700;color:#000000">{#invoicedValue#}</td>
  </tr>

  <tr class="tot-row payment {#paymentMethodRowClass#}">
    <td></td>
    <td class="tot-val payment-detail">
      <span class="{#paymentByCardClass#}">Received via {#cardBrand#} ****{#cardLastFour#}</span>
      <span class="{#paymentOnlineClass#}">Received online</span>
      <span class="{#paymentCashClass#}">Received in cash</span>
      <span class="{#paymentOtherClass#}">{#paymentMode#}</span>
    </td>
  </tr>
</table>

</div></body></html>$template$,
  $row$<tr><td>{#title#}<span class="{#postDiscountClass#}"> (Post Discount {#postDiscountCurrency#} {#postDiscountAmount#})</span></td><td class="n">{#amount#}</td><td class="n">{#vatPct#}</td><td class="n">{#vatAmount#}</td><td class="n">{#total#}</td></tr>$row$,
  $totals$<tr class="tot-row"><td>{#title#}</td><td class="tot-val">{#amount#}</td></tr>$totals$,
  now(), now()
) ON CONFLICT (id) DO UPDATE SET
  template = EXCLUDED.template,
  line_item_row_template = EXCLUDED.line_item_row_template,
  totals_line_row_template = EXCLUDED.totals_line_row_template,
  merchant_id = EXCLUDED.merchant_id,
  merchant_operating_city_id = EXCLUDED.merchant_operating_city_id,
  invoice_type = EXCLUDED.invoice_type,
  language = EXCLUDED.language,
  updated_at = now();

------------------------------------------------------------
-- BPP: Generic / FINNISH
------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.invoice_template (
  id, merchant_id, merchant_operating_city_id, invoice_type, language,
  template, line_item_row_template, totals_line_row_template,
  created_at, updated_at
) VALUES (
  'bb000002-1234-4000-8000-000000000002',
  'a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e',
  'beabba6a-c817-43d2-93b2-a916f5cf2ceb',
  NULL,
  'FINNISH',
  $template$<!DOCTYPE html><html><head>
<meta charset='UTF-8'>
<title>Lasku {#invoiceNumber#}</title>
<style>
* { margin:0; padding:0; box-sizing:border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif;
       color:#2d2d2d; background:#fff; padding:32px;
       -webkit-print-color-adjust:exact !important; print-color-adjust:exact !important; }
.wrap { max-width:820px; margin:0 auto; }
.header { width:100%; border-collapse:collapse; border:none; margin-bottom:28px; }
.header td { vertical-align:top; border:none; }
.header td.header-right-cell { text-align:right; border:none; }
.logo-wrap { display:block; }
.logo { height:44px; width:auto; margin-bottom:10px; display:block; }
.inv-number { font-size:15px; font-weight:700; color:#2d2d2d; margin-bottom:10px; }
.inv-date { font-size:13px; color:#2d2d2d; margin-top:2px; }
.parties { width:100%; border-collapse:collapse; border:none; margin-bottom:24px; padding:16px 0; }
.party-right { text-align:right; vertical-align:top; border:none; }
.party-left { vertical-align:top; width:50%; border:none; }
.party-lbl-plain { font-size:13px; color:#2d2d2d; margin-bottom:4px; }
.party-name { font-size:15px; font-weight:600; color:#2d2d2d; margin-bottom:3px; }
.party-addr { font-size:13px; color:#2d2d2d; line-height:1.6; }
.party-tax { font-size:12px; color:#2d2d2d; margin-top:3px; }
.from-location { font-size:13px; color:#2d2d2d; margin-bottom:20px; }
table.line-items { width:100%; border-collapse:collapse; margin-bottom:24px; border:2px solid #2d2d2d; }
table.line-items th { font-size:13px; font-weight:700; color:#2d2d2d; text-transform:none; letter-spacing:0;
     border:1px solid #2d2d2d; padding:8px 10px; text-align:left; background:#fff; }
table.line-items td { font-size:13px; padding:9px 10px; border:1px solid #2d2d2d; color:#2d2d2d; }
table.line-items th.n, table.line-items td.n { text-align:right; }
.totals { width:400px; margin-left:auto; padding-top:24px; border-collapse:collapse; border:none; }
.tot-row { width:100%; }
.tot-row td { font-size:13px; color:#2d2d2d; padding:3px 0; border:none; }
.tot-row td.tot-val { text-align:right; padding-left:24px; white-space:nowrap; border:none; }
.tot-row.grand td { font-size:15px; font-weight:700; padding-top:12px; border:none; }
.tot-row.payment td { font-size:12px; padding-top:4px; border:none; }
.payment-detail { font-style:italic; }
.hidden { display: none !important; }
@media print { body { padding:16px; } }
</style>
</head><body><div class='wrap'>

<table class='header'>
  <tr>
    <td>
      <div class="logo-wrap {#logoClass#}">
        <img src="{#logoUrl#}" alt="" class="logo">
      </div>
    </td>
    <td class='header-right-cell'>
      <div class='inv-number'>Laskun nro. {#invoiceNumber#}</div>
      <div class='inv-date'>Päivämäärä: {#issuedAt#}</div>
      <div class="inv-date {#dueDateRowClass#}">Eräpäivä: {#dueAt#}</div>
    </td>
  </tr>
</table>

<table class="parties {#partiesTableClass#}">
  <tr>
    <td class="party-left {#leftPartyClass#}">
      <div class='party-lbl-plain'>Vastaanottaja:</div>
      <div class="party-name {#recipientNameClass#}" style="font-weight:700;color:#000000">{#recipientName#}</div>
      <div class="party-addr">{#recipientAddress#}</div>
    </td>
    <td class="party-right {#rightPartyClass#}">
      <div class="party-name {#rightNameClass#}">{#rightName#}</div>
      <div class="party-addr {#rightAddressClass#}">{#rightAddress#}</div>
      <div class="party-tax {#rightGstinClass#}">GSTIN {#rightGstin#}</div>
      <div class="party-tax {#rightVatNumberClass#}">ALV-nro. {#rightVatNumber#}</div>
    </td>
  </tr>
</table>

<table class='line-items'>
  <thead>
    <tr>
      <th>Nimike</th>
      <th class="n">Määrä ({#currencyCode#})</th>
      <th class="n">ALV%</th>
      <th class="n">ALV ({#currencyCode#})</th>
      <th class="n">Kokonaissumma ({#currencyCode#})</th>
    </tr>
  </thead>
  <tbody>{#mainTableItemsHtml#}</tbody>
</table>

<table class="totals">
  <tr class="tot-row">
    <td>{#taxableLabel#}</td>
    <td class="tot-val">{#taxableSum#}</td>
  </tr>
  <tr class="tot-row {#vatRowClass#}">
    <td>ALV<span class="{#vatPctSuffixClass#}"> ({#vatPct#}%)</span></td>
    <td class="tot-val">{#taxSum#}</td>
  </tr>

  {#externalItemsHtml#}

  <tr class="tot-row grand">
    <td style="font-weight:700;color:#000000">Yhteensä sis. ALV ({#currencyCode#})</td>
    <td class="tot-val" style="font-weight:700;color:#000000">{#totalIncVat#}</td>
  </tr>

  {#adjustmentItemsHtml#}

  <tr class="tot-row grand {#invoicedValueRowClass#}">
    <td style="font-weight:700;color:#000000">Laskutettu arvo</td>
    <td class="tot-val" style="font-weight:700;color:#000000">{#invoicedValue#}</td>
  </tr>

  <tr class="tot-row payment {#paymentMethodRowClass#}">
    <td></td>
    <td class="tot-val payment-detail">
      <span class="{#paymentByCardClass#}">Vastaanotettu {#cardBrand#} ****{#cardLastFour#}</span>
      <span class="{#paymentOnlineClass#}">Vastaanotettu verkossa</span>
      <span class="{#paymentCashClass#}">Vastaanotettu käteisellä</span>
      <span class="{#paymentOtherClass#}">{#paymentMode#}</span>
    </td>
  </tr>
</table>

</div></body></html>$template$,
  $row$<tr><td>{#title#}<span class="{#postDiscountClass#}"> (Alennuksen jälkeen {#postDiscountCurrency#} {#postDiscountAmount#})</span></td><td class="n">{#amount#}</td><td class="n">{#vatPct#}</td><td class="n">{#vatAmount#}</td><td class="n">{#total#}</td></tr>$row$,
  $totals$<tr class="tot-row"><td>{#title#}</td><td class="tot-val">{#amount#}</td></tr>$totals$,
  now(), now()
) ON CONFLICT (id) DO UPDATE SET
  template = EXCLUDED.template,
  line_item_row_template = EXCLUDED.line_item_row_template,
  totals_line_row_template = EXCLUDED.totals_line_row_template,
  merchant_id = EXCLUDED.merchant_id,
  merchant_operating_city_id = EXCLUDED.merchant_operating_city_id,
  invoice_type = EXCLUDED.invoice_type,
  language = EXCLUDED.language,
  updated_at = now();

------------------------------------------------------------
-- BPP: Generic / DUTCH
------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.invoice_template (
  id, merchant_id, merchant_operating_city_id, invoice_type, language,
  template, line_item_row_template, totals_line_row_template,
  created_at, updated_at
) VALUES (
  'bb000003-1234-4000-8000-000000000003',
  'a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e',
  'beabba6a-c817-43d2-93b2-a916f5cf2ceb',
  NULL,
  'DUTCH',
  $template$<!DOCTYPE html><html><head>
<meta charset='UTF-8'>
<title>Factuur {#invoiceNumber#}</title>
<style>
* { margin:0; padding:0; box-sizing:border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif;
       color:#2d2d2d; background:#fff; padding:32px;
       -webkit-print-color-adjust:exact !important; print-color-adjust:exact !important; }
.wrap { max-width:820px; margin:0 auto; }
.header { width:100%; border-collapse:collapse; border:none; margin-bottom:28px; }
.header td { vertical-align:top; border:none; }
.header td.header-right-cell { text-align:right; border:none; }
.logo-wrap { display:block; }
.logo { height:44px; width:auto; margin-bottom:10px; display:block; }
.inv-number { font-size:15px; font-weight:700; color:#2d2d2d; margin-bottom:10px; }
.inv-date { font-size:13px; color:#2d2d2d; margin-top:2px; }
.parties { width:100%; border-collapse:collapse; border:none; margin-bottom:24px; padding:16px 0; }
.party-right { text-align:right; vertical-align:top; border:none; }
.party-left { vertical-align:top; width:50%; border:none; }
.party-lbl-plain { font-size:13px; color:#2d2d2d; margin-bottom:4px; }
.party-name { font-size:15px; font-weight:600; color:#2d2d2d; margin-bottom:3px; }
.party-addr { font-size:13px; color:#2d2d2d; line-height:1.6; }
.party-tax { font-size:12px; color:#2d2d2d; margin-top:3px; }
.from-location { font-size:13px; color:#2d2d2d; margin-bottom:20px; }
table.line-items { width:100%; border-collapse:collapse; margin-bottom:24px; border:2px solid #2d2d2d; }
table.line-items th { font-size:13px; font-weight:700; color:#2d2d2d; text-transform:none; letter-spacing:0;
     border:1px solid #2d2d2d; padding:8px 10px; text-align:left; background:#fff; }
table.line-items td { font-size:13px; padding:9px 10px; border:1px solid #2d2d2d; color:#2d2d2d; }
table.line-items th.n, table.line-items td.n { text-align:right; }
.totals { width:400px; margin-left:auto; padding-top:24px; border-collapse:collapse; border:none; }
.tot-row { width:100%; }
.tot-row td { font-size:13px; color:#2d2d2d; padding:3px 0; border:none; }
.tot-row td.tot-val { text-align:right; padding-left:24px; white-space:nowrap; border:none; }
.tot-row.grand td { font-size:15px; font-weight:700; padding-top:12px; border:none; }
.tot-row.payment td { font-size:12px; padding-top:4px; border:none; }
.payment-detail { font-style:italic; }
.hidden { display: none !important; }
@media print { body { padding:16px; } }
</style>
</head><body><div class='wrap'>

<table class='header'>
  <tr>
    <td>
      <div class="logo-wrap {#logoClass#}">
        <img src="{#logoUrl#}" alt="" class="logo">
      </div>
    </td>
    <td class='header-right-cell'>
      <div class='inv-number'>Factuurnr. {#invoiceNumber#}</div>
      <div class='inv-date'>Datum: {#issuedAt#}</div>
      <div class="inv-date {#dueDateRowClass#}">Vervaldatum: {#dueAt#}</div>
    </td>
  </tr>
</table>

<table class="parties {#partiesTableClass#}">
  <tr>
    <td class="party-left {#leftPartyClass#}">
      <div class='party-lbl-plain'>Ontvanger:</div>
      <div class="party-name {#recipientNameClass#}" style="font-weight:700;color:#000000">{#recipientName#}</div>
      <div class="party-addr">{#recipientAddress#}</div>
    </td>
    <td class="party-right {#rightPartyClass#}">
      <div class="party-name {#rightNameClass#}">{#rightName#}</div>
      <div class="party-addr {#rightAddressClass#}">{#rightAddress#}</div>
      <div class="party-tax {#rightGstinClass#}">GSTIN {#rightGstin#}</div>
      <div class="party-tax {#rightVatNumberClass#}">BTW-nr. {#rightVatNumber#}</div>
    </td>
  </tr>
</table>

<table class='line-items'>
  <thead>
    <tr>
      <th>Omschrijving</th>
      <th class="n">Bedrag ({#currencyCode#})</th>
      <th class="n">BTW%</th>
      <th class="n">BTW ({#currencyCode#})</th>
      <th class="n">Totaal ({#currencyCode#})</th>
    </tr>
  </thead>
  <tbody>{#mainTableItemsHtml#}</tbody>
</table>

<table class="totals">
  <tr class="tot-row">
    <td>{#taxableLabel#}</td>
    <td class="tot-val">{#taxableSum#}</td>
  </tr>
  <tr class="tot-row {#vatRowClass#}">
    <td>BTW<span class="{#vatPctSuffixClass#}"> ({#vatPct#}%)</span></td>
    <td class="tot-val">{#taxSum#}</td>
  </tr>

  {#externalItemsHtml#}

  <tr class="tot-row grand">
    <td style="font-weight:700;color:#000000">Totaal incl. BTW ({#currencyCode#})</td>
    <td class="tot-val" style="font-weight:700;color:#000000">{#totalIncVat#}</td>
  </tr>

  {#adjustmentItemsHtml#}

  <tr class="tot-row grand {#invoicedValueRowClass#}">
    <td style="font-weight:700;color:#000000">Gefactureerde waarde</td>
    <td class="tot-val" style="font-weight:700;color:#000000">{#invoicedValue#}</td>
  </tr>

  <tr class="tot-row payment {#paymentMethodRowClass#}">
    <td></td>
    <td class="tot-val payment-detail">
      <span class="{#paymentByCardClass#}">Ontvangen via {#cardBrand#} ****{#cardLastFour#}</span>
      <span class="{#paymentOnlineClass#}">Ontvangen online</span>
      <span class="{#paymentCashClass#}">Ontvangen contant</span>
      <span class="{#paymentOtherClass#}">{#paymentMode#}</span>
    </td>
  </tr>
</table>

</div></body></html>$template$,
  $row$<tr><td>{#title#}<span class="{#postDiscountClass#}"> (Na korting {#postDiscountCurrency#} {#postDiscountAmount#})</span></td><td class="n">{#amount#}</td><td class="n">{#vatPct#}</td><td class="n">{#vatAmount#}</td><td class="n">{#total#}</td></tr>$row$,
  $totals$<tr class="tot-row"><td>{#title#}</td><td class="tot-val">{#amount#}</td></tr>$totals$,
  now(), now()
) ON CONFLICT (id) DO UPDATE SET
  template = EXCLUDED.template,
  line_item_row_template = EXCLUDED.line_item_row_template,
  totals_line_row_template = EXCLUDED.totals_line_row_template,
  merchant_id = EXCLUDED.merchant_id,
  merchant_operating_city_id = EXCLUDED.merchant_operating_city_id,
  invoice_type = EXCLUDED.invoice_type,
  language = EXCLUDED.language,
  updated_at = now();

------------------------------------------------------------
-- BPP: AggregatedCommission / ENGLISH
------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.invoice_template (
  id, merchant_id, merchant_operating_city_id, invoice_type, language,
  template, line_item_row_template, totals_line_row_template,
  created_at, updated_at
) VALUES (
  'bb000004-1234-4000-8000-000000000004',
  'a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e',
  'beabba6a-c817-43d2-93b2-a916f5cf2ceb',
  'AggregatedCommission',
  'ENGLISH',
  $template$<!DOCTYPE html><html><head>
<meta charset='UTF-8'>
<title>Commission on App Bookings {#invoiceNumber#}</title>
<style>
* { margin:0; padding:0; box-sizing:border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif;
       color:#2d2d2d; background:#fff; padding:32px;
       -webkit-print-color-adjust:exact !important; print-color-adjust:exact !important; }
.wrap { max-width:820px; margin:0 auto; }
.header { width:100%; border-collapse:collapse; border:none; margin-bottom:28px; }
.header td { vertical-align:top; border:none; }
.header td.header-right-cell { text-align:right; border:none; }
.logo-wrap { display:block; }
.logo { height:44px; width:auto; margin-bottom:10px; display:block; }
.invoicing-period { font-size:13px; color:#2d2d2d; margin-bottom:12px; }
.agg-footer { font-size:13px; color:#2d2d2d; line-height:1.6; margin-top:24px;
              padding-top:12px; border-top:1px solid #e0e0e0; }
.inv-number { font-size:15px; font-weight:700; color:#2d2d2d; margin-bottom:10px; }
.inv-date { font-size:13px; color:#2d2d2d; margin-top:2px; }
.parties { width:100%; border-collapse:collapse; border:none; margin-bottom:24px; padding:16px 0; }
.party-right { text-align:right; vertical-align:top; border:none; }
.party-left { vertical-align:top; width:50%; border:none; }
.party-lbl-plain { font-size:13px; color:#2d2d2d; margin-bottom:4px; }
.party-trade { font-size:16px; font-weight:700; color:#1a1a1a; margin-bottom:2px; }
.party-name { font-size:15px; font-weight:600; color:#2d2d2d; margin-bottom:3px; }
.party-addr { font-size:13px; color:#2d2d2d; line-height:1.6; }
.party-tax { font-size:12px; color:#2d2d2d; margin-top:3px; }
table.line-items { width:100%; border-collapse:collapse; margin-bottom:24px; border:2px solid #2d2d2d; }
table.line-items th { font-size:13px; font-weight:700; color:#2d2d2d; text-transform:none; letter-spacing:0;
     border:1px solid #2d2d2d; padding:8px 10px; text-align:left; background:#fff; }
table.line-items td { font-size:13px; padding:9px 10px; border:1px solid #2d2d2d; color:#2d2d2d; }
table.line-items th.n, table.line-items td.n { text-align:right; }
.totals { width:400px; margin-left:auto; padding-top:24px; border-collapse:collapse; border:none; }
.tot-row { width:100%; }
.tot-row td { font-size:13px; color:#2d2d2d; padding:3px 0; border:none; }
.tot-row td.tot-val { text-align:right; padding-left:24px; white-space:nowrap; border:none; }
.tot-row.grand td { font-size:15px; font-weight:700; padding-top:12px; border:none; }
.hidden { display: none !important; }
@media print { body { padding:16px; } }
</style>
</head><body><div class='wrap'>

<table class='header'>
  <tr>
    <td>
      <div class="logo-wrap {#logoClass#}">
        <img src="{#logoUrl#}" alt="" class="logo">
      </div>
    </td>
    <td class='header-right-cell'>
      <div class='inv-number'>Invoice No. {#invoiceNumber#}</div>
      <div class='inv-date'>Date: {#issuedAt#}</div>
      <div class="inv-date {#dueDateRowClass#}">Due Date: {#dueAt#}</div>
    </td>
  </tr>
</table>

<table class="parties {#partiesTableClass#}">
  <tr>
    <td class="party-left {#leftPartyClass#}">
      <div class='party-lbl-plain'>Recipient:</div>
      <div class="party-trade {#recipientNameClass#}">{#recipientName#}</div>
      <div class="party-addr {#recipientAddressClass#}">{#recipientAddress#}</div>
      <div class="party-tax {#recipientBusinessIdClass#}">Business ID: {#recipientBusinessId#}</div>
      <div class="party-tax {#recipientVatNumberClass#}">VAT No.: {#recipientVatNumber#}</div>
    </td>
    <td class="party-right {#rightPartyClass#}">
      <div class='party-lbl-plain'>Seller:</div>
      <div class="party-trade {#sellerTradeNameFieldClass#}">{#sellerTradeNameField#}</div>
      <div class="party-name {#sellerNameFieldClass#}">{#sellerNameField#}</div>
      <div class="party-addr {#sellerAddressFieldClass#}">{#sellerAddressField#}</div>
      <div class="party-tax {#sellerBusinessIdFieldClass#}">Business ID: {#sellerBusinessIdField#}</div>
      <div class="party-tax {#sellerVatNumberFieldClass#}">VAT No.: {#sellerVatNumberField#}</div>
    </td>
  </tr>
</table>

<div class="invoicing-period {#invoicingPeriodClass#}">
  Invoicing Period: <strong>{#periodStart#} - {#periodEnd#}</strong>
</div>

<table class='line-items'>
  <thead>
    <tr>
      <th>Title</th>
      <th class="n">Quantity</th>
      <th class="n">Unit Price</th>
      <th class="n">Amount</th>
      <th class="n">VAT {#aggCommVatPct#}%</th>
      <th class="n">Total</th>
    </tr>
  </thead>
  <tbody>{#mainTableItemsHtml#}</tbody>
</table>

<table class="totals">
  <tr class="tot-row"><td>Subtotal</td><td class="tot-val">{#subtotal#}</td></tr>
  <tr class="tot-row"><td>VAT {#aggCommVatPct#}%</td><td class="tot-val">{#aggCommVatAmount#}</td></tr>
  <tr class="tot-row grand">
    <td style="font-weight:700;color:#000000">Total incl. VAT ({#currencyCode#})</td>
    <td class="tot-val" style="font-weight:700;color:#000000">{#totalIncVat#}</td>
  </tr>
</table>

<div class='agg-footer'>
  This invoice represents commission charges for services provided during the period.
  Check your <span class="{#appNameClass#}">{#appName#} </span>fleet dashboard for the most up-to-date balance information.
</div>

</div></body></html>$template$,
  $row$<tr><td>{#title#}<span class="{#periodSuffixClass#}"> ({#periodStart#} - {#periodEnd#})</span></td><td class="n">{#quantity#}</td><td class="n">{#unitPrice#}</td><td class="n">{#amount#}</td><td class="n">{#vatAmount#}</td><td class="n">{#total#}</td></tr>$row$,
  $totals$<tr class="tot-row"><td>{#title#}</td><td class="tot-val">{#amount#}</td></tr>$totals$,
  now(), now()
) ON CONFLICT (id) DO UPDATE SET
  template = EXCLUDED.template,
  line_item_row_template = EXCLUDED.line_item_row_template,
  totals_line_row_template = EXCLUDED.totals_line_row_template,
  merchant_id = EXCLUDED.merchant_id,
  merchant_operating_city_id = EXCLUDED.merchant_operating_city_id,
  invoice_type = EXCLUDED.invoice_type,
  language = EXCLUDED.language,
  updated_at = now();

------------------------------------------------------------
-- BPP: AggregatedCommission / FINNISH
------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.invoice_template (
  id, merchant_id, merchant_operating_city_id, invoice_type, language,
  template, line_item_row_template, totals_line_row_template,
  created_at, updated_at
) VALUES (
  'bb000005-1234-4000-8000-000000000005',
  'a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e',
  'beabba6a-c817-43d2-93b2-a916f5cf2ceb',
  'AggregatedCommission',
  'FINNISH',
  $template$<!DOCTYPE html><html><head>
<meta charset='UTF-8'>
<title>Komissio sovellustilauksista {#invoiceNumber#}</title>
<style>
* { margin:0; padding:0; box-sizing:border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif;
       color:#2d2d2d; background:#fff; padding:32px;
       -webkit-print-color-adjust:exact !important; print-color-adjust:exact !important; }
.wrap { max-width:820px; margin:0 auto; }
.header { width:100%; border-collapse:collapse; border:none; margin-bottom:28px; }
.header td { vertical-align:top; border:none; }
.header td.header-right-cell { text-align:right; border:none; }
.logo-wrap { display:block; }
.logo { height:44px; width:auto; margin-bottom:10px; display:block; }
.invoicing-period { font-size:13px; color:#2d2d2d; margin-bottom:12px; }
.agg-footer { font-size:13px; color:#2d2d2d; line-height:1.6; margin-top:24px;
              padding-top:12px; border-top:1px solid #e0e0e0; }
.inv-number { font-size:15px; font-weight:700; color:#2d2d2d; margin-bottom:10px; }
.inv-date { font-size:13px; color:#2d2d2d; margin-top:2px; }
.parties { width:100%; border-collapse:collapse; border:none; margin-bottom:24px; padding:16px 0; }
.party-right { text-align:right; vertical-align:top; border:none; }
.party-left { vertical-align:top; width:50%; border:none; }
.party-lbl-plain { font-size:13px; color:#2d2d2d; margin-bottom:4px; }
.party-trade { font-size:16px; font-weight:700; color:#1a1a1a; margin-bottom:2px; }
.party-name { font-size:15px; font-weight:600; color:#2d2d2d; margin-bottom:3px; }
.party-addr { font-size:13px; color:#2d2d2d; line-height:1.6; }
.party-tax { font-size:12px; color:#2d2d2d; margin-top:3px; }
table.line-items { width:100%; border-collapse:collapse; margin-bottom:24px; border:2px solid #2d2d2d; }
table.line-items th { font-size:13px; font-weight:700; color:#2d2d2d; text-transform:none; letter-spacing:0;
     border:1px solid #2d2d2d; padding:8px 10px; text-align:left; background:#fff; }
table.line-items td { font-size:13px; padding:9px 10px; border:1px solid #2d2d2d; color:#2d2d2d; }
table.line-items th.n, table.line-items td.n { text-align:right; }
.totals { width:400px; margin-left:auto; padding-top:24px; border-collapse:collapse; border:none; }
.tot-row { width:100%; }
.tot-row td { font-size:13px; color:#2d2d2d; padding:3px 0; border:none; }
.tot-row td.tot-val { text-align:right; padding-left:24px; white-space:nowrap; border:none; }
.tot-row.grand td { font-size:15px; font-weight:700; padding-top:12px; border:none; }
.hidden { display: none !important; }
@media print { body { padding:16px; } }
</style>
</head><body><div class='wrap'>

<table class='header'>
  <tr>
    <td>
      <div class="logo-wrap {#logoClass#}">
        <img src="{#logoUrl#}" alt="" class="logo">
      </div>
    </td>
    <td class='header-right-cell'>
      <div class='inv-number'>Laskun nro. {#invoiceNumber#}</div>
      <div class='inv-date'>Päivämäärä: {#issuedAt#}</div>
      <div class="inv-date {#dueDateRowClass#}">Eräpäivä: {#dueAt#}</div>
    </td>
  </tr>
</table>

<table class="parties {#partiesTableClass#}">
  <tr>
    <td class="party-left {#leftPartyClass#}">
      <div class='party-lbl-plain'>Vastaanottaja:</div>
      <div class="party-trade {#recipientNameClass#}">{#recipientName#}</div>
      <div class="party-addr {#recipientAddressClass#}">{#recipientAddress#}</div>
      <div class="party-tax {#recipientBusinessIdClass#}">Y-tunnus: {#recipientBusinessId#}</div>
      <div class="party-tax {#recipientVatNumberClass#}">ALV-nro.: {#recipientVatNumber#}</div>
    </td>
    <td class="party-right {#rightPartyClass#}">
      <div class='party-lbl-plain'>Myyjä:</div>
      <div class="party-trade {#sellerTradeNameFieldClass#}">{#sellerTradeNameField#}</div>
      <div class="party-name {#sellerNameFieldClass#}">{#sellerNameField#}</div>
      <div class="party-addr {#sellerAddressFieldClass#}">{#sellerAddressField#}</div>
      <div class="party-tax {#sellerBusinessIdFieldClass#}">Y-tunnus: {#sellerBusinessIdField#}</div>
      <div class="party-tax {#sellerVatNumberFieldClass#}">ALV-nro.: {#sellerVatNumberField#}</div>
    </td>
  </tr>
</table>

<div class="invoicing-period {#invoicingPeriodClass#}">
  Laskutusjakso: <strong>{#periodStart#} - {#periodEnd#}</strong>
</div>

<table class='line-items'>
  <thead>
    <tr>
      <th>Nimike</th>
      <th class="n">Määrä</th>
      <th class="n">Yksikköhinta</th>
      <th class="n">Summa</th>
      <th class="n">ALV {#aggCommVatPct#}%</th>
      <th class="n">Kokonaissumma</th>
    </tr>
  </thead>
  <tbody>{#mainTableItemsHtml#}</tbody>
</table>

<table class="totals">
  <tr class="tot-row"><td>Yhteensä</td><td class="tot-val">{#subtotal#}</td></tr>
  <tr class="tot-row"><td>ALV {#aggCommVatPct#}%</td><td class="tot-val">{#aggCommVatAmount#}</td></tr>
  <tr class="tot-row grand">
    <td style="font-weight:700;color:#000000">Yhteensä sis. ALV ({#currencyCode#})</td>
    <td class="tot-val" style="font-weight:700;color:#000000">{#totalIncVat#}</td>
  </tr>
</table>

<div class='agg-footer'>
  Tämä lasku kuvaa ajanjakson aikana perittyjä komissiomaksuja. Tarkista
  <span class="{#appNameClass#}">{#appName#}-</span>kalustopaneelistasi ajantasaiset tiedot saldostasi.
</div>

</div></body></html>$template$,
  $row$<tr><td>{#title#}<span class="{#periodSuffixClass#}"> ({#periodStart#} - {#periodEnd#})</span></td><td class="n">{#quantity#}</td><td class="n">{#unitPrice#}</td><td class="n">{#amount#}</td><td class="n">{#vatAmount#}</td><td class="n">{#total#}</td></tr>$row$,
  $totals$<tr class="tot-row"><td>{#title#}</td><td class="tot-val">{#amount#}</td></tr>$totals$,
  now(), now()
) ON CONFLICT (id) DO UPDATE SET
  template = EXCLUDED.template,
  line_item_row_template = EXCLUDED.line_item_row_template,
  totals_line_row_template = EXCLUDED.totals_line_row_template,
  merchant_id = EXCLUDED.merchant_id,
  merchant_operating_city_id = EXCLUDED.merchant_operating_city_id,
  invoice_type = EXCLUDED.invoice_type,
  language = EXCLUDED.language,
  updated_at = now();

------------------------------------------------------------
-- BPP: AggregatedCommission / DUTCH
------------------------------------------------------------
INSERT INTO atlas_driver_offer_bpp.invoice_template (
  id, merchant_id, merchant_operating_city_id, invoice_type, language,
  template, line_item_row_template, totals_line_row_template,
  created_at, updated_at
) VALUES (
  'bb000006-1234-4000-8000-000000000006',
  'a7f3c8e2-4b5d-4c9a-8f6e-2d1a3b4c5d6e',
  'beabba6a-c817-43d2-93b2-a916f5cf2ceb',
  'AggregatedCommission',
  'DUTCH',
  $template$<!DOCTYPE html><html><head>
<meta charset='UTF-8'>
<title>Commissie op app-boekingen {#invoiceNumber#}</title>
<style>
* { margin:0; padding:0; box-sizing:border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif;
       color:#2d2d2d; background:#fff; padding:32px;
       -webkit-print-color-adjust:exact !important; print-color-adjust:exact !important; }
.wrap { max-width:820px; margin:0 auto; }
.header { width:100%; border-collapse:collapse; border:none; margin-bottom:28px; }
.header td { vertical-align:top; border:none; }
.header td.header-right-cell { text-align:right; border:none; }
.logo-wrap { display:block; }
.logo { height:44px; width:auto; margin-bottom:10px; display:block; }
.invoicing-period { font-size:13px; color:#2d2d2d; margin-bottom:12px; }
.agg-footer { font-size:13px; color:#2d2d2d; line-height:1.6; margin-top:24px;
              padding-top:12px; border-top:1px solid #e0e0e0; }
.inv-number { font-size:15px; font-weight:700; color:#2d2d2d; margin-bottom:10px; }
.inv-date { font-size:13px; color:#2d2d2d; margin-top:2px; }
.parties { width:100%; border-collapse:collapse; border:none; margin-bottom:24px; padding:16px 0; }
.party-right { text-align:right; vertical-align:top; border:none; }
.party-left { vertical-align:top; width:50%; border:none; }
.party-lbl-plain { font-size:13px; color:#2d2d2d; margin-bottom:4px; }
.party-trade { font-size:16px; font-weight:700; color:#1a1a1a; margin-bottom:2px; }
.party-name { font-size:15px; font-weight:600; color:#2d2d2d; margin-bottom:3px; }
.party-addr { font-size:13px; color:#2d2d2d; line-height:1.6; }
.party-tax { font-size:12px; color:#2d2d2d; margin-top:3px; }
table.line-items { width:100%; border-collapse:collapse; margin-bottom:24px; border:2px solid #2d2d2d; }
table.line-items th { font-size:13px; font-weight:700; color:#2d2d2d; text-transform:none; letter-spacing:0;
     border:1px solid #2d2d2d; padding:8px 10px; text-align:left; background:#fff; }
table.line-items td { font-size:13px; padding:9px 10px; border:1px solid #2d2d2d; color:#2d2d2d; }
table.line-items th.n, table.line-items td.n { text-align:right; }
.totals { width:400px; margin-left:auto; padding-top:24px; border-collapse:collapse; border:none; }
.tot-row { width:100%; }
.tot-row td { font-size:13px; color:#2d2d2d; padding:3px 0; border:none; }
.tot-row td.tot-val { text-align:right; padding-left:24px; white-space:nowrap; border:none; }
.tot-row.grand td { font-size:15px; font-weight:700; padding-top:12px; border:none; }
.hidden { display: none !important; }
@media print { body { padding:16px; } }
</style>
</head><body><div class='wrap'>

<table class='header'>
  <tr>
    <td>
      <div class="logo-wrap {#logoClass#}">
        <img src="{#logoUrl#}" alt="" class="logo">
      </div>
    </td>
    <td class='header-right-cell'>
      <div class='inv-number'>Factuurnr. {#invoiceNumber#}</div>
      <div class='inv-date'>Datum: {#issuedAt#}</div>
      <div class="inv-date {#dueDateRowClass#}">Vervaldatum: {#dueAt#}</div>
    </td>
  </tr>
</table>

<table class="parties {#partiesTableClass#}">
  <tr>
    <td class="party-left {#leftPartyClass#}">
      <div class='party-lbl-plain'>Ontvanger:</div>
      <div class="party-trade {#recipientNameClass#}">{#recipientName#}</div>
      <div class="party-addr {#recipientAddressClass#}">{#recipientAddress#}</div>
      <div class="party-tax {#recipientBusinessIdClass#}">Bedrijfs-ID: {#recipientBusinessId#}</div>
      <div class="party-tax {#recipientVatNumberClass#}">BTW-nr.: {#recipientVatNumber#}</div>
    </td>
    <td class="party-right {#rightPartyClass#}">
      <div class='party-lbl-plain'>Verkoper:</div>
      <div class="party-trade {#sellerTradeNameFieldClass#}">{#sellerTradeNameField#}</div>
      <div class="party-name {#sellerNameFieldClass#}">{#sellerNameField#}</div>
      <div class="party-addr {#sellerAddressFieldClass#}">{#sellerAddressField#}</div>
      <div class="party-tax {#sellerBusinessIdFieldClass#}">Bedrijfs-ID: {#sellerBusinessIdField#}</div>
      <div class="party-tax {#sellerVatNumberFieldClass#}">BTW-nr.: {#sellerVatNumberField#}</div>
    </td>
  </tr>
</table>

<div class="invoicing-period {#invoicingPeriodClass#}">
  Factureringsperiode: <strong>{#periodStart#} - {#periodEnd#}</strong>
</div>

<table class='line-items'>
  <thead>
    <tr>
      <th>Omschrijving</th>
      <th class="n">Aantal</th>
      <th class="n">Eenheidsprijs</th>
      <th class="n">Bedrag</th>
      <th class="n">BTW {#aggCommVatPct#}%</th>
      <th class="n">Totaal</th>
    </tr>
  </thead>
  <tbody>{#mainTableItemsHtml#}</tbody>
</table>

<table class="totals">
  <tr class="tot-row"><td>Subtotaal</td><td class="tot-val">{#subtotal#}</td></tr>
  <tr class="tot-row"><td>BTW {#aggCommVatPct#}%</td><td class="tot-val">{#aggCommVatAmount#}</td></tr>
  <tr class="tot-row grand">
    <td style="font-weight:700;color:#000000">Totaal incl. BTW ({#currencyCode#})</td>
    <td class="tot-val" style="font-weight:700;color:#000000">{#totalIncVat#}</td>
  </tr>
</table>

<div class='agg-footer'>
  Deze factuur vertegenwoordigt commissiekosten voor tijdens de periode geleverde diensten. Raadpleeg uw
  <span class="{#appNameClass#}">{#appName#}-</span>vlootdashboard voor actuele saldo-informatie.
</div>

</div></body></html>$template$,
  $row$<tr><td>{#title#}<span class="{#periodSuffixClass#}"> ({#periodStart#} - {#periodEnd#})</span></td><td class="n">{#quantity#}</td><td class="n">{#unitPrice#}</td><td class="n">{#amount#}</td><td class="n">{#vatAmount#}</td><td class="n">{#total#}</td></tr>$row$,
  $totals$<tr class="tot-row"><td>{#title#}</td><td class="tot-val">{#amount#}</td></tr>$totals$,
  now(), now()
) ON CONFLICT (id) DO UPDATE SET
  template = EXCLUDED.template,
  line_item_row_template = EXCLUDED.line_item_row_template,
  totals_line_row_template = EXCLUDED.totals_line_row_template,
  merchant_id = EXCLUDED.merchant_id,
  merchant_operating_city_id = EXCLUDED.merchant_operating_city_id,
  invoice_type = EXCLUDED.invoice_type,
  language = EXCLUDED.language,
  updated_at = now();

------------------------------------------------------------
-- BAP: Generic / ENGLISH
------------------------------------------------------------
INSERT INTO atlas_app.invoice_template (
  id, merchant_id, merchant_operating_city_id, invoice_type, language,
  template, line_item_row_template, totals_line_row_template,
  created_at, updated_at
) VALUES (
  'aa000001-1234-4000-8000-000000000001',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f',
  'f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'ENGLISH',
  $template$<!DOCTYPE html><html><head>
<meta charset='UTF-8'>
<title>Invoice {#invoiceNumber#}</title>
<style>
* { margin:0; padding:0; box-sizing:border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif;
       color:#2d2d2d; background:#fff; padding:32px;
       -webkit-print-color-adjust:exact !important; print-color-adjust:exact !important; }
.wrap { max-width:820px; margin:0 auto; }
.header { width:100%; border-collapse:collapse; border:none; margin-bottom:28px; }
.header td { vertical-align:top; border:none; }
.header td.header-right-cell { text-align:right; border:none; }
.logo-wrap { display:block; }
.logo { height:44px; width:auto; margin-bottom:10px; display:block; }
.inv-number { font-size:15px; font-weight:700; color:#2d2d2d; margin-bottom:10px; }
.inv-date { font-size:13px; color:#2d2d2d; margin-top:2px; }
.parties { width:100%; border-collapse:collapse; border:none; margin-bottom:24px; padding:16px 0; }
.party-right { text-align:right; vertical-align:top; border:none; }
.party-left { vertical-align:top; width:50%; border:none; }
.party-lbl-plain { font-size:13px; color:#2d2d2d; margin-bottom:4px; }
.party-name { font-size:15px; font-weight:600; color:#2d2d2d; margin-bottom:3px; }
.party-addr { font-size:13px; color:#2d2d2d; line-height:1.6; }
.party-tax { font-size:12px; color:#2d2d2d; margin-top:3px; }
.from-location { font-size:13px; color:#2d2d2d; margin-bottom:20px; }
table.line-items { width:100%; border-collapse:collapse; margin-bottom:24px; border:2px solid #2d2d2d; }
table.line-items th { font-size:13px; font-weight:700; color:#2d2d2d; text-transform:none; letter-spacing:0;
     border:1px solid #2d2d2d; padding:8px 10px; text-align:left; background:#fff; }
table.line-items td { font-size:13px; padding:9px 10px; border:1px solid #2d2d2d; color:#2d2d2d; }
table.line-items th.n, table.line-items td.n { text-align:right; }
.totals { width:400px; margin-left:auto; padding-top:24px; border-collapse:collapse; border:none; }
.tot-row { width:100%; }
.tot-row td { font-size:13px; color:#2d2d2d; padding:3px 0; border:none; }
.tot-row td.tot-val { text-align:right; padding-left:24px; white-space:nowrap; border:none; }
.tot-row.grand td { font-size:15px; font-weight:700; padding-top:12px; border:none; }
.tot-row.payment td { font-size:12px; padding-top:4px; border:none; }
.payment-detail { font-style:italic; }
.hidden { display: none !important; }
@media print { body { padding:16px; } }
</style>
</head><body><div class='wrap'>

<table class='header'>
  <tr>
    <td>
      <div class="logo-wrap {#logoClass#}">
        <img src="{#logoUrl#}" alt="" class="logo">
      </div>
    </td>
    <td class='header-right-cell'>
      <div class='inv-number'>Invoice No. {#invoiceNumber#}</div>
      <div class='inv-date'>Date: {#issuedAt#}</div>
      <div class="inv-date {#dueDateRowClass#}">Due Date: {#dueAt#}</div>
    </td>
  </tr>
</table>

<table class="parties {#partiesTableClass#}">
  <tr>
    <td class="party-left {#leftPartyClass#}">
      <div class='party-lbl-plain'>Recipient:</div>
      <div class="party-name {#recipientNameClass#}" style="font-weight:700;color:#000000">{#recipientName#}</div>
      <div class="party-addr">{#recipientAddress#}</div>
    </td>
    <td class="party-right {#rightPartyClass#}">
      <div class="party-name {#rightNameClass#}">{#rightName#}</div>
      <div class="party-addr {#rightAddressClass#}">{#rightAddress#}</div>
      <div class="party-tax {#rightGstinClass#}">GSTIN {#rightGstin#}</div>
      <div class="party-tax {#rightVatNumberClass#}">VAT No. {#rightVatNumber#}</div>
    </td>
  </tr>
</table>

<table class='line-items'>
  <thead>
    <tr>
      <th>Title</th>
      <th class="n">Amount ({#currencyCode#})</th>
      <th class="n">VAT (%)</th>
      <th class="n">VAT ({#currencyCode#})</th>
      <th class="n">Total ({#currencyCode#})</th>
    </tr>
  </thead>
  <tbody>{#mainTableItemsHtml#}</tbody>
</table>

<table class="totals">
  <tr class="tot-row">
    <td>{#taxableLabel#}</td>
    <td class="tot-val">{#taxableSum#}</td>
  </tr>
  <tr class="tot-row {#vatRowClass#}">
    <td>VAT<span class="{#vatPctSuffixClass#}"> ({#vatPct#}%)</span></td>
    <td class="tot-val">{#taxSum#}</td>
  </tr>

  {#externalItemsHtml#}

  <tr class="tot-row grand">
    <td style="font-weight:700;color:#000000">Total incl. VAT ({#currencyCode#})</td>
    <td class="tot-val" style="font-weight:700;color:#000000">{#totalIncVat#}</td>
  </tr>

  {#adjustmentItemsHtml#}

  <tr class="tot-row grand {#invoicedValueRowClass#}">
    <td style="font-weight:700;color:#000000">Invoiced Value</td>
    <td class="tot-val" style="font-weight:700;color:#000000">{#invoicedValue#}</td>
  </tr>

  <tr class="tot-row payment {#paymentMethodRowClass#}">
    <td></td>
    <td class="tot-val payment-detail">
      <span class="{#paymentByCardClass#}">Received via {#cardBrand#} ****{#cardLastFour#}</span>
      <span class="{#paymentOnlineClass#}">Received online</span>
      <span class="{#paymentCashClass#}">Received in cash</span>
      <span class="{#paymentOtherClass#}">{#paymentMode#}</span>
    </td>
  </tr>
</table>

</div></body></html>$template$,
  $row$<tr><td>{#title#}<span class="{#postDiscountClass#}"> (Post Discount {#postDiscountCurrency#} {#postDiscountAmount#})</span></td><td class="n">{#amount#}</td><td class="n">{#vatPct#}</td><td class="n">{#vatAmount#}</td><td class="n">{#total#}</td></tr>$row$,
  $totals$<tr class="tot-row"><td>{#title#}</td><td class="tot-val">{#amount#}</td></tr>$totals$,
  now(), now()
) ON CONFLICT (id) DO UPDATE SET
  template = EXCLUDED.template,
  line_item_row_template = EXCLUDED.line_item_row_template,
  totals_line_row_template = EXCLUDED.totals_line_row_template,
  merchant_id = EXCLUDED.merchant_id,
  merchant_operating_city_id = EXCLUDED.merchant_operating_city_id,
  invoice_type = EXCLUDED.invoice_type,
  language = EXCLUDED.language,
  updated_at = now();

------------------------------------------------------------
-- BAP: Generic / FINNISH
------------------------------------------------------------
INSERT INTO atlas_app.invoice_template (
  id, merchant_id, merchant_operating_city_id, invoice_type, language,
  template, line_item_row_template, totals_line_row_template,
  created_at, updated_at
) VALUES (
  'aa000002-1234-4000-8000-000000000002',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f',
  'f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'FINNISH',
  $template$<!DOCTYPE html><html><head>
<meta charset='UTF-8'>
<title>Lasku {#invoiceNumber#}</title>
<style>
* { margin:0; padding:0; box-sizing:border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif;
       color:#2d2d2d; background:#fff; padding:32px;
       -webkit-print-color-adjust:exact !important; print-color-adjust:exact !important; }
.wrap { max-width:820px; margin:0 auto; }
.header { width:100%; border-collapse:collapse; border:none; margin-bottom:28px; }
.header td { vertical-align:top; border:none; }
.header td.header-right-cell { text-align:right; border:none; }
.logo-wrap { display:block; }
.logo { height:44px; width:auto; margin-bottom:10px; display:block; }
.inv-number { font-size:15px; font-weight:700; color:#2d2d2d; margin-bottom:10px; }
.inv-date { font-size:13px; color:#2d2d2d; margin-top:2px; }
.parties { width:100%; border-collapse:collapse; border:none; margin-bottom:24px; padding:16px 0; }
.party-right { text-align:right; vertical-align:top; border:none; }
.party-left { vertical-align:top; width:50%; border:none; }
.party-lbl-plain { font-size:13px; color:#2d2d2d; margin-bottom:4px; }
.party-name { font-size:15px; font-weight:600; color:#2d2d2d; margin-bottom:3px; }
.party-addr { font-size:13px; color:#2d2d2d; line-height:1.6; }
.party-tax { font-size:12px; color:#2d2d2d; margin-top:3px; }
.from-location { font-size:13px; color:#2d2d2d; margin-bottom:20px; }
table.line-items { width:100%; border-collapse:collapse; margin-bottom:24px; border:2px solid #2d2d2d; }
table.line-items th { font-size:13px; font-weight:700; color:#2d2d2d; text-transform:none; letter-spacing:0;
     border:1px solid #2d2d2d; padding:8px 10px; text-align:left; background:#fff; }
table.line-items td { font-size:13px; padding:9px 10px; border:1px solid #2d2d2d; color:#2d2d2d; }
table.line-items th.n, table.line-items td.n { text-align:right; }
.totals { width:400px; margin-left:auto; padding-top:24px; border-collapse:collapse; border:none; }
.tot-row { width:100%; }
.tot-row td { font-size:13px; color:#2d2d2d; padding:3px 0; border:none; }
.tot-row td.tot-val { text-align:right; padding-left:24px; white-space:nowrap; border:none; }
.tot-row.grand td { font-size:15px; font-weight:700; padding-top:12px; border:none; }
.tot-row.payment td { font-size:12px; padding-top:4px; border:none; }
.payment-detail { font-style:italic; }
.hidden { display: none !important; }
@media print { body { padding:16px; } }
</style>
</head><body><div class='wrap'>

<table class='header'>
  <tr>
    <td>
      <div class="logo-wrap {#logoClass#}">
        <img src="{#logoUrl#}" alt="" class="logo">
      </div>
    </td>
    <td class='header-right-cell'>
      <div class='inv-number'>Laskun nro. {#invoiceNumber#}</div>
      <div class='inv-date'>Päivämäärä: {#issuedAt#}</div>
      <div class="inv-date {#dueDateRowClass#}">Eräpäivä: {#dueAt#}</div>
    </td>
  </tr>
</table>

<table class="parties {#partiesTableClass#}">
  <tr>
    <td class="party-left {#leftPartyClass#}">
      <div class='party-lbl-plain'>Vastaanottaja:</div>
      <div class="party-name {#recipientNameClass#}" style="font-weight:700;color:#000000">{#recipientName#}</div>
      <div class="party-addr">{#recipientAddress#}</div>
    </td>
    <td class="party-right {#rightPartyClass#}">
      <div class="party-name {#rightNameClass#}">{#rightName#}</div>
      <div class="party-addr {#rightAddressClass#}">{#rightAddress#}</div>
      <div class="party-tax {#rightGstinClass#}">GSTIN {#rightGstin#}</div>
      <div class="party-tax {#rightVatNumberClass#}">ALV-nro. {#rightVatNumber#}</div>
    </td>
  </tr>
</table>

<table class='line-items'>
  <thead>
    <tr>
      <th>Nimike</th>
      <th class="n">Määrä ({#currencyCode#})</th>
      <th class="n">ALV%</th>
      <th class="n">ALV ({#currencyCode#})</th>
      <th class="n">Kokonaissumma ({#currencyCode#})</th>
    </tr>
  </thead>
  <tbody>{#mainTableItemsHtml#}</tbody>
</table>

<table class="totals">
  <tr class="tot-row">
    <td>{#taxableLabel#}</td>
    <td class="tot-val">{#taxableSum#}</td>
  </tr>
  <tr class="tot-row {#vatRowClass#}">
    <td>ALV<span class="{#vatPctSuffixClass#}"> ({#vatPct#}%)</span></td>
    <td class="tot-val">{#taxSum#}</td>
  </tr>

  {#externalItemsHtml#}

  <tr class="tot-row grand">
    <td style="font-weight:700;color:#000000">Yhteensä sis. ALV ({#currencyCode#})</td>
    <td class="tot-val" style="font-weight:700;color:#000000">{#totalIncVat#}</td>
  </tr>

  {#adjustmentItemsHtml#}

  <tr class="tot-row grand {#invoicedValueRowClass#}">
    <td style="font-weight:700;color:#000000">Laskutettu arvo</td>
    <td class="tot-val" style="font-weight:700;color:#000000">{#invoicedValue#}</td>
  </tr>

  <tr class="tot-row payment {#paymentMethodRowClass#}">
    <td></td>
    <td class="tot-val payment-detail">
      <span class="{#paymentByCardClass#}">Vastaanotettu {#cardBrand#} ****{#cardLastFour#}</span>
      <span class="{#paymentOnlineClass#}">Vastaanotettu verkossa</span>
      <span class="{#paymentCashClass#}">Vastaanotettu käteisellä</span>
      <span class="{#paymentOtherClass#}">{#paymentMode#}</span>
    </td>
  </tr>
</table>

</div></body></html>$template$,
  $row$<tr><td>{#title#}<span class="{#postDiscountClass#}"> (Alennuksen jälkeen {#postDiscountCurrency#} {#postDiscountAmount#})</span></td><td class="n">{#amount#}</td><td class="n">{#vatPct#}</td><td class="n">{#vatAmount#}</td><td class="n">{#total#}</td></tr>$row$,
  $totals$<tr class="tot-row"><td>{#title#}</td><td class="tot-val">{#amount#}</td></tr>$totals$,
  now(), now()
) ON CONFLICT (id) DO UPDATE SET
  template = EXCLUDED.template,
  line_item_row_template = EXCLUDED.line_item_row_template,
  totals_line_row_template = EXCLUDED.totals_line_row_template,
  merchant_id = EXCLUDED.merchant_id,
  merchant_operating_city_id = EXCLUDED.merchant_operating_city_id,
  invoice_type = EXCLUDED.invoice_type,
  language = EXCLUDED.language,
  updated_at = now();

------------------------------------------------------------
-- BAP: Generic / DUTCH
------------------------------------------------------------
INSERT INTO atlas_app.invoice_template (
  id, merchant_id, merchant_operating_city_id, invoice_type, language,
  template, line_item_row_template, totals_line_row_template,
  created_at, updated_at
) VALUES (
  'aa000003-1234-4000-8000-000000000003',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f',
  'f9903ef6-f595-428e-b5ac-e8816cbdf979',
  NULL,
  'DUTCH',
  $template$<!DOCTYPE html><html><head>
<meta charset='UTF-8'>
<title>Factuur {#invoiceNumber#}</title>
<style>
* { margin:0; padding:0; box-sizing:border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif;
       color:#2d2d2d; background:#fff; padding:32px;
       -webkit-print-color-adjust:exact !important; print-color-adjust:exact !important; }
.wrap { max-width:820px; margin:0 auto; }
.header { width:100%; border-collapse:collapse; border:none; margin-bottom:28px; }
.header td { vertical-align:top; border:none; }
.header td.header-right-cell { text-align:right; border:none; }
.logo-wrap { display:block; }
.logo { height:44px; width:auto; margin-bottom:10px; display:block; }
.inv-number { font-size:15px; font-weight:700; color:#2d2d2d; margin-bottom:10px; }
.inv-date { font-size:13px; color:#2d2d2d; margin-top:2px; }
.parties { width:100%; border-collapse:collapse; border:none; margin-bottom:24px; padding:16px 0; }
.party-right { text-align:right; vertical-align:top; border:none; }
.party-left { vertical-align:top; width:50%; border:none; }
.party-lbl-plain { font-size:13px; color:#2d2d2d; margin-bottom:4px; }
.party-name { font-size:15px; font-weight:600; color:#2d2d2d; margin-bottom:3px; }
.party-addr { font-size:13px; color:#2d2d2d; line-height:1.6; }
.party-tax { font-size:12px; color:#2d2d2d; margin-top:3px; }
.from-location { font-size:13px; color:#2d2d2d; margin-bottom:20px; }
table.line-items { width:100%; border-collapse:collapse; margin-bottom:24px; border:2px solid #2d2d2d; }
table.line-items th { font-size:13px; font-weight:700; color:#2d2d2d; text-transform:none; letter-spacing:0;
     border:1px solid #2d2d2d; padding:8px 10px; text-align:left; background:#fff; }
table.line-items td { font-size:13px; padding:9px 10px; border:1px solid #2d2d2d; color:#2d2d2d; }
table.line-items th.n, table.line-items td.n { text-align:right; }
.totals { width:400px; margin-left:auto; padding-top:24px; border-collapse:collapse; border:none; }
.tot-row { width:100%; }
.tot-row td { font-size:13px; color:#2d2d2d; padding:3px 0; border:none; }
.tot-row td.tot-val { text-align:right; padding-left:24px; white-space:nowrap; border:none; }
.tot-row.grand td { font-size:15px; font-weight:700; padding-top:12px; border:none; }
.tot-row.payment td { font-size:12px; padding-top:4px; border:none; }
.payment-detail { font-style:italic; }
.hidden { display: none !important; }
@media print { body { padding:16px; } }
</style>
</head><body><div class='wrap'>

<table class='header'>
  <tr>
    <td>
      <div class="logo-wrap {#logoClass#}">
        <img src="{#logoUrl#}" alt="" class="logo">
      </div>
    </td>
    <td class='header-right-cell'>
      <div class='inv-number'>Factuurnr. {#invoiceNumber#}</div>
      <div class='inv-date'>Datum: {#issuedAt#}</div>
      <div class="inv-date {#dueDateRowClass#}">Vervaldatum: {#dueAt#}</div>
    </td>
  </tr>
</table>

<table class="parties {#partiesTableClass#}">
  <tr>
    <td class="party-left {#leftPartyClass#}">
      <div class='party-lbl-plain'>Ontvanger:</div>
      <div class="party-name {#recipientNameClass#}" style="font-weight:700;color:#000000">{#recipientName#}</div>
      <div class="party-addr">{#recipientAddress#}</div>
    </td>
    <td class="party-right {#rightPartyClass#}">
      <div class="party-name {#rightNameClass#}">{#rightName#}</div>
      <div class="party-addr {#rightAddressClass#}">{#rightAddress#}</div>
      <div class="party-tax {#rightGstinClass#}">GSTIN {#rightGstin#}</div>
      <div class="party-tax {#rightVatNumberClass#}">BTW-nr. {#rightVatNumber#}</div>
    </td>
  </tr>
</table>

<table class='line-items'>
  <thead>
    <tr>
      <th>Omschrijving</th>
      <th class="n">Bedrag ({#currencyCode#})</th>
      <th class="n">BTW%</th>
      <th class="n">BTW ({#currencyCode#})</th>
      <th class="n">Totaal ({#currencyCode#})</th>
    </tr>
  </thead>
  <tbody>{#mainTableItemsHtml#}</tbody>
</table>

<table class="totals">
  <tr class="tot-row">
    <td>{#taxableLabel#}</td>
    <td class="tot-val">{#taxableSum#}</td>
  </tr>
  <tr class="tot-row {#vatRowClass#}">
    <td>BTW<span class="{#vatPctSuffixClass#}"> ({#vatPct#}%)</span></td>
    <td class="tot-val">{#taxSum#}</td>
  </tr>

  {#externalItemsHtml#}

  <tr class="tot-row grand">
    <td style="font-weight:700;color:#000000">Totaal incl. BTW ({#currencyCode#})</td>
    <td class="tot-val" style="font-weight:700;color:#000000">{#totalIncVat#}</td>
  </tr>

  {#adjustmentItemsHtml#}

  <tr class="tot-row grand {#invoicedValueRowClass#}">
    <td style="font-weight:700;color:#000000">Gefactureerde waarde</td>
    <td class="tot-val" style="font-weight:700;color:#000000">{#invoicedValue#}</td>
  </tr>

  <tr class="tot-row payment {#paymentMethodRowClass#}">
    <td></td>
    <td class="tot-val payment-detail">
      <span class="{#paymentByCardClass#}">Ontvangen via {#cardBrand#} ****{#cardLastFour#}</span>
      <span class="{#paymentOnlineClass#}">Ontvangen online</span>
      <span class="{#paymentCashClass#}">Ontvangen contant</span>
      <span class="{#paymentOtherClass#}">{#paymentMode#}</span>
    </td>
  </tr>
</table>

</div></body></html>$template$,
  $row$<tr><td>{#title#}<span class="{#postDiscountClass#}"> (Na korting {#postDiscountCurrency#} {#postDiscountAmount#})</span></td><td class="n">{#amount#}</td><td class="n">{#vatPct#}</td><td class="n">{#vatAmount#}</td><td class="n">{#total#}</td></tr>$row$,
  $totals$<tr class="tot-row"><td>{#title#}</td><td class="tot-val">{#amount#}</td></tr>$totals$,
  now(), now()
) ON CONFLICT (id) DO UPDATE SET
  template = EXCLUDED.template,
  line_item_row_template = EXCLUDED.line_item_row_template,
  totals_line_row_template = EXCLUDED.totals_line_row_template,
  merchant_id = EXCLUDED.merchant_id,
  merchant_operating_city_id = EXCLUDED.merchant_operating_city_id,
  invoice_type = EXCLUDED.invoice_type,
  language = EXCLUDED.language,
  updated_at = now();

------------------------------------------------------------
-- BAP: AggregatedCommission / ENGLISH
------------------------------------------------------------
INSERT INTO atlas_app.invoice_template (
  id, merchant_id, merchant_operating_city_id, invoice_type, language,
  template, line_item_row_template, totals_line_row_template,
  created_at, updated_at
) VALUES (
  'aa000004-1234-4000-8000-000000000004',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f',
  'f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'AggregatedCommission',
  'ENGLISH',
  $template$<!DOCTYPE html><html><head>
<meta charset='UTF-8'>
<title>Commission on App Bookings {#invoiceNumber#}</title>
<style>
* { margin:0; padding:0; box-sizing:border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif;
       color:#2d2d2d; background:#fff; padding:32px;
       -webkit-print-color-adjust:exact !important; print-color-adjust:exact !important; }
.wrap { max-width:820px; margin:0 auto; }
.header { width:100%; border-collapse:collapse; border:none; margin-bottom:28px; }
.header td { vertical-align:top; border:none; }
.header td.header-right-cell { text-align:right; border:none; }
.logo-wrap { display:block; }
.logo { height:44px; width:auto; margin-bottom:10px; display:block; }
.invoicing-period { font-size:13px; color:#2d2d2d; margin-bottom:12px; }
.agg-footer { font-size:13px; color:#2d2d2d; line-height:1.6; margin-top:24px;
              padding-top:12px; border-top:1px solid #e0e0e0; }
.inv-number { font-size:15px; font-weight:700; color:#2d2d2d; margin-bottom:10px; }
.inv-date { font-size:13px; color:#2d2d2d; margin-top:2px; }
.parties { width:100%; border-collapse:collapse; border:none; margin-bottom:24px; padding:16px 0; }
.party-right { text-align:right; vertical-align:top; border:none; }
.party-left { vertical-align:top; width:50%; border:none; }
.party-lbl-plain { font-size:13px; color:#2d2d2d; margin-bottom:4px; }
.party-trade { font-size:16px; font-weight:700; color:#1a1a1a; margin-bottom:2px; }
.party-name { font-size:15px; font-weight:600; color:#2d2d2d; margin-bottom:3px; }
.party-addr { font-size:13px; color:#2d2d2d; line-height:1.6; }
.party-tax { font-size:12px; color:#2d2d2d; margin-top:3px; }
table.line-items { width:100%; border-collapse:collapse; margin-bottom:24px; border:2px solid #2d2d2d; }
table.line-items th { font-size:13px; font-weight:700; color:#2d2d2d; text-transform:none; letter-spacing:0;
     border:1px solid #2d2d2d; padding:8px 10px; text-align:left; background:#fff; }
table.line-items td { font-size:13px; padding:9px 10px; border:1px solid #2d2d2d; color:#2d2d2d; }
table.line-items th.n, table.line-items td.n { text-align:right; }
.totals { width:400px; margin-left:auto; padding-top:24px; border-collapse:collapse; border:none; }
.tot-row { width:100%; }
.tot-row td { font-size:13px; color:#2d2d2d; padding:3px 0; border:none; }
.tot-row td.tot-val { text-align:right; padding-left:24px; white-space:nowrap; border:none; }
.tot-row.grand td { font-size:15px; font-weight:700; padding-top:12px; border:none; }
.hidden { display: none !important; }
@media print { body { padding:16px; } }
</style>
</head><body><div class='wrap'>

<table class='header'>
  <tr>
    <td>
      <div class="logo-wrap {#logoClass#}">
        <img src="{#logoUrl#}" alt="" class="logo">
      </div>
    </td>
    <td class='header-right-cell'>
      <div class='inv-number'>Invoice No. {#invoiceNumber#}</div>
      <div class='inv-date'>Date: {#issuedAt#}</div>
      <div class="inv-date {#dueDateRowClass#}">Due Date: {#dueAt#}</div>
    </td>
  </tr>
</table>

<table class="parties {#partiesTableClass#}">
  <tr>
    <td class="party-left {#leftPartyClass#}">
      <div class='party-lbl-plain'>Recipient:</div>
      <div class="party-trade {#recipientNameClass#}">{#recipientName#}</div>
      <div class="party-addr {#recipientAddressClass#}">{#recipientAddress#}</div>
      <div class="party-tax {#recipientBusinessIdClass#}">Business ID: {#recipientBusinessId#}</div>
      <div class="party-tax {#recipientVatNumberClass#}">VAT No.: {#recipientVatNumber#}</div>
    </td>
    <td class="party-right {#rightPartyClass#}">
      <div class='party-lbl-plain'>Seller:</div>
      <div class="party-trade {#sellerTradeNameFieldClass#}">{#sellerTradeNameField#}</div>
      <div class="party-name {#sellerNameFieldClass#}">{#sellerNameField#}</div>
      <div class="party-addr {#sellerAddressFieldClass#}">{#sellerAddressField#}</div>
      <div class="party-tax {#sellerBusinessIdFieldClass#}">Business ID: {#sellerBusinessIdField#}</div>
      <div class="party-tax {#sellerVatNumberFieldClass#}">VAT No.: {#sellerVatNumberField#}</div>
    </td>
  </tr>
</table>

<div class="invoicing-period {#invoicingPeriodClass#}">
  Invoicing Period: <strong>{#periodStart#} - {#periodEnd#}</strong>
</div>

<table class='line-items'>
  <thead>
    <tr>
      <th>Title</th>
      <th class="n">Quantity</th>
      <th class="n">Unit Price</th>
      <th class="n">Amount</th>
      <th class="n">VAT {#aggCommVatPct#}%</th>
      <th class="n">Total</th>
    </tr>
  </thead>
  <tbody>{#mainTableItemsHtml#}</tbody>
</table>

<table class="totals">
  <tr class="tot-row"><td>Subtotal</td><td class="tot-val">{#subtotal#}</td></tr>
  <tr class="tot-row"><td>VAT {#aggCommVatPct#}%</td><td class="tot-val">{#aggCommVatAmount#}</td></tr>
  <tr class="tot-row grand">
    <td style="font-weight:700;color:#000000">Total incl. VAT ({#currencyCode#})</td>
    <td class="tot-val" style="font-weight:700;color:#000000">{#totalIncVat#}</td>
  </tr>
</table>

<div class='agg-footer'>
  This invoice represents commission charges for services provided during the period.
  Check your <span class="{#appNameClass#}">{#appName#} </span>fleet dashboard for the most up-to-date balance information.
</div>

</div></body></html>$template$,
  $row$<tr><td>{#title#}<span class="{#periodSuffixClass#}"> ({#periodStart#} - {#periodEnd#})</span></td><td class="n">{#quantity#}</td><td class="n">{#unitPrice#}</td><td class="n">{#amount#}</td><td class="n">{#vatAmount#}</td><td class="n">{#total#}</td></tr>$row$,
  $totals$<tr class="tot-row"><td>{#title#}</td><td class="tot-val">{#amount#}</td></tr>$totals$,
  now(), now()
) ON CONFLICT (id) DO UPDATE SET
  template = EXCLUDED.template,
  line_item_row_template = EXCLUDED.line_item_row_template,
  totals_line_row_template = EXCLUDED.totals_line_row_template,
  merchant_id = EXCLUDED.merchant_id,
  merchant_operating_city_id = EXCLUDED.merchant_operating_city_id,
  invoice_type = EXCLUDED.invoice_type,
  language = EXCLUDED.language,
  updated_at = now();

------------------------------------------------------------
-- BAP: AggregatedCommission / FINNISH
------------------------------------------------------------
INSERT INTO atlas_app.invoice_template (
  id, merchant_id, merchant_operating_city_id, invoice_type, language,
  template, line_item_row_template, totals_line_row_template,
  created_at, updated_at
) VALUES (
  'aa000005-1234-4000-8000-000000000005',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f',
  'f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'AggregatedCommission',
  'FINNISH',
  $template$<!DOCTYPE html><html><head>
<meta charset='UTF-8'>
<title>Komissio sovellustilauksista {#invoiceNumber#}</title>
<style>
* { margin:0; padding:0; box-sizing:border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif;
       color:#2d2d2d; background:#fff; padding:32px;
       -webkit-print-color-adjust:exact !important; print-color-adjust:exact !important; }
.wrap { max-width:820px; margin:0 auto; }
.header { width:100%; border-collapse:collapse; border:none; margin-bottom:28px; }
.header td { vertical-align:top; border:none; }
.header td.header-right-cell { text-align:right; border:none; }
.logo-wrap { display:block; }
.logo { height:44px; width:auto; margin-bottom:10px; display:block; }
.invoicing-period { font-size:13px; color:#2d2d2d; margin-bottom:12px; }
.agg-footer { font-size:13px; color:#2d2d2d; line-height:1.6; margin-top:24px;
              padding-top:12px; border-top:1px solid #e0e0e0; }
.inv-number { font-size:15px; font-weight:700; color:#2d2d2d; margin-bottom:10px; }
.inv-date { font-size:13px; color:#2d2d2d; margin-top:2px; }
.parties { width:100%; border-collapse:collapse; border:none; margin-bottom:24px; padding:16px 0; }
.party-right { text-align:right; vertical-align:top; border:none; }
.party-left { vertical-align:top; width:50%; border:none; }
.party-lbl-plain { font-size:13px; color:#2d2d2d; margin-bottom:4px; }
.party-trade { font-size:16px; font-weight:700; color:#1a1a1a; margin-bottom:2px; }
.party-name { font-size:15px; font-weight:600; color:#2d2d2d; margin-bottom:3px; }
.party-addr { font-size:13px; color:#2d2d2d; line-height:1.6; }
.party-tax { font-size:12px; color:#2d2d2d; margin-top:3px; }
table.line-items { width:100%; border-collapse:collapse; margin-bottom:24px; border:2px solid #2d2d2d; }
table.line-items th { font-size:13px; font-weight:700; color:#2d2d2d; text-transform:none; letter-spacing:0;
     border:1px solid #2d2d2d; padding:8px 10px; text-align:left; background:#fff; }
table.line-items td { font-size:13px; padding:9px 10px; border:1px solid #2d2d2d; color:#2d2d2d; }
table.line-items th.n, table.line-items td.n { text-align:right; }
.totals { width:400px; margin-left:auto; padding-top:24px; border-collapse:collapse; border:none; }
.tot-row { width:100%; }
.tot-row td { font-size:13px; color:#2d2d2d; padding:3px 0; border:none; }
.tot-row td.tot-val { text-align:right; padding-left:24px; white-space:nowrap; border:none; }
.tot-row.grand td { font-size:15px; font-weight:700; padding-top:12px; border:none; }
.hidden { display: none !important; }
@media print { body { padding:16px; } }
</style>
</head><body><div class='wrap'>

<table class='header'>
  <tr>
    <td>
      <div class="logo-wrap {#logoClass#}">
        <img src="{#logoUrl#}" alt="" class="logo">
      </div>
    </td>
    <td class='header-right-cell'>
      <div class='inv-number'>Laskun nro. {#invoiceNumber#}</div>
      <div class='inv-date'>Päivämäärä: {#issuedAt#}</div>
      <div class="inv-date {#dueDateRowClass#}">Eräpäivä: {#dueAt#}</div>
    </td>
  </tr>
</table>

<table class="parties {#partiesTableClass#}">
  <tr>
    <td class="party-left {#leftPartyClass#}">
      <div class='party-lbl-plain'>Vastaanottaja:</div>
      <div class="party-trade {#recipientNameClass#}">{#recipientName#}</div>
      <div class="party-addr {#recipientAddressClass#}">{#recipientAddress#}</div>
      <div class="party-tax {#recipientBusinessIdClass#}">Y-tunnus: {#recipientBusinessId#}</div>
      <div class="party-tax {#recipientVatNumberClass#}">ALV-nro.: {#recipientVatNumber#}</div>
    </td>
    <td class="party-right {#rightPartyClass#}">
      <div class='party-lbl-plain'>Myyjä:</div>
      <div class="party-trade {#sellerTradeNameFieldClass#}">{#sellerTradeNameField#}</div>
      <div class="party-name {#sellerNameFieldClass#}">{#sellerNameField#}</div>
      <div class="party-addr {#sellerAddressFieldClass#}">{#sellerAddressField#}</div>
      <div class="party-tax {#sellerBusinessIdFieldClass#}">Y-tunnus: {#sellerBusinessIdField#}</div>
      <div class="party-tax {#sellerVatNumberFieldClass#}">ALV-nro.: {#sellerVatNumberField#}</div>
    </td>
  </tr>
</table>

<div class="invoicing-period {#invoicingPeriodClass#}">
  Laskutusjakso: <strong>{#periodStart#} - {#periodEnd#}</strong>
</div>

<table class='line-items'>
  <thead>
    <tr>
      <th>Nimike</th>
      <th class="n">Määrä</th>
      <th class="n">Yksikköhinta</th>
      <th class="n">Summa</th>
      <th class="n">ALV {#aggCommVatPct#}%</th>
      <th class="n">Kokonaissumma</th>
    </tr>
  </thead>
  <tbody>{#mainTableItemsHtml#}</tbody>
</table>

<table class="totals">
  <tr class="tot-row"><td>Yhteensä</td><td class="tot-val">{#subtotal#}</td></tr>
  <tr class="tot-row"><td>ALV {#aggCommVatPct#}%</td><td class="tot-val">{#aggCommVatAmount#}</td></tr>
  <tr class="tot-row grand">
    <td style="font-weight:700;color:#000000">Yhteensä sis. ALV ({#currencyCode#})</td>
    <td class="tot-val" style="font-weight:700;color:#000000">{#totalIncVat#}</td>
  </tr>
</table>

<div class='agg-footer'>
  Tämä lasku kuvaa ajanjakson aikana perittyjä komissiomaksuja. Tarkista
  <span class="{#appNameClass#}">{#appName#}-</span>kalustopaneelistasi ajantasaiset tiedot saldostasi.
</div>

</div></body></html>$template$,
  $row$<tr><td>{#title#}<span class="{#periodSuffixClass#}"> ({#periodStart#} - {#periodEnd#})</span></td><td class="n">{#quantity#}</td><td class="n">{#unitPrice#}</td><td class="n">{#amount#}</td><td class="n">{#vatAmount#}</td><td class="n">{#total#}</td></tr>$row$,
  $totals$<tr class="tot-row"><td>{#title#}</td><td class="tot-val">{#amount#}</td></tr>$totals$,
  now(), now()
) ON CONFLICT (id) DO UPDATE SET
  template = EXCLUDED.template,
  line_item_row_template = EXCLUDED.line_item_row_template,
  totals_line_row_template = EXCLUDED.totals_line_row_template,
  merchant_id = EXCLUDED.merchant_id,
  merchant_operating_city_id = EXCLUDED.merchant_operating_city_id,
  invoice_type = EXCLUDED.invoice_type,
  language = EXCLUDED.language,
  updated_at = now();

------------------------------------------------------------
-- BAP: AggregatedCommission / DUTCH
------------------------------------------------------------
INSERT INTO atlas_app.invoice_template (
  id, merchant_id, merchant_operating_city_id, invoice_type, language,
  template, line_item_row_template, totals_line_row_template,
  created_at, updated_at
) VALUES (
  'aa000006-1234-4000-8000-000000000006',
  'b9e1d4f6-7c2a-4e8b-9d3f-5a6b7c8d9e0f',
  'f9903ef6-f595-428e-b5ac-e8816cbdf979',
  'AggregatedCommission',
  'DUTCH',
  $template$<!DOCTYPE html><html><head>
<meta charset='UTF-8'>
<title>Commissie op app-boekingen {#invoiceNumber#}</title>
<style>
* { margin:0; padding:0; box-sizing:border-box; }
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Arial, sans-serif;
       color:#2d2d2d; background:#fff; padding:32px;
       -webkit-print-color-adjust:exact !important; print-color-adjust:exact !important; }
.wrap { max-width:820px; margin:0 auto; }
.header { width:100%; border-collapse:collapse; border:none; margin-bottom:28px; }
.header td { vertical-align:top; border:none; }
.header td.header-right-cell { text-align:right; border:none; }
.logo-wrap { display:block; }
.logo { height:44px; width:auto; margin-bottom:10px; display:block; }
.invoicing-period { font-size:13px; color:#2d2d2d; margin-bottom:12px; }
.agg-footer { font-size:13px; color:#2d2d2d; line-height:1.6; margin-top:24px;
              padding-top:12px; border-top:1px solid #e0e0e0; }
.inv-number { font-size:15px; font-weight:700; color:#2d2d2d; margin-bottom:10px; }
.inv-date { font-size:13px; color:#2d2d2d; margin-top:2px; }
.parties { width:100%; border-collapse:collapse; border:none; margin-bottom:24px; padding:16px 0; }
.party-right { text-align:right; vertical-align:top; border:none; }
.party-left { vertical-align:top; width:50%; border:none; }
.party-lbl-plain { font-size:13px; color:#2d2d2d; margin-bottom:4px; }
.party-trade { font-size:16px; font-weight:700; color:#1a1a1a; margin-bottom:2px; }
.party-name { font-size:15px; font-weight:600; color:#2d2d2d; margin-bottom:3px; }
.party-addr { font-size:13px; color:#2d2d2d; line-height:1.6; }
.party-tax { font-size:12px; color:#2d2d2d; margin-top:3px; }
table.line-items { width:100%; border-collapse:collapse; margin-bottom:24px; border:2px solid #2d2d2d; }
table.line-items th { font-size:13px; font-weight:700; color:#2d2d2d; text-transform:none; letter-spacing:0;
     border:1px solid #2d2d2d; padding:8px 10px; text-align:left; background:#fff; }
table.line-items td { font-size:13px; padding:9px 10px; border:1px solid #2d2d2d; color:#2d2d2d; }
table.line-items th.n, table.line-items td.n { text-align:right; }
.totals { width:400px; margin-left:auto; padding-top:24px; border-collapse:collapse; border:none; }
.tot-row { width:100%; }
.tot-row td { font-size:13px; color:#2d2d2d; padding:3px 0; border:none; }
.tot-row td.tot-val { text-align:right; padding-left:24px; white-space:nowrap; border:none; }
.tot-row.grand td { font-size:15px; font-weight:700; padding-top:12px; border:none; }
.hidden { display: none !important; }
@media print { body { padding:16px; } }
</style>
</head><body><div class='wrap'>

<table class='header'>
  <tr>
    <td>
      <div class="logo-wrap {#logoClass#}">
        <img src="{#logoUrl#}" alt="" class="logo">
      </div>
    </td>
    <td class='header-right-cell'>
      <div class='inv-number'>Factuurnr. {#invoiceNumber#}</div>
      <div class='inv-date'>Datum: {#issuedAt#}</div>
      <div class="inv-date {#dueDateRowClass#}">Vervaldatum: {#dueAt#}</div>
    </td>
  </tr>
</table>

<table class="parties {#partiesTableClass#}">
  <tr>
    <td class="party-left {#leftPartyClass#}">
      <div class='party-lbl-plain'>Ontvanger:</div>
      <div class="party-trade {#recipientNameClass#}">{#recipientName#}</div>
      <div class="party-addr {#recipientAddressClass#}">{#recipientAddress#}</div>
      <div class="party-tax {#recipientBusinessIdClass#}">Bedrijfs-ID: {#recipientBusinessId#}</div>
      <div class="party-tax {#recipientVatNumberClass#}">BTW-nr.: {#recipientVatNumber#}</div>
    </td>
    <td class="party-right {#rightPartyClass#}">
      <div class='party-lbl-plain'>Verkoper:</div>
      <div class="party-trade {#sellerTradeNameFieldClass#}">{#sellerTradeNameField#}</div>
      <div class="party-name {#sellerNameFieldClass#}">{#sellerNameField#}</div>
      <div class="party-addr {#sellerAddressFieldClass#}">{#sellerAddressField#}</div>
      <div class="party-tax {#sellerBusinessIdFieldClass#}">Bedrijfs-ID: {#sellerBusinessIdField#}</div>
      <div class="party-tax {#sellerVatNumberFieldClass#}">BTW-nr.: {#sellerVatNumberField#}</div>
    </td>
  </tr>
</table>

<div class="invoicing-period {#invoicingPeriodClass#}">
  Factureringsperiode: <strong>{#periodStart#} - {#periodEnd#}</strong>
</div>

<table class='line-items'>
  <thead>
    <tr>
      <th>Omschrijving</th>
      <th class="n">Aantal</th>
      <th class="n">Eenheidsprijs</th>
      <th class="n">Bedrag</th>
      <th class="n">BTW {#aggCommVatPct#}%</th>
      <th class="n">Totaal</th>
    </tr>
  </thead>
  <tbody>{#mainTableItemsHtml#}</tbody>
</table>

<table class="totals">
  <tr class="tot-row"><td>Subtotaal</td><td class="tot-val">{#subtotal#}</td></tr>
  <tr class="tot-row"><td>BTW {#aggCommVatPct#}%</td><td class="tot-val">{#aggCommVatAmount#}</td></tr>
  <tr class="tot-row grand">
    <td style="font-weight:700;color:#000000">Totaal incl. BTW ({#currencyCode#})</td>
    <td class="tot-val" style="font-weight:700;color:#000000">{#totalIncVat#}</td>
  </tr>
</table>

<div class='agg-footer'>
  Deze factuur vertegenwoordigt commissiekosten voor tijdens de periode geleverde diensten. Raadpleeg uw
  <span class="{#appNameClass#}">{#appName#}-</span>vlootdashboard voor actuele saldo-informatie.
</div>

</div></body></html>$template$,
  $row$<tr><td>{#title#}<span class="{#periodSuffixClass#}"> ({#periodStart#} - {#periodEnd#})</span></td><td class="n">{#quantity#}</td><td class="n">{#unitPrice#}</td><td class="n">{#amount#}</td><td class="n">{#vatAmount#}</td><td class="n">{#total#}</td></tr>$row$,
  $totals$<tr class="tot-row"><td>{#title#}</td><td class="tot-val">{#amount#}</td></tr>$totals$,
  now(), now()
) ON CONFLICT (id) DO UPDATE SET
  template = EXCLUDED.template,
  line_item_row_template = EXCLUDED.line_item_row_template,
  totals_line_row_template = EXCLUDED.totals_line_row_template,
  merchant_id = EXCLUDED.merchant_id,
  merchant_operating_city_id = EXCLUDED.merchant_operating_city_id,
  invoice_type = EXCLUDED.invoice_type,
  language = EXCLUDED.language,
  updated_at = now();
