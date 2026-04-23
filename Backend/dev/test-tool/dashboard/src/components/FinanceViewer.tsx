/**
 * FinanceViewer — Full domain visualization dashboard for finance data.
 * Shows wallets, ledger timeline, invoices — all from DB, no filters needed.
 */

import React, { useState, useEffect, useCallback } from 'react';
import './FinanceViewer.css';

const API = 'http://localhost:7082';
type Side = 'bap' | 'bpp' | 'earnings';

async function fetchJson(url: string) {
  const resp = await fetch(url);
  return resp.json();
}

async function postJson(url: string, body?: any) {
  const resp = await fetch(url, { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: body ? JSON.stringify(body) : undefined });
  return resp.json();
}

// ──────────────────────────────────────────────────────────────────────────
// Print-combined: render both BAP and BPP dashboards into a single
// self-contained HTML document and open it in a new window for native
// browser print → "Save as PDF". Keeps styling simple / inline so no external
// CSS is needed in the print window.
// ──────────────────────────────────────────────────────────────────────────
function esc(s: any): string {
  if (s == null) return '';
  return String(s)
    .replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
}

function fmtPrint(amt: any, cur?: string): string {
  if (amt == null) return '-';
  const s = Number(amt).toFixed(2);
  return cur ? `${cur} ${s}` : s;
}

function accountLabelPrint(type: string, party: string): string {
  const map: Record<string, Record<string, string>> = {
    Asset: { BUYER: 'Buyer Asset (Payment Receivable)', SELLER: 'Platform Asset', DEFAULT: `${party} Asset` },
    External: { BUYER: 'Payment Gateway', DEFAULT: `${party} External` },
    Liability: { DRIVER: 'Driver Payout (Owed)', FLEET_OWNER: 'Fleet Payout (Owed)', RIDER: 'Rider Liability', GOVERNMENT_INDIRECT: 'GST/VAT Payable', GOVERNMENT_DIRECT: 'TDS Payable', DEFAULT: `${party} Liability` },
    Control: { DRIVER: 'Driver Cash Earnings (Tracked)', FLEET_OWNER: 'Fleet Cash Earnings (Tracked)', BUYER: 'Buyer Cash Tracker', DEFAULT: `${party} Cash Tracker` },
    Expense: { BUYER: 'Discount Expense', DRIVER: 'Driver Expense', GOVERNMENT_INDIRECT: 'GST/VAT Expense (Recoverable from Govt)', DEFAULT: `${party} Expense` },
    Revenue: { SELLER: 'Platform Commission', DEFAULT: `${party} Revenue` },
  };
  return map[type]?.[party] || map[type]?.DEFAULT || `${type} (${party})`;
}

function renderSideHtml(side: 'BAP' | 'BPP', data: any): string {
  const notVoided = (e: any) => (e?.status ?? '').toString().toUpperCase() !== 'VOIDED';
  const accounts = data?.accounts || [];
  const timeline = data?.timeline || [];
  const summary = data?.summary || {};

  // Per-account totals from non-voided timeline entries
  const tot: Record<string, { credits: number; debits: number }> = {};
  const bump = (k: string, credits: number, debits: number) => {
    if (!tot[k]) tot[k] = { credits: 0, debits: 0 };
    tot[k].credits += credits;
    tot[k].debits += debits;
  };
  for (const g of timeline) for (const e of (g.entries || [])) {
    if (!notVoided(e)) continue;
    const amt = Math.abs(Number(e.amount) || 0);
    if (e.to_type && e.to_counterparty) bump(`${e.to_type}|${e.to_counterparty}`, amt, 0);
    if (e.from_type && e.from_counterparty) bump(`${e.from_type}|${e.from_counterparty}`, 0, amt);
  }
  const visibleAccounts = accounts.filter((a: any) => {
    const t = tot[`${a.account_type}|${a.counterparty_type}`];
    return t && (t.credits > 0 || t.debits > 0);
  });

  const visibleGroups = timeline
    .map((g: any) => ({ ...g, entries: (g.entries || []).filter(notVoided) }))
    .filter((g: any) => g.entries.length > 0 || (g.invoice && g.invoice.status !== 'Voided'));

  const acctRows = visibleAccounts.map((a: any) => {
    const t = tot[`${a.account_type}|${a.counterparty_type}`] || { credits: 0, debits: 0 };
    const bal = Number(a.balance) || 0;
    return `<tr>
      <td>${esc(accountLabelPrint(a.account_type, a.counterparty_type))}</td>
      <td>${esc(a.account_type)}</td>
      <td>${esc(a.counterparty_type)}<br/><span class="mono tiny">${esc(a.counterparty_id?.substring(0, 20) || '')}</span></td>
      <td class="r ${bal < 0 ? 'neg' : ''}">${esc(fmtPrint(a.balance, a.currency))}</td>
      <td class="r pos">+${esc(fmtPrint(t.credits))}</td>
      <td class="r neg">-${esc(fmtPrint(t.debits))}</td>
    </tr>`;
  }).join('');

  const invoiceTable = (inv: any) => {
    let items: any[] = [];
    try { items = typeof inv.line_items === 'string' ? JSON.parse(inv.line_items) : (inv.line_items || []); } catch { items = []; }
    const rows = items.map((li: any) => `<tr class="${Number(li.lineTotal) < 0 ? 'neg-row' : ''}">
      <td>${esc(li.description)}${li.isExternalCharge ? ' *' : ''}</td>
      <td class="r">${esc(li.quantity)}</td>
      <td class="r">${esc(fmtPrint(li.unitPrice))}</td>
      <td class="r ${Number(li.lineTotal) < 0 ? 'neg' : ''}">${esc(fmtPrint(li.lineTotal))}</td>
    </tr>`).join('');
    const tax = (Number(inv.total_amount) || 0) - (Number(inv.subtotal) || 0);
    return `<div class="inv">
      <div class="inv-head">
        <strong>Invoice ${esc(inv.invoice_number || '')}</strong>
        <span class="pill">${esc(inv.status)}</span>
        <span>${esc(inv.invoice_type)}</span>
        <span class="tiny">${esc(new Date(inv.created_at).toLocaleString())}</span>
      </div>
      <div class="tiny">To: ${esc(inv.issued_to_name || inv.issued_to_id?.substring(0, 24) || '')} · From: ${esc(inv.issued_by_name || '-')}</div>
      <table class="inv-tbl">
        <thead><tr><th>Description</th><th class="r">Qty</th><th class="r">Price</th><th class="r">Total</th></tr></thead>
        <tbody>${rows}</tbody>
        <tfoot>
          <tr><td colspan="3" class="r">Subtotal</td><td class="r">${esc(fmtPrint(inv.subtotal))}</td></tr>
          <tr><td colspan="3" class="r">Tax</td><td class="r">${esc(fmtPrint(tax))}</td></tr>
          <tr class="tot"><td colspan="3" class="r">Total</td><td class="r">${esc(fmtPrint(inv.total_amount, inv.currency))}</td></tr>
        </tfoot>
      </table>
    </div>`;
  };

  const groupBlocks = visibleGroups.map((g: any) => {
    const entryRows = (g.entries || []).map((le: any) => `<tr>
      <td class="tiny">${esc(new Date(le.created_at).toLocaleTimeString())}</td>
      <td>${esc(le.reference_type)}</td>
      <td class="tiny">${esc(le.from_type || '?')}${le.from_counterparty ? ` (${esc(le.from_counterparty.substring(0, 12))})` : ''} → ${esc(le.to_type || '?')}${le.to_counterparty ? ` (${esc(le.to_counterparty.substring(0, 12))})` : ''}</td>
      <td class="r ${Number(le.amount) < 0 ? 'neg' : ''}">${esc(fmtPrint(le.amount, le.currency))}</td>
      <td>${esc(le.status)}</td>
    </tr>`).join('');
    const inv = g.invoice && g.invoice.status !== 'Voided' ? g.invoice : null;
    return `<div class="grp">
      <div class="grp-head"><span class="pill">${esc(g.primary_type || '')}</span>
        <span class="mono tiny">${esc(g.reference_id || '')}</span>
        <span class="tiny">${esc(new Date(g.earliest).toLocaleString())}</span>
        <span class="tiny">${(g.entries || []).length} entries</span>
      </div>
      <table class="ent-tbl">
        <thead><tr><th>Time</th><th>Ref</th><th>Flow</th><th class="r">Amount</th><th>Status</th></tr></thead>
        <tbody>${entryRows}</tbody>
      </table>
      ${inv ? invoiceTable(inv) : ''}
    </div>`;
  }).join('');

  return `<section class="side">
    <h2>${side} — ${side === 'BAP' ? 'Buyer App (Rider)' : 'Seller App (Driver)'}</h2>
    <div class="cards">
      <div class="card"><div class="lbl">Total Entries</div><div class="val">${timeline.reduce((s: number, g: any) => s + (g.entries || []).filter(notVoided).length, 0)}</div></div>
      <div class="card"><div class="lbl">Invoices</div><div class="val">${esc(summary.totalInvoices || 0)}</div></div>
      <div class="card"><div class="lbl">Active Invoices</div><div class="val">${esc(summary.activeInvoices || 0)}</div></div>
      <div class="card"><div class="lbl">Accounts</div><div class="val">${visibleAccounts.length}</div></div>
    </div>
    <h3>Accounts</h3>
    <table class="acct-tbl">
      <thead><tr><th>Account</th><th>Type</th><th>Party</th><th class="r">Balance</th><th class="r">Credits</th><th class="r">Debits</th></tr></thead>
      <tbody>${acctRows || `<tr><td colspan="6" class="tiny">No accounts</td></tr>`}</tbody>
    </table>
    <h3>Ledger Timeline <span class="tiny">(${visibleGroups.length} groups)</span></h3>
    ${groupBlocks || `<div class="tiny">No ledger entries</div>`}
  </section>`;
}

function buildPrintHtml(bap: any, bpp: any): string {
  const now = new Date().toLocaleString();
  return `<!doctype html><html><head><meta charset="utf-8"/><title>Finance Ledger — BAP + BPP</title>
<style>
  * { box-sizing: border-box; }
  body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; color: #111; margin: 16px; font-size: 11px; }
  h1 { font-size: 18px; margin: 0 0 4px; }
  h2 { font-size: 15px; margin: 18px 0 6px; border-bottom: 2px solid #333; padding-bottom: 3px; }
  h3 { font-size: 13px; margin: 12px 0 4px; color: #333; }
  .ts { color: #666; font-size: 10px; margin-bottom: 10px; }
  .cards { display: flex; gap: 8px; margin: 6px 0; }
  .card { flex: 1; border: 1px solid #ccc; padding: 4px 8px; border-radius: 4px; }
  .lbl { font-size: 9px; color: #666; text-transform: uppercase; }
  .val { font-size: 16px; font-weight: bold; }
  table { width: 100%; border-collapse: collapse; font-size: 10px; }
  th, td { padding: 3px 5px; text-align: left; border-bottom: 1px solid #e5e5e5; vertical-align: top; }
  th { background: #f4f4f4; font-weight: 600; }
  .r { text-align: right; }
  .pos { color: #0a7b2c; }
  .neg { color: #b3261e; }
  .neg-row td { color: #b3261e; }
  .tiny { font-size: 9px; color: #555; }
  .mono { font-family: ui-monospace, SFMono-Regular, Menlo, monospace; }
  .pill { background: #eaeaea; padding: 1px 5px; border-radius: 3px; font-size: 9px; }
  .grp { border: 1px solid #ddd; border-radius: 4px; padding: 6px 8px; margin-bottom: 8px; }
  .grp-head { display: flex; gap: 8px; align-items: center; margin-bottom: 4px; }
  .ent-tbl { margin-top: 2px; }
  .inv { margin-top: 6px; padding: 6px 8px; border: 1px dashed #aaa; border-radius: 3px; background: #fafafa; }
  .inv-head { display: flex; gap: 8px; align-items: center; margin-bottom: 3px; }
  .inv-tbl { margin-top: 3px; }
  .inv-tbl .tot td { font-weight: bold; border-top: 1px solid #999; }
  .side { page-break-after: always; }
  .side:last-child { page-break-after: auto; }
  @media print {
    body { margin: 8px; }
    .no-print { display: none; }
  }
</style></head><body>
  <h1>Finance Ledger — BAP + BPP</h1>
  <div class="ts">Generated ${esc(now)}</div>
  ${renderSideHtml('BAP', bap)}
  ${renderSideHtml('BPP', bpp)}
  <script>setTimeout(function(){ window.print(); }, 400);</script>
</body></html>`;
}

export const FinanceViewer: React.FC = () => {
  const [side, setSide] = useState<Side>('bpp');
  const [data, setData] = useState<any>(null);
  const [earnings, setEarnings] = useState<any>(null);
  const [driverFilter, setDriverFilter] = useState<string>('');
  const [loading, setLoading] = useState(false);
  const [selectedInvoice, setSelectedInvoice] = useState<any>(null);
  const [selectedEntry, setSelectedEntry] = useState<any>(null);
  const [expandedRefs, setExpandedRefs] = useState<Set<string>>(new Set());
  const [expandedAccts, setExpandedAccts] = useState<Set<string>>(new Set());

  const load = useCallback(async () => {
    setLoading(true);
    if (side === 'earnings') {
      const q = driverFilter.trim() ? `?counterpartyId=${encodeURIComponent(driverFilter.trim())}` : '';
      const d = await fetchJson(`${API}/api/finance/earnings${q}`);
      setEarnings(d);
    } else {
      const d = await fetchJson(`${API}/api/finance/dashboard?side=${side}`);
      setData(d);
    }
    setLoading(false);
  }, [side, driverFilter]);

  useEffect(() => { load(); }, [load]);

  const clearAll = async () => {
    if (!window.confirm(`Delete ALL finance data on ${side.toUpperCase()}? This cannot be undone.`)) return;
    await postJson(`${API}/api/finance/clear-all`, { side });
    setData(null);
    setSelectedInvoice(null);
    setSelectedEntry(null);
    setExpandedRefs(new Set());
    load();
  };

  const printCombined = async () => {
    setLoading(true);
    try {
      const [bap, bpp] = await Promise.all([
        fetchJson(`${API}/api/finance/dashboard?side=bap`),
        fetchJson(`${API}/api/finance/dashboard?side=bpp`),
      ]);
      const html = buildPrintHtml(bap, bpp);
      const w = window.open('', '_blank');
      if (!w) {
        window.alert('Popup blocked. Allow popups for this site and try again.');
        return;
      }
      w.document.open();
      w.document.write(html);
      w.document.close();
      w.focus();
    } finally {
      setLoading(false);
    }
  };

  const toggleRef = (refId: string) => {
    setExpandedRefs(prev => {
      const next = new Set(prev);
      next.has(refId) ? next.delete(refId) : next.add(refId);
      return next;
    });
  };

  const toggleAcct = (acctId: string) => {
    setExpandedAccts(prev => {
      const next = new Set(prev);
      next.has(acctId) ? next.delete(acctId) : next.add(acctId);
      return next;
    });
  };

  const accountLabel = (type: string, party: string) => {
    const map: Record<string, Record<string, string>> = {
      Asset:    { BUYER: 'Buyer Asset (Payment Receivable)', SELLER: 'Platform Asset', DEFAULT: `${party} Asset` },
      External: { BUYER: 'Payment Gateway', DEFAULT: `${party} External` },
      Liability:{ DRIVER: 'Driver Payout (Owed)', FLEET_OWNER: 'Fleet Payout (Owed)', RIDER: 'Rider Liability', GOVERNMENT_INDIRECT: 'GST/VAT Payable', GOVERNMENT_DIRECT: 'TDS Payable', DEFAULT: `${party} Liability` },
      Expense:  { BUYER: 'Discount Expense', DRIVER: 'Driver Expense', GOVERNMENT_INDIRECT: 'GST/VAT Expense (Recoverable from Govt)', DEFAULT: `${party} Expense` },
      Revenue:  { SELLER: 'Platform Commission', DEFAULT: `${party} Revenue` },
      Control:  { DRIVER: 'Driver Cash Earnings (Tracked)', FLEET_OWNER: 'Fleet Cash Earnings (Tracked)', BUYER: 'Buyer Cash Tracker', DEFAULT: `${party} Cash Tracker` },
    };
    return map[type]?.[party] || map[type]?.DEFAULT || `${type} (${party})`;
  };

  const fmt = (amt: number, cur?: string) => {
    if (amt == null) return '-';
    const s = Number(amt).toFixed(2);
    return cur ? `${cur} ${s}` : s;
  };

  if (loading && side !== 'earnings' && !data) return <div className="fv"><div className="fv-loading">Loading finance data...</div></div>;
  if (loading && side === 'earnings' && !earnings) return <div className="fv"><div className="fv-loading">Loading earnings...</div></div>;

  const accounts = data?.accounts || [];
  const timeline = data?.timeline || [];
  const summary = data?.summary || {};

  // UI-side filter: exclude VOIDED entries from all views (accounts breakdown
  // + ledger timeline). Keeps the backend raw; dashboard just hides noise.
  const isNotVoided = (e: any) => (e.status ?? '').toString().toUpperCase() !== 'VOIDED';

  // Per-account per-referenceType credit/debit breakdown, derived from the
  // timeline so no backend change is needed. An entry going _into_ an account
  // counts as a credit for that account; the counter-leg counts as a debit.
  const acctRefBreakdown: Record<string, Record<string, { credits: number; debits: number }>> = {};
  // Per-account totals derived from the filtered timeline — replaces the
  // backend's total_credits/total_debits which include VOIDED entries.
  const acctTotals: Record<string, { credits: number; debits: number }> = {};
  const ensureBD = (k: string, rt: string) => {
    if (!acctRefBreakdown[k]) acctRefBreakdown[k] = {};
    if (!acctRefBreakdown[k][rt]) acctRefBreakdown[k][rt] = { credits: 0, debits: 0 };
    return acctRefBreakdown[k][rt];
  };
  const ensureTot = (k: string) => {
    if (!acctTotals[k]) acctTotals[k] = { credits: 0, debits: 0 };
    return acctTotals[k];
  };
  for (const g of timeline) {
    for (const e of (g.entries || []) as any[]) {
      if (!isNotVoided(e)) continue;
      const amt = Math.abs(Number(e.amount) || 0);
      const rt = e.reference_type || '?';
      if (e.to_type && e.to_counterparty) {
        const k = `${e.to_type}|${e.to_counterparty}`;
        ensureBD(k, rt).credits += amt;
        ensureTot(k).credits += amt;
      }
      if (e.from_type && e.from_counterparty) {
        const k = `${e.from_type}|${e.from_counterparty}`;
        ensureBD(k, rt).debits += amt;
        ensureTot(k).debits += amt;
      }
    }
  }
  // Hide accounts whose only activity was voided (no non-voided flows).
  const visibleAccounts = accounts.filter((a: any) => {
    const k = `${a.account_type}|${a.counterparty_type}`;
    const t = acctTotals[k];
    return t && (t.credits > 0 || t.debits > 0);
  });

  return (
    <div className="fv">
      {/* Top bar */}
      <div className="fv-topbar">
        <div className="fv-title">Finance Dashboard</div>
        <div className="fv-toggle">
          <button className={side === 'bap' ? 'on' : ''} onClick={() => setSide('bap')}>BAP (Rider)</button>
          <button className={side === 'bpp' ? 'on' : ''} onClick={() => setSide('bpp')}>BPP (Driver)</button>
          <button className={side === 'earnings' ? 'on' : ''} onClick={() => setSide('earnings')}>BPP Earnings</button>
        </div>
        <div className="fv-actions">
          <button className="fv-btn-refresh" onClick={load}>Refresh</button>
          <button className="fv-btn-refresh" onClick={printCombined} title="Open BAP + BPP ledgers in a print view (Save as PDF)">Print PDF</button>
          <button className="fv-btn-danger" onClick={clearAll}>Clear All</button>
        </div>
      </div>

      {/* BPP Earnings view — replaces accounts + timeline when active */}
      {side === 'earnings' ? (
        <EarningsView
          data={earnings}
          driverFilter={driverFilter}
          setDriverFilter={setDriverFilter}
          onApplyFilter={load}
          fmt={fmt}
        />
      ) : (
      <>
      {/* Summary cards */}
      <div className="fv-cards">
        <div className="fv-card fv-card-blue">
          <div className="fv-card-label">Total Entries</div>
          <div className="fv-card-value">{timeline.reduce((s: number, g: any) => s + (g.entries || []).filter(isNotVoided).length, 0)}</div>
        </div>
        <div className="fv-card fv-card-green">
          <div className="fv-card-label">Invoices</div>
          <div className="fv-card-value">{summary.totalInvoices || 0}</div>
        </div>
        <div className="fv-card fv-card-yellow">
          <div className="fv-card-label">Active Invoices</div>
          <div className="fv-card-value">{summary.activeInvoices || 0}</div>
        </div>
        <div className="fv-card fv-card-purple">
          <div className="fv-card-label">Accounts</div>
          <div className="fv-card-value">{visibleAccounts.length}</div>
        </div>
      </div>

      {/* Wallet balances */}
      <div className="fv-section">
        <h3>Accounts</h3>
        <table className="fv-accounts-table">
          <thead>
            <tr>
              <th>Account</th>
              <th>Type</th>
              <th>Party</th>
              <th className="r">Balance</th>
              <th className="r">Credits</th>
              <th className="r">Debits</th>
            </tr>
          </thead>
          <tbody>
            {visibleAccounts.map((a: any) => {
              const label = accountLabel(a.account_type, a.counterparty_type);
              const bdKey = `${a.account_type}|${a.counterparty_type}`;
              const refs = acctRefBreakdown[bdKey] || {};
              const refEntries = Object.entries(refs).sort((x, y) =>
                (y[1].credits + y[1].debits) - (x[1].credits + x[1].debits));
              const hasBreakdown = refEntries.length > 0;
              const isOpen = expandedAccts.has(a.id);
              const tot = acctTotals[bdKey] || { credits: 0, debits: 0 };
              return (
                <React.Fragment key={a.id}>
                  <tr
                    className={hasBreakdown ? 'fv-acct-row clickable' : 'fv-acct-row'}
                    onClick={() => hasBreakdown && toggleAcct(a.id)}
                    style={hasBreakdown ? { cursor: 'pointer' } : undefined}
                  >
                    <td>
                      {hasBreakdown && (
                        <span className="fv-tl-arrow" style={{ marginRight: 4 }}>
                          {isOpen ? '\u25BE' : '\u25B8'}
                        </span>
                      )}
                      <span className={`fv-acct-badge fv-acct-${a.account_type?.toLowerCase()}`}>{label}</span>
                    </td>
                    <td className="fv-acct-type">{a.account_type}</td>
                    <td className="fv-acct-party">{a.counterparty_type}<br/><span className="fv-acct-id">{a.counterparty_id?.substring(0, 12)}</span></td>
                    <td className={`r ${(a.balance || 0) < 0 ? 'neg' : ''}`}>{fmt(a.balance, a.currency)}</td>
                    <td className="r fv-credit-col">+{fmt(tot.credits)}</td>
                    <td className="r fv-debit-col">-{fmt(tot.debits)}</td>
                  </tr>
                  {isOpen && refEntries.map(([rt, v]) => (
                    <tr key={`${a.id}-${rt}`} className="fv-acct-ref-row">
                      <td style={{ paddingLeft: 40, opacity: 0.85 }}>
                        <span className="fv-tl-e-ref">{rt}</span>
                      </td>
                      <td colSpan={2} style={{ opacity: 0.6, fontSize: '0.9em' }}>by reference type</td>
                      <td></td>
                      <td className="r fv-credit-col">{v.credits > 0 ? `+${fmt(v.credits)}` : fmt(0)}</td>
                      <td className="r fv-debit-col">{v.debits > 0 ? `-${fmt(v.debits)}` : fmt(0)}</td>
                    </tr>
                  ))}
                </React.Fragment>
              );
            })}
          </tbody>
        </table>
        {visibleAccounts.length === 0 && <div className="fv-empty-msg">No accounts yet</div>}
      </div>

      {/* Ledger timeline grouped by reference_id — VOIDED entries filtered out in UI */}
      {(() => {
        const visibleGroups = timeline
          .map((group: any) => ({ ...group, entries: (group.entries || []).filter(isNotVoided) }))
          .filter((group: any) => (group.entries || []).length > 0 || (group.invoice && group.invoice.status !== 'Voided'));
        return (
          <div className="fv-section fv-section-grow">
            <h3>Ledger Timeline <span className="fv-count">({visibleGroups.length} groups)</span></h3>
            <div className="fv-timeline">
              {visibleGroups.map((group: any) => {
                const isOpen = expandedRefs.has(group.reference_id);
                const entries = group.entries || [];
                const inv = group.invoice && group.invoice.status !== 'Voided' ? group.invoice : null;
                const totalIn = entries.reduce((s: number, e: any) => s + (e.amount > 0 ? e.amount : 0), 0);
                const totalOut = entries.reduce((s: number, e: any) => s + (e.amount < 0 ? e.amount : 0), 0);

                return (
                  <div key={group.reference_id} className="fv-tl-group">
                    <div className="fv-tl-header" onClick={() => toggleRef(group.reference_id)}>
                      <span className="fv-tl-arrow">{isOpen ? '\u25BE' : '\u25B8'}</span>
                      <span className={`fv-tl-type fv-tl-type-${group.primary_type?.toLowerCase()}`}>{group.primary_type}</span>
                      <span className="fv-tl-refid">{group.reference_id?.substring(0, 20)}...</span>
                      <span className="fv-tl-time">{new Date(group.earliest).toLocaleString()}</span>
                      <span className="fv-tl-stats">
                        <span className="fv-tl-in">+{fmt(totalIn)}</span>
                        <span className="fv-tl-out">{fmt(totalOut)}</span>
                        <span className="fv-tl-entries">{entries.length} entries</span>
                      </span>
                      {inv && (
                        <button className="fv-tl-inv-btn" onClick={e => { e.stopPropagation(); setSelectedInvoice(inv); }}>
                          Invoice
                        </button>
                      )}
                    </div>

                    {isOpen && (
                      <div className="fv-tl-entries">
                        {entries.map((le: any) => (
                          <div key={le.id} className="fv-tl-entry" onClick={() => setSelectedEntry(le)}>
                            <span className="fv-tl-e-time">{new Date(le.created_at).toLocaleTimeString()}</span>
                            <span className="fv-tl-e-ref">{le.reference_type}</span>
                            <span className="fv-tl-e-flow">
                              <span className="fv-tl-e-from">{le.from_type || '?'}{le.from_counterparty ? ` (${le.from_counterparty})` : ''}</span>
                              <span className="fv-tl-e-arrow">{'\u2192'}</span>
                              <span className="fv-tl-e-to">{le.to_type || '?'}{le.to_counterparty ? ` (${le.to_counterparty})` : ''}</span>
                            </span>
                            <span className={`fv-tl-e-amt ${le.amount < 0 ? 'neg' : ''}`}>{fmt(le.amount, le.currency)}</span>
                            <span className={`fv-tl-e-status fv-s-${le.status?.toLowerCase()}`}>{le.status}</span>
                          </div>
                        ))}
                      </div>
                    )}
                  </div>
                );
              })}
              {visibleGroups.length === 0 && <div className="fv-empty-msg">No ledger entries yet. Run a ride flow to generate data.</div>}
            </div>
          </div>
        );
      })()}
      </>
      )}

      {/* Invoice popup */}
      {selectedInvoice && (
        <div className="fv-overlay" onClick={() => setSelectedInvoice(null)}>
          <div className="fv-popup" onClick={e => e.stopPropagation()}>
            <div className="fv-popup-head">
              <h3>Invoice {selectedInvoice.invoice_number}</h3>
              <button onClick={() => setSelectedInvoice(null)}>{'\u2715'}</button>
            </div>
            <div className="fv-popup-meta">
              <span className={`fv-s-${selectedInvoice.status?.toLowerCase()}`}>{selectedInvoice.status}</span>
              <span>{selectedInvoice.invoice_type}</span>
              <span>{selectedInvoice.tax_breakdown ? 'VAT' : 'GST'}</span>
              <span>{new Date(selectedInvoice.created_at).toLocaleString()}</span>
            </div>
            <div className="fv-popup-parties">
              <div>Issued to: {selectedInvoice.issued_to_name || selectedInvoice.issued_to_id?.substring(0, 20)}</div>
              <div>Issued by: {selectedInvoice.issued_by_name || '-'}</div>
            </div>
            <table className="fv-popup-table">
              <thead><tr><th>Description</th><th>Qty</th><th>Price</th><th>Total</th></tr></thead>
              <tbody>
                {(() => {
                  try {
                    const items = typeof selectedInvoice.line_items === 'string'
                      ? JSON.parse(selectedInvoice.line_items) : selectedInvoice.line_items || [];
                    return items.map((li: any, i: number) => (
                      <tr key={i} className={li.lineTotal < 0 ? 'neg-row' : ''}>
                        <td>{li.description}{li.isExternalCharge ? ' *' : ''}</td>
                        <td>{li.quantity}</td>
                        <td className="r">{fmt(li.unitPrice)}</td>
                        <td className={`r ${li.lineTotal < 0 ? 'neg' : ''}`}>{fmt(li.lineTotal)}</td>
                      </tr>
                    ));
                  } catch { return <tr><td colSpan={4}>Unable to parse</td></tr>; }
                })()}
              </tbody>
              <tfoot>
                <tr><td colSpan={3}>Subtotal</td><td className="r">{fmt(selectedInvoice.subtotal)}</td></tr>
                <tr><td colSpan={3}>Tax</td><td className="r">{fmt(selectedInvoice.total_amount - selectedInvoice.subtotal)}</td></tr>
                <tr className="fv-popup-total"><td colSpan={3}>Total</td><td className="r">{fmt(selectedInvoice.total_amount, selectedInvoice.currency)}</td></tr>
              </tfoot>
            </table>
          </div>
        </div>
      )}

      {/* Entry detail popup */}
      {selectedEntry && (
        <div className="fv-overlay" onClick={() => setSelectedEntry(null)}>
          <div className="fv-popup fv-popup-sm" onClick={e => e.stopPropagation()}>
            <div className="fv-popup-head">
              <h3>Ledger Entry</h3>
              <button onClick={() => setSelectedEntry(null)}>{'\u2715'}</button>
            </div>
            <div className="fv-detail-grid">
              <div className="fv-dg-label">ID</div><div className="fv-dg-val mono">{selectedEntry.id}</div>
              <div className="fv-dg-label">Reference</div><div className="fv-dg-val">{selectedEntry.reference_type} / {selectedEntry.reference_id}</div>
              <div className="fv-dg-label">Amount</div><div className={`fv-dg-val ${selectedEntry.amount < 0 ? 'neg' : ''}`}>{fmt(selectedEntry.amount, selectedEntry.currency)}</div>
              <div className="fv-dg-label">From</div><div className="fv-dg-val">{selectedEntry.from_type} ({selectedEntry.from_counterparty})</div>
              <div className="fv-dg-label">To</div><div className="fv-dg-val">{selectedEntry.to_type} ({selectedEntry.to_counterparty})</div>
              <div className="fv-dg-label">Status</div><div className="fv-dg-val"><span className={`fv-s-${selectedEntry.status?.toLowerCase()}`}>{selectedEntry.status}</span></div>
              <div className="fv-dg-label">Type</div><div className="fv-dg-val">{selectedEntry.entry_type}</div>
              <div className="fv-dg-label">Created</div><div className="fv-dg-val">{new Date(selectedEntry.created_at).toLocaleString()}</div>
            </div>
          </div>
        </div>
      )}
    </div>
  );
};

// ──────────────────────────────────────────────────────────────────────────
// EarningsView — mirrors driver-app `GET /wallet/transactions` response:
// additions (credits to DRIVER Asset) and deductions (debits from it),
// grouped by ledger ref type with friendly labels. Driver-id filter optional;
// when blank, the context-api aggregates platform-wide across every driver.
// ──────────────────────────────────────────────────────────────────────────
const EarningsView: React.FC<{
  data: any;
  driverFilter: string;
  setDriverFilter: (s: string) => void;
  onApplyFilter: () => void;
  fmt: (amt: number, cur?: string) => string;
}> = ({ data, driverFilter, setDriverFilter, onApplyFilter, fmt }) => {
  const additions = data?.additions || { totalAmount: 0, items: [] };
  const deductions = data?.deductions || { totalAmount: 0, items: [] };
  const cur = data?.currency || undefined;
  const balance = data?.balance;
  const net = data?.net || 0;

  return (
    <>
      <div className="fv-cards" style={{ alignItems: 'stretch' }}>
        <div className="fv-card fv-card-blue" style={{ flex: 2 }}>
          <div className="fv-card-label">Driver Filter (counterpartyId)</div>
          <div style={{ display: 'flex', gap: 6, marginTop: 4 }}>
            <input
              placeholder="Leave blank for all drivers"
              value={driverFilter}
              onChange={e => setDriverFilter(e.target.value)}
              onKeyDown={e => { if (e.key === 'Enter') onApplyFilter(); }}
              style={{ flex: 1, padding: '4px 8px', background: '#1e1e1e', color: '#eee', border: '1px solid #444', borderRadius: 4 }}
            />
            <button className="fv-btn-refresh" onClick={onApplyFilter}>Apply</button>
          </div>
        </div>
        {balance != null && (
          <div className="fv-card fv-card-purple">
            <div className="fv-card-label">Wallet Balance</div>
            <div className="fv-card-value">{fmt(balance, cur)}</div>
          </div>
        )}
        <div className="fv-card fv-card-green">
          <div className="fv-card-label">Total Additions</div>
          <div className="fv-card-value">+{fmt(additions.totalAmount, cur)}</div>
        </div>
        <div className="fv-card fv-card-yellow">
          <div className="fv-card-label">Total Deductions</div>
          <div className="fv-card-value">-{fmt(deductions.totalAmount, cur)}</div>
        </div>
        <div className="fv-card fv-card-blue">
          <div className="fv-card-label">Net</div>
          <div className={`fv-card-value ${net < 0 ? 'neg' : ''}`}>{fmt(net, cur)}</div>
        </div>
      </div>

      <div className="fv-section">
        <h3>Additions (credits to driver)</h3>
        <table className="fv-accounts-table">
          <thead>
            <tr>
              <th>Reference</th>
              <th>Category</th>
              <th className="r">Entries</th>
              <th className="r">Amount</th>
            </tr>
          </thead>
          <tbody>
            {additions.items.length === 0 ? (
              <tr><td colSpan={4} className="fv-empty-msg">No additions in range</td></tr>
            ) : additions.items.map((it: any) => (
              <tr key={`add-${it.refType}`} className="fv-acct-row">
                <td><span className="fv-acct-badge fv-acct-revenue">{it.itemName}</span></td>
                <td className="fv-acct-type">{it.refType}</td>
                <td className="r">{it.entries}</td>
                <td className="r fv-credit-col">+{fmt(it.amount, it.currency || cur)}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>

      <div className="fv-section">
        <h3>Deductions (debits from driver)</h3>
        <table className="fv-accounts-table">
          <thead>
            <tr>
              <th>Reference</th>
              <th>Category</th>
              <th className="r">Entries</th>
              <th className="r">Amount</th>
            </tr>
          </thead>
          <tbody>
            {deductions.items.length === 0 ? (
              <tr><td colSpan={4} className="fv-empty-msg">No deductions in range</td></tr>
            ) : deductions.items.map((it: any) => (
              <tr key={`ded-${it.refType}`} className="fv-acct-row">
                <td><span className="fv-acct-badge fv-acct-expense">{it.itemName}</span></td>
                <td className="fv-acct-type">{it.refType}</td>
                <td className="r">{it.entries}</td>
                <td className="r fv-debit-col">-{fmt(it.amount, it.currency || cur)}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </>
  );
};
