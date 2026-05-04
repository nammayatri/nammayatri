// Tiny client for the test-context-api in-memory ui-state bag.
//
// Why a server-side bag rather than localStorage? Two reasons:
//   1. Multiple browser windows / IDE-embedded webviews share the same
//      backend; localStorage would diverge per origin (e.g. file:// vs
//      http://localhost:3000 vs the production-build serve port).
//   2. The test-context-api process is the natural lifecycle owner of
//      "the current dev session" — when it restarts the user normally
//      WANTS the picks to reset to fresh defaults (e.g. the launcher's
//      auto-detected variant list may have changed). Keeping the store
//      in-process gives us that reset for free.
//
// The bag is a flat string-keyed dict; values can be any JSON. Don't put
// secrets in here — the server has no auth.

const PROXY_BASE = 'http://localhost:7082';

export type UiState = Record<string, unknown>;

export async function loadUiState(): Promise<UiState> {
  try {
    const r = await fetch(`${PROXY_BASE}/api/ui-state`);
    if (!r.ok) return {};
    const d = await r.json();
    return (d && typeof d === 'object') ? d as UiState : {};
  } catch {
    return {};
  }
}

// Fire-and-forget partial update. Network failures are silently swallowed
// — losing a single dropdown value is harmless and we don't want a blip
// to surface as a red banner. For multi-key changes coalesced by callers,
// pass them all in one call (one PUT, one merge round-trip).
export function saveUiState(patch: UiState): void {
  // null values delete the key on the server (see _ui_state PUT handler).
  fetch(`${PROXY_BASE}/api/ui-state`, {
    method: 'PUT',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(patch),
  }).catch(() => { /* ignore */ });
}
