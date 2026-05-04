import React, { useEffect, useMemo, useRef, useState } from 'react';

const PROXY_BASE = 'http://localhost:7082';

interface RefSummary {
  name: string;
  sha: string;
  date?: string;
}

interface CommitSummary {
  sha: string;
  subject: string;
  author: string;
  date: string;
}

interface RefsPayload {
  cloned: boolean;
  default_branch: string;
  current: { branch: string; sha: string; subject: string; date: string } | null;
  branches: RefSummary[];
  commits: CommitSummary[];
}

interface RefPickerProps {
  /** GitHub repo, e.g. "nammayatri/ny-react-native" — must match a key
   *  in server.py's KNOWN_REPOS map. */
  repo: string;
  /** The currently chosen ref (controlled). Empty string → "use whatever
   *  the local checkout is on right now" (no checkout step in setup). */
  value: string;
  onChange: (ref: string) => void;
  disabled?: boolean;
}

/**
 * Datalist-backed combobox for picking a git ref (branch name or commit
 * SHA). Behaviour:
 *   - On mount, fetch /api/git/refs?repo=<repo>. Show the local current
 *     branch+commit as a hint above the input.
 *   - User types a substring → we re-fetch with ?q= so the suggestion
 *     list filters server-side (covers refs that aren't on disk yet).
 *   - The input value is the literal ref text we send back to the start
 *     POST. Branch names AND commit SHAs both work; the setup script
 *     does `git fetch + checkout --detach` either way.
 *   - Empty input → no checkout step (leave the existing checkout in
 *     place). Useful when the user just pulled and wants to reuse it.
 */
export const RefPicker: React.FC<RefPickerProps> = ({
  repo, value, onChange, disabled,
}) => {
  const [data, setData] = useState<RefsPayload | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [searching, setSearching] = useState(false);
  const debounceRef = useRef<number | null>(null);

  // Fetch refs (with optional ?q= for server-side filtering). Debounce
  // typing so we don't hammer the gh API on every keystroke.
  useEffect(() => {
    if (debounceRef.current) {
      window.clearTimeout(debounceRef.current);
    }
    debounceRef.current = window.setTimeout(async () => {
      setSearching(true);
      try {
        const url = new URL(`${PROXY_BASE}/api/git/refs`);
        url.searchParams.set('repo', repo);
        if (value && value.length >= 2) url.searchParams.set('q', value);
        const r = await fetch(url.toString());
        if (!r.ok) {
          const err = await r.json().catch(() => ({}));
          setError(err?.error ?? `HTTP ${r.status}`);
          setData(null);
          return;
        }
        const d = await r.json() as RefsPayload;
        setError(null);
        setData(d);
      } catch (e) {
        setError(String(e));
        setData(null);
      } finally {
        setSearching(false);
      }
    }, 250);
    return () => {
      if (debounceRef.current) window.clearTimeout(debounceRef.current);
    };
  }, [repo, value]);

  // Build the datalist option list — branches first, then recent commits
  // (formatted as "<short-sha> — <subject>"). Dedup by exact value.
  const datalistId = `refpicker-${repo.replace(/[^A-Za-z0-9]/g, '_')}`;
  const options = useMemo(() => {
    if (!data) return [] as { value: string; label: string }[];
    const seen = new Set<string>();
    const out: { value: string; label: string }[] = [];
    for (const b of data.branches) {
      if (seen.has(b.name)) continue;
      seen.add(b.name);
      out.push({ value: b.name, label: `branch · ${b.name}` });
    }
    for (const c of data.commits) {
      const short = c.sha.slice(0, 8);
      if (seen.has(short)) continue;
      seen.add(short);
      out.push({ value: short, label: `${short} · ${c.subject} (${c.author})` });
    }
    return out;
  }, [data]);

  // Chip text — kept short so it slots into the label row without
  // pushing the input down. Subject + branch count live in the title
  // tooltip so hover gives the user the full context on demand.
  const chipText = data?.current
    ? `${data.current.branch} @ ${data.current.sha.slice(0, 7)}`
    : data?.cloned === false ? `default · ${data.default_branch}`
    : searching ? '…' : '';
  const chipTitle = data?.current
    ? `${data.current.branch} @ ${data.current.sha}\n${data.current.subject}\n` +
      `${data.branches.length} branches · ${data.commits.length} recent commits`
    : data?.cloned === false
      ? `repo not cloned yet — blank input uses ${data.default_branch}`
      : 'loading refs…';

  return (
    <div className="tb-refpicker">
      <label className="tb-refpicker-label" htmlFor={datalistId + '-input'}>
        Ref
        {chipText && (
          <span
            className={`tb-refpicker-current${error ? ' is-error' : ''}`}
            title={error ?? chipTitle}
          >
            {error ? '!' : chipText}
          </span>
        )}
      </label>
      <input
        id={datalistId + '-input'}
        list={datalistId}
        value={value}
        onChange={e => onChange(e.target.value)}
        placeholder={data?.default_branch ?? 'main'}
        disabled={disabled}
        spellCheck={false}
        autoComplete="off"
        className="tb-refpicker-input"
        title={chipTitle}
      />
      <datalist id={datalistId}>
        {options.map(o => (
          <option key={o.value} value={o.value}>{o.label}</option>
        ))}
      </datalist>
    </div>
  );
};
