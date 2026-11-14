import React, { useEffect, useMemo, useState } from 'react';
import './Base64Media.css';

export type MediaKind = 'pdf' | 'image' | 'video' | 'audio';

export interface DetectedMedia {
  mime: string;
  kind: MediaKind;
  base64: string;
}

export interface FoundMedia extends DetectedMedia {
  path: string;
}

const MIN_LENGTH = 64;
const MAX_RESULTS = 20;
const MAX_DEPTH = 12;

const SIGNATURES: Array<{ prefix: string; mime: string; kind: MediaKind }> = [
  { prefix: 'JVBERi0', mime: 'application/pdf', kind: 'pdf' },
  { prefix: 'iVBORw0KGgo', mime: 'image/png', kind: 'image' },
  { prefix: '/9j/', mime: 'image/jpeg', kind: 'image' },
  { prefix: 'R0lGOD', mime: 'image/gif', kind: 'image' },
  { prefix: 'Qk', mime: 'image/bmp', kind: 'image' },
  { prefix: 'PHN2Zy', mime: 'image/svg+xml', kind: 'image' },
  { prefix: 'PD94bWwg', mime: 'image/svg+xml', kind: 'image' },
  { prefix: 'GkXfo', mime: 'video/webm', kind: 'video' },
  { prefix: 'SUQz', mime: 'audio/mpeg', kind: 'audio' },
  { prefix: '//uQ', mime: 'audio/mpeg', kind: 'audio' },
  { prefix: '//sQ', mime: 'audio/mpeg', kind: 'audio' },
  { prefix: 'T2dnUw', mime: 'audio/ogg', kind: 'audio' },
];

const BASE64_RE = /^[A-Za-z0-9+/_-]+={0,2}$/;

const kindOfMime = (mime: string): MediaKind | null => {
  if (mime === 'application/pdf') return 'pdf';
  if (mime.startsWith('image/')) return 'image';
  if (mime.startsWith('video/')) return 'video';
  if (mime.startsWith('audio/')) return 'audio';
  return null;
};

const riffMedia = (b64: string): { mime: string; kind: MediaKind } | null => {
  if (!b64.startsWith('UklGR')) return null;
  const head = safeAtob(b64.slice(0, 16));
  if (!head) return null;
  const tag = head.slice(8, 12);
  if (tag === 'WEBP') return { mime: 'image/webp', kind: 'image' };
  if (tag === 'WAVE') return { mime: 'audio/wav', kind: 'audio' };
  if (tag === 'AVI ') return { mime: 'video/x-msvideo', kind: 'video' };
  return null;
};

const mp4Media = (b64: string): { mime: string; kind: MediaKind } | null => {
  const head = safeAtob(b64.slice(0, 16));
  return head && head.slice(4, 8) === 'ftyp' ? { mime: 'video/mp4', kind: 'video' } : null;
};

function safeAtob(s: string): string | null {
  try {
    return atob(s.replace(/-/g, '+').replace(/_/g, '/'));
  } catch {
    return null;
  }
}

export function detectBase64Media(value: string): DetectedMedia | null {
  if (typeof value !== 'string' || value.length < MIN_LENGTH) return null;
  const dataUrl = /^data:([\w.+-]+\/[\w.+-]+)(?:;[\w-]+=[^;,]+)*;base64,/.exec(value.slice(0, 200));
  if (dataUrl) {
    const kind = kindOfMime(dataUrl[1]);
    return kind ? { mime: dataUrl[1], kind, base64: value.slice(dataUrl[0].length) } : null;
  }
  const compact = value.replace(/\s+/g, '');
  if (!BASE64_RE.test(compact.slice(0, 4096))) return null;
  const sig = SIGNATURES.find(s => compact.startsWith(s.prefix));
  const found = sig ? { mime: sig.mime, kind: sig.kind } : riffMedia(compact) || mp4Media(compact);
  if (!found) return null;
  if (found.mime === 'image/bmp' && compact.length < 200) return null;
  return { ...found, base64: compact };
}

export function findBase64Media(data: unknown): FoundMedia[] {
  const out: FoundMedia[] = [];
  const walk = (node: unknown, path: string, depth: number) => {
    if (out.length >= MAX_RESULTS || depth > MAX_DEPTH || node === null || node === undefined) return;
    if (typeof node === 'string') {
      const media = detectBase64Media(node);
      if (media) {
        out.push({ ...media, path: path || '(body)' });
        return;
      }
      const trimmed = node.trim();
      if (depth === 0 && (trimmed.startsWith('{') || trimmed.startsWith('['))) {
        try {
          walk(JSON.parse(trimmed), path, depth + 1);
        } catch {
          return;
        }
      }
      return;
    }
    if (Array.isArray(node)) {
      node.forEach((v, i) => walk(v, `${path}[${i}]`, depth + 1));
      return;
    }
    if (typeof node === 'object') {
      Object.entries(node as Record<string, unknown>).forEach(([k, v]) => walk(v, path ? `${path}.${k}` : k, depth + 1));
    }
  };
  walk(data, '', 0);
  return out;
}

function base64ToBlob(base64: string, mime: string): Blob {
  const bin = safeAtob(base64) || '';
  const bytes = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
  return new Blob([bytes], { type: mime });
}

const extensionOf = (mime: string) => (mime === 'image/svg+xml' ? 'svg' : mime.split('/')[1].replace('x-msvideo', 'avi').replace('mpeg', 'mp3'));

const formatSize = (base64: string) => {
  const bytes = Math.floor((base64.length * 3) / 4);
  return bytes > 1024 * 1024 ? `${(bytes / 1024 / 1024).toFixed(1)} MB` : `${Math.max(1, Math.round(bytes / 1024))} KB`;
};

const KIND_ICON: Record<MediaKind, string> = { pdf: '📄', image: '🖼', video: '🎬', audio: '🎵' };

export function MediaViewerModal({ media, onClose }: { media: FoundMedia; onClose: () => void }) {
  const url = useMemo(() => URL.createObjectURL(base64ToBlob(media.base64, media.mime)), [media]);
  useEffect(() => () => URL.revokeObjectURL(url), [url]);
  useEffect(() => {
    const onKey = (e: KeyboardEvent) => { if (e.key === 'Escape') onClose(); };
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, [onClose]);
  const fileName = `${media.path.replace(/[^\w.-]+/g, '_') || 'media'}.${extensionOf(media.mime)}`;

  return (
    <div className="b64m-overlay" onClick={onClose}>
      <div className="b64m-modal" onClick={e => e.stopPropagation()}>
        <div className="b64m-toolbar">
          <span className="b64m-title">{KIND_ICON[media.kind]} {media.path}</span>
          <span className="b64m-meta">{media.mime} · {formatSize(media.base64)}</span>
          <a className="b64m-btn" href={url} download={fileName}>Download</a>
          <a className="b64m-btn" href={url} target="_blank" rel="noreferrer">Open in tab</a>
          <button className="b64m-close" onClick={onClose}>{'✕'}</button>
        </div>
        <div className="b64m-body">
          {media.kind === 'pdf' && <iframe className="b64m-frame" src={url} title={media.path} />}
          {media.kind === 'image' && <img className="b64m-img" src={url} alt={media.path} />}
          {media.kind === 'video' && <video className="b64m-img" src={url} controls />}
          {media.kind === 'audio' && <audio src={url} controls />}
        </div>
      </div>
    </div>
  );
}

export function Base64MediaButtons({ data }: { data: unknown }) {
  const found = useMemo(() => findBase64Media(data), [data]);
  const [open, setOpen] = useState<FoundMedia | null>(null);
  if (found.length === 0) return null;
  return (
    <div className="b64m-bar" onClick={e => e.stopPropagation()}>
      <span className="b64m-label">Media in response:</span>
      {found.map(m => (
        <button key={m.path} className="b64m-chip" onClick={() => setOpen(m)} title={`${m.mime} · ${formatSize(m.base64)}`}>
          {KIND_ICON[m.kind]} View {m.path}
        </button>
      ))}
      {open && <MediaViewerModal media={open} onClose={() => setOpen(null)} />}
    </div>
  );
}

export function InlineMediaButton({ value, path }: { value: string; path: string }) {
  const media = useMemo(() => detectBase64Media(value), [value]);
  const [open, setOpen] = useState(false);
  if (!media) return null;
  return (
    <>
      <button className="b64m-inline" onClick={e => { e.stopPropagation(); setOpen(true); }} title={`${media.mime} · ${formatSize(media.base64)}`}>
        {KIND_ICON[media.kind]} view {media.kind}
      </button>
      {open && <MediaViewerModal media={{ ...media, path: path || '(value)' }} onClose={() => setOpen(false)} />}
    </>
  );
}
