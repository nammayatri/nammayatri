import React, { useEffect, useState } from 'react';
import './TopBarActions.css';

/**
 * Promise-based replacements for window.alert / window.confirm with the
 * dashboard's dark-themed modal chrome. Usage:
 *
 *   await showAlert('Failed to launch X');
 *   if (await showConfirm('Delete the world?', { variant: 'danger' })) ...
 *
 * Requires <DialogHost /> mounted once at the top of the app tree.
 */

export type DialogVariant = 'info' | 'danger' | 'success';

interface DialogReq {
  id: string;
  type: 'alert' | 'confirm';
  title?: string;
  message: string;
  variant?: DialogVariant;
  confirmLabel?: string;
  cancelLabel?: string;
  resolve: (ok: boolean) => void;
}

const subscribers = new Set<(reqs: DialogReq[]) => void>();
let queue: DialogReq[] = [];
const notify = () => subscribers.forEach(s => s([...queue]));
const nextId = () => Math.random().toString(36).slice(2);

export interface AlertOptions {
  title?: string;
  variant?: DialogVariant;
  confirmLabel?: string;
}

export interface ConfirmOptions extends AlertOptions {
  cancelLabel?: string;
}

export function showAlert(message: string, opts: AlertOptions = {}): Promise<void> {
  return new Promise(resolve => {
    queue.push({
      id: nextId(),
      type: 'alert',
      message,
      title: opts.title,
      variant: opts.variant ?? 'info',
      confirmLabel: opts.confirmLabel ?? 'OK',
      resolve: () => resolve(),
    });
    notify();
  });
}

export function showConfirm(message: string, opts: ConfirmOptions = {}): Promise<boolean> {
  return new Promise(resolve => {
    queue.push({
      id: nextId(),
      type: 'confirm',
      message,
      title: opts.title,
      variant: opts.variant ?? 'info',
      confirmLabel: opts.confirmLabel ?? 'Confirm',
      cancelLabel: opts.cancelLabel ?? 'Cancel',
      resolve,
    });
    notify();
  });
}

const variantIcon = (v?: DialogVariant) => {
  switch (v) {
    case 'danger': return '⚠';
    case 'success': return '✓';
    default: return 'ℹ';
  }
};
const variantTitleColor = (v?: DialogVariant) => {
  switch (v) {
    case 'danger': return '#f85149';
    case 'success': return '#56d364';
    default: return '#58a6ff';
  }
};

export const DialogHost: React.FC = () => {
  const [reqs, setReqs] = useState<DialogReq[]>([]);

  useEffect(() => {
    subscribers.add(setReqs);
    return () => { subscribers.delete(setReqs); };
  }, []);

  // Esc dismisses (Cancel for confirm, OK for alert).
  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      if (e.key === 'Escape' && queue.length > 0) {
        const [head, ...rest] = queue;
        queue = rest;
        notify();
        head.resolve(head.type === 'alert');
      } else if (e.key === 'Enter' && queue.length > 0) {
        const [head, ...rest] = queue;
        queue = rest;
        notify();
        head.resolve(true);
      }
    };
    window.addEventListener('keydown', onKey);
    return () => window.removeEventListener('keydown', onKey);
  }, []);

  const top = reqs[0];
  if (!top) return null;

  const respond = (ok: boolean) => {
    queue = queue.slice(1);
    notify();
    top.resolve(ok);
  };

  return (
    <div
      className="tb-modal-backdrop"
      style={{ zIndex: 2147483000 }}
      onClick={() => top.type === 'confirm' && respond(false)}
    >
      <div
        className="tb-modal"
        style={{ width: 'min(92vw, 460px)', maxHeight: 'unset' }}
        onClick={e => e.stopPropagation()}
        role="dialog"
        aria-modal="true"
      >
        <div className="tb-modal-header">
          <span className="tb-modal-title" style={{ color: variantTitleColor(top.variant) }}>
            <span style={{ fontSize: 16 }}>{variantIcon(top.variant)}</span>
            {top.title ?? (top.type === 'confirm' ? 'Confirm' : 'Notice')}
          </span>
          <button
            className="tb-modal-close"
            onClick={() => respond(top.type === 'alert')}
            title={top.type === 'confirm' ? 'Cancel' : 'Close'}
          >
            ✕
          </button>
        </div>

        <div
          style={{
            padding: '18px 18px 8px 18px',
            color: '#c9d1d9',
            fontSize: 13,
            lineHeight: 1.55,
            whiteSpace: 'pre-wrap',
            wordBreak: 'break-word',
          }}
        >
          {top.message}
        </div>

        <div className="tb-modal-footer" style={{ justifyContent: 'flex-end', gap: 8 }}>
          {top.type === 'confirm' && (
            <button className="tb-btn" onClick={() => respond(false)} autoFocus>
              {top.cancelLabel}
            </button>
          )}
          <button
            className={`tb-btn ${top.variant === 'danger' ? 'tb-err' : 'tb-modal-form-primary'}`}
            onClick={() => respond(true)}
            autoFocus={top.type === 'alert'}
          >
            {top.confirmLabel}
          </button>
        </div>
      </div>
    </div>
  );
};
