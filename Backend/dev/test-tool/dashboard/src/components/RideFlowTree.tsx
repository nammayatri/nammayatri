import React, { useState } from 'react';
import { StepResult, StepStatus, Step } from '../types';
import { LocationPreset } from '../api-catalog/types';
import {
  getReconciliationTypeOption,
  isReconciliationType,
  RECONCILIATION_TYPE_OPTIONS,
  type ReconciliationType,
} from '../backendTypes';
import { getLocationsForCity } from '../mock-data/locations';
import './RideFlowTree.css';

// --- Tree node definitions ---

export interface TreeNode {
  id: string;
  title: string;
  tag: 'driver' | 'rider' | 'system';
  steps: Step[];
  children?: TreeNode[];  // branching options (e.g. fulfillment vs cancel)
}

export interface FlowOption {
  id: string;
  name: string;
  description: string;
}

interface Props {
  // Flow selector
  flows: FlowOption[];
  activeFlowId: string;
  onFlowChange: (id: string) => void;
  // Common
  city: string;
  results: Record<string, StepResult>;
  isRunning: boolean;
  runningNodeId: string | null;
  // Config state (ride flow)
  driverLocationIdx: number;
  onDriverLocationChange: (idx: number) => void;
  fromLocationIdx: number;
  toLocationIdx: number;
  onFromLocationChange: (idx: number) => void;
  onToLocationChange: (idx: number) => void;
  paymentPreset: string;
  onPaymentPresetChange: (id: string) => void;
  paymentMethods: { cardId: string; lastFourDigits?: string; cardType?: string }[];
  selectedPaymentMethodId: string;
  onPaymentMethodChange: (id: string) => void;
  paymentMethodsLoading: boolean;
  captureRideId: string;
  onCaptureRideIdChange: (id: string) => void;
  tipAmount: number;
  onTipAmountChange: (amount: number) => void;
  skipTip: boolean;
  onSkipTipChange: (skip: boolean) => void;
  rideEndMode: string;
  onRideEndModeChange: (mode: string) => void;
  reconType: ReconciliationType;
  onReconTypeChange: (type: ReconciliationType) => void;
  reconDate: string;
  onReconDateChange: (date: string) => void;
  subscriptionPurchaseId: string;
  onSubscriptionPurchaseIdChange: (id: string) => void;
  // Actions
  onMakeDriverAvailable: () => void;
  onRunNode: (nodeId: string) => void;
  onRunAll: () => void;
  onStop: () => void;
  driverAvailable: boolean;
  selectedOutcome: string;
  onOutcomeChange: (outcome: string) => void;
  skippedNodes: Record<string, boolean>;
  onToggleSkipNode: (nodeId: string) => void;
  // Fleet onboarding
  fleetType?: string;
  onFleetTypeChange?: (type: string) => void;
  fleetMobile?: string;
  onFleetMobileChange?: (mobile: string) => void;
  aadhaarNumber?: string;
  onAadhaarNumberChange?: (num: string) => void;
  panNumber?: string;
  onPanNumberChange?: (num: string) => void;
  gstNumber?: string;
  onGstNumberChange?: (num: string) => void;
  requiresAdminApproval?: boolean;
  onRequiresAdminApprovalChange?: (val: boolean) => void;
  adminEmail?: string;
  onAdminEmailChange?: (email: string) => void;
  adminPassword?: string;
  onAdminPasswordChange?: (password: string) => void;
  /** Reconciliation flow: which merchant/city API calls use (from context-api recon_harness). */
  reconHarnessHint?: string;
}

function statusIcon(status: StepStatus | undefined): string {
  switch (status) {
    case 'pass': return 'node-pass';
    case 'fail': return 'node-fail';
    case 'running': return 'node-running';
    case 'skip': return 'node-skip';
    default: return 'node-pending';
  }
}

function nodeStatus(node: TreeNode, results: Record<string, StepResult>): StepStatus {
  const statuses = node.steps.map(s => results[s.id]?.status);
  if (statuses.some(s => s === 'fail')) return 'fail';
  if (statuses.some(s => s === 'running')) return 'running';
  if (statuses.every(s => s === 'pass')) return 'pass';
  if (statuses.some(s => s === 'pass')) return 'running'; // partially done
  return 'pending';
}

function StepRow({ step, result }: { step: Step; result?: StepResult }) {
  const [expanded, setExpanded] = useState(false);
  const status = result?.status || 'pending';
  return (
    <div className="step-row">
      <div className={`step-header ${status}`} onClick={() => result && setExpanded(!expanded)}>
        <span className={`step-dot ${status}`} />
        <span className="step-name">{step.name}</span>
        {result?.summary && <span className="step-summary">{result.summary}</span>}
        {result?.durationMs ? <span className="step-time">{result.durationMs}ms</span> : null}
        {result?.error && <span className="step-error">{result.error}</span>}
        {result && <span className="step-expand">{expanded ? '\u25BC' : '\u25B6'}</span>}
      </div>
      {expanded && result?.response && (
        <pre className="step-response">{JSON.stringify(result.response, null, 2)}</pre>
      )}
    </div>
  );
}

export const RideFlowTree: React.FC<Props> = ({
  flows, activeFlowId, onFlowChange,
  city, results, isRunning, runningNodeId,
  driverLocationIdx, onDriverLocationChange,
  fromLocationIdx, toLocationIdx, onFromLocationChange, onToLocationChange,
  paymentPreset, onPaymentPresetChange,
  paymentMethods, selectedPaymentMethodId, onPaymentMethodChange, paymentMethodsLoading,
  captureRideId, onCaptureRideIdChange,
  tipAmount, onTipAmountChange, skipTip, onSkipTipChange,
  rideEndMode, onRideEndModeChange,
  reconType, onReconTypeChange, reconDate, onReconDateChange, subscriptionPurchaseId, onSubscriptionPurchaseIdChange,
  onMakeDriverAvailable, onRunNode, onRunAll, onStop,
  driverAvailable,
  selectedOutcome: selectedOutcomeProp, onOutcomeChange,
  skippedNodes, onToggleSkipNode,
  fleetType, onFleetTypeChange, fleetMobile, onFleetMobileChange,
  aadhaarNumber, onAadhaarNumberChange, panNumber, onPanNumberChange, gstNumber, onGstNumberChange,
  requiresAdminApproval, onRequiresAdminApprovalChange,
  adminEmail, onAdminEmailChange, adminPassword, onAdminPasswordChange,
  reconHarnessHint,
}) => {
  const locations = getLocationsForCity(city);
  const [expandedNodes, setExpandedNodes] = useState<Record<string, boolean>>({ discovery: true });
  const selectedOutcome = selectedOutcomeProp as 'fulfillment' | 'driver-cancel' | 'customer-cancel';

  const toggle = (id: string) => setExpandedNodes(prev => ({ ...prev, [id]: !prev[id] }));

  // Define tree nodes per flow
  const outcomeNodes: Record<string, TreeNode> = {
    'fulfillment': { id: 'fulfillment', title: 'Ride Fulfillment', tag: 'driver', steps: [
        { id: 'driver-ride-list', name: 'Get Active Ride (Driver)', method: 'GET', service: 'driver', path: '/placeholder', auth: true },
        { id: 'ride-start', name: 'Start Ride', method: 'POST', service: 'driver', path: '/placeholder', auth: true },
        { id: 'inflate-distance', name: 'Inflate Traveled Distance (3x)', method: 'POST', service: 'internal' as const, path: '/placeholder', auth: false },
        { id: 'ride-end', name: 'End Ride', method: 'POST', service: 'driver', path: '/placeholder', auth: true },
    ] },
    'driver-cancel': { id: 'driver-cancel', title: 'Driver Cancellation', tag: 'driver', steps: [
        { id: 'driver-ride-list-cancel', name: 'Get Active Ride (Driver)', method: 'GET', service: 'driver', path: '/placeholder', auth: true },
        { id: 'driver-cancel-ride', name: 'Cancel Ride', method: 'POST', service: 'driver', path: '/placeholder', auth: true },
    ] },
    'customer-cancel': { id: 'customer-cancel', title: 'Customer Cancellation', tag: 'rider', steps: [
        { id: 'customer-cancel-ride', name: 'Cancel Ride', method: 'POST', service: 'rider', path: '/placeholder', auth: true },
    ] },
  };

  const rideNodes: TreeNode[] = [
    { id: 'discovery', title: 'Discovery', tag: 'rider', steps: [
        { id: 'ride-search', name: 'Search Ride', method: 'POST', service: 'rider', path: '/rideSearch', auth: true },
        { id: 'get-estimates', name: 'Get Estimates', method: 'GET', service: 'rider', path: '/placeholder', auth: true },
        { id: 'estimate-select', name: 'Select Estimate', method: 'POST', service: 'rider', path: '/placeholder', auth: true },
    ] },
    { id: 'driver-accept', title: 'Driver Accept', tag: 'driver', steps: [
        { id: 'driver-nearby', name: 'Get Nearby Requests', method: 'GET', service: 'driver', path: '/driver/nearbyRideRequest', auth: true },
        { id: 'driver-respond', name: 'Accept Quote', method: 'POST', service: 'driver', path: '/driver/searchRequest/quote/respond', auth: true },
    ] },
    { id: 'booking', title: 'Booking Confirmation', tag: 'rider', steps: [
        { id: 'poll-booking', name: 'Poll Estimate Results', method: 'GET', service: 'rider', path: '/placeholder', auth: true },
        { id: 'get-booking', name: 'Get Booking Details', method: 'POST', service: 'rider', path: '/placeholder', auth: true },
    ] },
    outcomeNodes[selectedOutcome],
    // Add Tip node — only visible after fulfillment (not cancellation)
    ...(selectedOutcome === 'fulfillment' ? [{
      id: 'add-tip', title: 'Add Tip', tag: 'rider' as const, steps: [
        { id: 'add-tip' as const, name: 'Add Tip', method: 'POST' as const, service: 'rider' as const, path: '/placeholder', auth: true },
      ],
    }] : []),
  ];

  const duesNodes: TreeNode[] = [
    { id: 'check-dues', title: 'Check Pending Dues', tag: 'rider', steps: [
        { id: 'get-dues', name: 'Get Due Amount', method: 'GET', service: 'rider', path: '/payment/getDueAmount', auth: true },
    ] },
    { id: 'capture-payment', title: 'Capture Payment', tag: 'rider', steps: [
        { id: 'capture-payment', name: 'Capture Payment Intent', method: 'POST', service: 'rider', path: '/placeholder', auth: true },
    ] },
    { id: 'clear-dues', title: 'Clear Dues', tag: 'rider', steps: [
        { id: 'clear-dues', name: 'Clear Dues', method: 'POST', service: 'rider', path: '/payment/clearDues', auth: true },
    ] },
    { id: 'cancellation-dues', title: 'Cancellation Dues', tag: 'rider', steps: [
        { id: 'get-cancellation-dues', name: 'Get Cancellation Dues', method: 'GET', service: 'rider', path: '/rideBooking/cancellationDues', auth: true },
    ] },
  ];

  const reconNodes: TreeNode[] = [
    { id: 'prereq-check', title: 'Check Readiness', tag: 'system', steps: [
        { id: 'recon-readiness', name: 'Check Readiness', method: 'GET', service: 'internal' as const, path: '/placeholder', auth: false },
    ] },
    { id: 'seed-payment-settlement', title: 'Seed Payment Settlement', tag: 'system', steps: [
        { id: 'seed-payment-settlement', name: 'Seed Payment Settlement', method: 'POST', service: 'internal' as const, path: '/placeholder', auth: false },
    ] },
    { id: 'trigger-recon', title: 'Trigger Reconciliation', tag: 'system', steps: [
        { id: 'trigger-recon', name: 'Trigger Reconciliation', method: 'POST', service: 'internal' as const, path: '/placeholder', auth: false },
    ] },
    { id: 'poll-job', title: 'Poll Job', tag: 'system', steps: [
        { id: 'poll-recon-job', name: 'Poll Reconciliation Job', method: 'GET', service: 'internal' as const, path: '/placeholder', auth: false },
    ] },
    { id: 'verify-summary', title: 'Verify Summary', tag: 'system', steps: [
        { id: 'verify-recon-summary', name: 'Verify Reconciliation Summary', method: 'GET', service: 'internal' as const, path: '/placeholder', auth: false },
    ] },
    { id: 'verify-entries', title: 'Verify Entries', tag: 'system', steps: [
        { id: 'verify-recon-entries', name: 'Verify Reconciliation Entries', method: 'GET', service: 'internal' as const, path: '/placeholder', auth: false },
    ] },
  ];

  const adminApprovalNode: TreeNode = { id: 'fleet-admin-approve', title: 'Admin Approval', tag: 'system', steps: [
      { id: 'admin-login', name: 'Admin Login', method: 'POST', service: 'provider-dashboard', path: '/placeholder', auth: false },
      { id: 'fetch-unverified', name: 'Fetch Unverified Accounts', method: 'GET', service: 'provider-dashboard', path: '/placeholder', auth: true },
      { id: 'approve-fleet', name: 'Approve Fleet Owner', method: 'POST', service: 'provider-dashboard', path: '/placeholder', auth: true },
      { id: 'fleet-verify-enabled', name: 'Verify Fleet Enabled', method: 'GET', service: 'provider-dashboard', path: '/placeholder', auth: true },
  ] };

  const docUploadNode: TreeNode = { id: 'fleet-doc-upload', title: 'Document Upload', tag: 'system', steps: [
      { id: 'upload-aadhaar-front', name: 'Upload Aadhaar Front', method: 'POST', service: 'provider-dashboard', path: '/placeholder', auth: true },
      { id: 'upload-aadhaar-back', name: 'Upload Aadhaar Back', method: 'POST', service: 'provider-dashboard', path: '/placeholder', auth: true },
      { id: 'upload-pan', name: 'Upload PAN Card', method: 'POST', service: 'provider-dashboard', path: '/placeholder', auth: true },
      { id: 'upload-gst', name: 'Upload GST Certificate', method: 'POST', service: 'provider-dashboard', path: '/placeholder', auth: true },
  ] };

  const docVerifyNode: TreeNode = { id: 'fleet-verify-docs', title: 'Document Verification', tag: 'system', steps: [
      { id: 'configure-mock-idfy', name: 'Configure Mock Idfy', method: 'POST', service: 'mock-idfy', path: '/configure', auth: false },
      { id: 'verify-aadhaar', name: 'Verify Aadhaar', method: 'POST', service: 'provider-dashboard', path: '/placeholder', auth: true },
      { id: 'verify-pan', name: 'Verify PAN', method: 'POST', service: 'provider-dashboard', path: '/placeholder', auth: true },
      { id: 'verify-gst', name: 'Verify GST', method: 'POST', service: 'provider-dashboard', path: '/placeholder', auth: true },
  ] };

  const fleetNodes: TreeNode[] = [
    { id: 'fleet-auth', title: 'Fleet Login', tag: 'system', steps: [
        { id: 'fleet-login-otp', name: 'Send OTP', method: 'POST', service: 'provider-dashboard', path: '/placeholder', auth: false },
        { id: 'fleet-verify-otp', name: 'Verify OTP', method: 'POST', service: 'provider-dashboard', path: '/placeholder', auth: false },
        { id: 'fleet-profile', name: 'Get Profile', method: 'GET', service: 'provider-dashboard', path: '/placeholder', auth: true },
    ] },
    ...(requiresAdminApproval
      ? [adminApprovalNode]
      : [docUploadNode, docVerifyNode,
         { id: 'fleet-register', title: 'Complete Registration', tag: 'system' as const, steps: [
             { id: 'fleet-register', name: 'Complete Registration', method: 'POST' as const, service: 'provider-dashboard' as const, path: '/placeholder', auth: true },
             { id: 'fleet-verify-profile', name: 'Verify Fleet Status', method: 'GET' as const, service: 'provider-dashboard' as const, path: '/placeholder', auth: true },
         ] }]),
  ];

  const nodes =
    activeFlowId === 'fleet-onboarding'
      ? fleetNodes
      : activeFlowId === 'dues-flow'
        ? duesNodes
        : activeFlowId === 'reconciliation-flow'
          ? reconNodes
          : rideNodes;
  const showDriverSetup = activeFlowId === 'ride-flow';

  const renderLocationSelect = (label: string, value: number, onChange: (idx: number) => void, locs: LocationPreset[]) => (
    <div className="tree-config-field">
      <label>{label}</label>
      <select value={value} onChange={e => onChange(Number(e.target.value))}>
        {locs.map((loc, i) => (
          <option key={i} value={i}>{loc.name}</option>
        ))}
      </select>
    </div>
  );

  const renderNode = (node: TreeNode, index: number, isLast: boolean) => {
    const isSkipped = skippedNodes[node.id] || false;
    const ns = isSkipped ? 'skip' as StepStatus : nodeStatus(node, results);
    const isExpanded = expandedNodes[node.id] ?? false;
    const isActive = runningNodeId === node.id;
    const isDisabled = node.id === 'cancellation'; // coming soon

    return (
      <div key={node.id} className={`tree-node ${isActive ? 'active' : ''} ${isDisabled ? 'disabled' : ''} ${isSkipped ? 'skipped' : ''}`}>
        {/* Connector line */}
        {index > 0 && <div className="tree-connector" />}

        {/* Node header */}
        <div className={`tree-node-header ${statusIcon(ns)}`} onClick={() => !isDisabled && toggle(node.id)}>
          <div className="tree-node-left">
            <span className={`tree-node-dot ${statusIcon(ns)}`} />
            <span className={`tree-node-tag tag-${node.tag}`}>{node.tag}</span>
            <span className="tree-node-title">{node.title}</span>
            {isDisabled && <span className="tree-node-badge">Coming Soon</span>}
          </div>
          <div className="tree-node-right">
            {isSkipped && <span className="tree-node-skip-badge">Skipped</span>}
            {!isSkipped && ns === 'pass' && <span className="tree-node-check">Done</span>}
            {!isSkipped && ns === 'fail' && <span className="tree-node-fail-badge">Failed</span>}
            {!isSkipped && ns === 'running' && <span className="tree-node-running-badge">Running...</span>}
            {!isDisabled && <span className="tree-expand">{isExpanded ? '\u25BC' : '\u25B6'}</span>}
          </div>
        </div>

        {/* Node body (expanded) */}
        {isExpanded && !isDisabled && (
          <div className="tree-node-body">
            {/* Config area for fleet auth node */}
            {node.id === 'fleet-auth' && (
              <div className="tree-config">
                <div className="tree-config-field">
                  <label>Fleet Type</label>
                  <select value={fleetType || 'NORMAL_FLEET'} onChange={e => onFleetTypeChange?.(e.target.value)}>
                    <option value="NORMAL_FLEET">Individual Fleet</option>
                    <option value="BUSINESS_FLEET">Business Fleet</option>
                  </select>
                </div>
                <div className="tree-config-field">
                  <label>Mobile Number</label>
                  <input type="text" value={fleetMobile || ''} onChange={e => onFleetMobileChange?.(e.target.value)} placeholder="9999900001" style={{ width: 120 }} />
                </div>
                <div className="tree-config-field">
                  <label>Approval</label>
                  <label className="tree-checkbox-label" style={{ display: 'flex', alignItems: 'center', gap: 4, marginTop: 4 }}>
                    <input type="checkbox" checked={requiresAdminApproval || false} onChange={e => onRequiresAdminApprovalChange?.(e.target.checked)} />
                    Admin Approval
                  </label>
                </div>
                {requiresAdminApproval && (
                  <>
                    <div className="tree-config-field">
                      <label>Admin Email</label>
                      <input type="text" value={adminEmail || ''} onChange={e => onAdminEmailChange?.(e.target.value)} placeholder="admin@example.com" style={{ width: 160 }} />
                    </div>
                    <div className="tree-config-field">
                      <label>Admin Password</label>
                      <input type="password" value={adminPassword || ''} onChange={e => onAdminPasswordChange?.(e.target.value)} placeholder="password" style={{ width: 120 }} />
                    </div>
                  </>
                )}
                {!requiresAdminApproval && (
                  <>
                    <div className="tree-config-field">
                      <label>Aadhaar No.</label>
                      <input type="text" value={aadhaarNumber || ''} onChange={e => onAadhaarNumberChange?.(e.target.value)} placeholder="123456789012" style={{ width: 120 }} />
                    </div>
                    <div className="tree-config-field">
                      <label>PAN No.</label>
                      <input type="text" value={panNumber || ''} onChange={e => onPanNumberChange?.(e.target.value)} placeholder="ABCDE1234F" style={{ width: 100 }} />
                    </div>
                    <div className="tree-config-field">
                      <label>GST No.</label>
                      <input type="text" value={gstNumber || ''} onChange={e => onGstNumberChange?.(e.target.value)} placeholder="22ABCDE1234F1Z5" style={{ width: 140 }} />
                    </div>
                  </>
                )}
              </div>
            )}
            {/* Config area for discovery node */}
            {node.id === 'discovery' && (
              <div className="tree-config">
                {renderLocationSelect('From', fromLocationIdx, onFromLocationChange, locations)}
                {renderLocationSelect('To', toLocationIdx, onToLocationChange, locations.length > 1 ? locations : locations)}
                <div className="tree-config-field">
                  <label>Payment</label>
                  <select value={paymentPreset} onChange={e => onPaymentPresetChange(e.target.value)}>
                    <option value="with-card-payment">Card Payment</option>
                    <option value="cash-payment">Cash Payment</option>
                  </select>
                </div>
                {paymentPreset === 'with-card-payment' && (
                  <div className="tree-config-field">
                    <label>Card {paymentMethodsLoading ? '(loading...)' : `(${paymentMethods.length})`}</label>
                    <select
                      value={selectedPaymentMethodId}
                      onChange={e => onPaymentMethodChange(e.target.value)}
                      disabled={paymentMethodsLoading || paymentMethods.length === 0}
                    >
                      {paymentMethods.length === 0 && !paymentMethodsLoading && <option value="">No cards found</option>}
                      {paymentMethods.map(pm => (
                        <option key={pm.cardId} value={pm.cardId}>
                          {pm.cardId}
                        </option>
                      ))}
                    </select>
                  </div>
                )}
              </div>
            )}

            {/* Config area for fulfillment node */}
            {node.id === 'fulfillment' && (
              <div className="tree-config">
                <div className="tree-config-field">
                  <label>Ride End Location</label>
                  <select value={rideEndMode} onChange={e => onRideEndModeChange(e.target.value)}>
                    <option value="actual">Actual Destination (no recompute)</option>
                    <option value="downward-recompute">Pickup Location (downward recompute)</option>
                    <option value="upward-recompute">3x Overshoot (upward recompute)</option>
                  </select>
                </div>
              </div>
            )}

            {/* Config area for add-tip node */}
            {node.id === 'add-tip' && (
              <div className="tree-config">
                <div className="tree-config-field" style={{ maxWidth: 150 }}>
                  <label>Tip Amount</label>
                  <input
                    className="tree-config-input"
                    type="number"
                    min={1}
                    value={tipAmount}
                    onChange={e => onTipAmountChange(Number(e.target.value) || 0)}
                    disabled={skipTip}
                  />
                </div>
                <div className="tree-config-field" style={{ maxWidth: 120, display: 'flex', alignItems: 'flex-end' }}>
                  <label className="tree-checkbox-label">
                    <input type="checkbox" checked={skipTip} onChange={e => onSkipTipChange(e.target.checked)} />
                    Skip Tip
                  </label>
                </div>
              </div>
            )}

            {/* Config area for capture-payment node */}
            {node.id === 'capture-payment' && (
              <div className="tree-config">
                <div className="tree-config-field">
                  <label>Ride ID (auto-filled from dues or leave empty for latest)</label>
                  <input
                    className="tree-config-input"
                    type="text"
                    placeholder="Enter rideId or auto-detected from Get Dues"
                    value={captureRideId}
                    onChange={e => onCaptureRideIdChange(e.target.value)}
                  />
                </div>
              </div>
            )}

            {node.id === 'prereq-check' && (
              <div className="tree-config">
                <div className="tree-config-field">
                  <label>Reconciliation Type</label>
                  <select
                    value={reconType}
                    onChange={e => {
                      const v = e.target.value;
                      if (isReconciliationType(v)) onReconTypeChange(v);
                    }}
                  >
                    {RECONCILIATION_TYPE_OPTIONS.map(opt => (
                      <option key={opt.value} value={opt.value}>{opt.label}</option>
                    ))}
                  </select>
                  {(() => {
                    const opt = getReconciliationTypeOption(reconType);
                    if (!opt?.prerequisites.length) return null;
                    return (
                      <ul className="tree-config-prereq" style={{ margin: '8px 0 0', paddingLeft: '1.2rem', fontSize: '0.85rem', opacity: 0.9 }}>
                        {opt.prerequisites.map((line, i) => (
                          <li key={i}>{line}</li>
                        ))}
                      </ul>
                    );
                  })()}
                </div>
                <div className="tree-config-field">
                  <label>Recon Date</label>
                  <input
                    className="tree-config-input"
                    type="date"
                    value={reconDate}
                    onChange={e => onReconDateChange(e.target.value)}
                  />
                </div>
                <div className="tree-config-field" style={{ minWidth: 320 }}>
                  <label>Trigger</label>
                  <div className="tree-config-help">
                    {`POST provider-dashboard /bpp/driver-offer/{merchantShortId}/{city}/merchant/scheduler/trigger with jobName=ReconciliationTrigger (production scheduler path).`}
                  </div>
                  {reconHarnessHint && (
                    <div className="tree-config-help" style={{ marginTop: 8 }}>{reconHarnessHint}</div>
                  )}
                  <div className="tree-config-help" style={{ marginTop: 8 }}>
                    Run Check Readiness applies transporter_config (reconciliation enabled + scheduler time), scheduler_job GRANT, Redis TransporterConfig cache flush for that city, then runs readiness queries.
                  </div>
                </div>
              </div>
            )}

            {node.id === 'seed-payment-settlement' && (
              <div className="tree-config">
                <div className="tree-config-field">
                  <label>Subscription Purchase ID</label>
                  <input
                    className="tree-config-input"
                    type="text"
                    placeholder="Required for PG payment settlement seeding"
                    value={subscriptionPurchaseId}
                    onChange={e => onSubscriptionPurchaseIdChange(e.target.value)}
                  />
                </div>
              </div>
            )}

            {/* Config area for clear-dues node */}
            {node.id === 'clear-dues' && (
              <div className="tree-config">
                <div className="tree-config-field">
                  <label>Card {paymentMethodsLoading ? '(loading...)' : `(${paymentMethods.length})`}</label>
                  <select
                    value={selectedPaymentMethodId}
                    onChange={e => onPaymentMethodChange(e.target.value)}
                    disabled={paymentMethodsLoading || paymentMethods.length === 0}
                  >
                    {paymentMethods.length === 0 && !paymentMethodsLoading && <option value="">No cards found</option>}
                    {paymentMethods.map(pm => (
                      <option key={pm.cardId} value={pm.cardId}>{pm.cardId}</option>
                    ))}
                  </select>
                </div>
              </div>
            )}

            {/* Steps */}
            <div className="tree-steps">
              {node.steps.map(step => (
                <StepRow key={step.id} step={step} result={results[step.id]} />
              ))}
            </div>

            {/* Node actions */}
            <div className="tree-node-actions">
              <button
                className="btn-run-node"
                onClick={() => onRunNode(node.id)}
                disabled={isRunning || isSkipped}
              >
                Run {node.title}
              </button>
              <button
                className={`btn-skip-node ${isSkipped ? 'active' : ''}`}
                onClick={() => onToggleSkipNode(node.id)}
                disabled={isRunning}
              >
                {isSkipped ? 'Unskip' : 'Skip'}
              </button>
            </div>
          </div>
        )}

        {/* Branch children (e.g. fulfillment has cancellation child) */}
        {isExpanded && node.children && (
          <div className="tree-branch">
            {node.children.map((child, ci) => renderNode(child, ci + 1, ci === node.children!.length - 1))}
          </div>
        )}
      </div>
    );
  };

  return (
    <div className="ride-flow-tree">
      {/* Flow selector */}
      <div className="flow-selector">
        {flows.map(f => (
          <button
            key={f.id}
            className={`flow-selector-btn ${f.id === activeFlowId ? 'active' : ''}`}
            onClick={() => onFlowChange(f.id)}
            disabled={isRunning}
            title={f.description}
          >
            {f.name}
          </button>
        ))}
      </div>

      {/* Pre-step: Make Driver Available (ride flow only) */}
      {showDriverSetup && (
        <>
          <div className={`tree-prestep ${driverAvailable ? 'available' : ''}`}>
            <div className="tree-prestep-header">
              <span className={`tree-node-dot ${driverAvailable ? 'node-pass' : 'node-pending'}`} />
              <span className="tree-prestep-title">Driver Setup</span>
              {driverAvailable && <span className="tree-node-check">Online</span>}
            </div>
            <div className="tree-prestep-body">
              <div className="tree-config">
                {renderLocationSelect('Driver Location', driverLocationIdx, onDriverLocationChange, locations)}
              </div>
              <button
                className={`btn-driver-available ${driverAvailable ? 'active' : ''}`}
                onClick={onMakeDriverAvailable}
                disabled={isRunning}
              >
                {driverAvailable ? 'Driver Online (pinging)' : 'Make Driver Available'}
              </button>
            </div>
            <div className="tree-connector" />
          </div>
        </>
      )}

      {/* Main flow tree */}
      {nodes.map((node, i) => {
        // Insert outcome tabs before the last node in ride flow
        const isOutcomeNode = activeFlowId === 'ride-flow' && i === nodes.length - 1;
        return (
          <React.Fragment key={node.id}>
            {isOutcomeNode && (
              <>
                <div className="tree-connector" />
                <div className="outcome-tabs">
                  {([
                    { id: 'fulfillment', label: 'Ride Fulfillment' },
                    { id: 'driver-cancel', label: 'Driver Cancel' },
                    { id: 'customer-cancel', label: 'Customer Cancel' },
                  ] as const).map(tab => (
                    <button
                      key={tab.id}
                      className={`outcome-tab ${selectedOutcome === tab.id ? 'active' : ''}`}
                      onClick={() => onOutcomeChange(tab.id)}
                      disabled={isRunning}
                    >
                      {tab.label}
                    </button>
                  ))}
                </div>
              </>
            )}
            {renderNode(node, i, i === nodes.length - 1)}
          </React.Fragment>
        );
      })}

      {/* Bottom actions */}
      <div className="tree-actions">
        <button className="btn-run-all" onClick={onRunAll} disabled={isRunning}>
          Run Full Flow
        </button>
        <button className="btn-stop" onClick={onStop} disabled={!isRunning}>
          Stop
        </button>
      </div>
    </div>
  );
};
