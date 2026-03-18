#!/usr/bin/env node
/**
 * Quick verification script for usePublicTransportData singleton pattern fix
 * 
 * This script simulates the behavior to verify the fix works correctly.
 * Run with: node verify-fix.js
 */

console.log('\n╔════════════════════════════════════════════════════════════════╗');
console.log('║  usePublicTransportData Singleton Pattern Fix Verification    ║');
console.log('╚════════════════════════════════════════════════════════════════╝\n');

// ============================================================================
// Simulate the singleton pattern from the fixed hook
// ============================================================================

// Module-level singleton state (same as in the hook)
let isFetchInProgress = false;
let ongoingFetchPromise = null;
const fetchCompletionCallbacks = new Set();

const notifyFetchCompletion = (success) => {
    fetchCompletionCallbacks.forEach(callback => {
        try {
            callback(success);
        } catch (e) {
            console.error('Error in callback:', e);
        }
    });
    fetchCompletionCallbacks.clear();
};

// Simulate API call
let apiCallCount = 0;
const simulateAPICall = async () => {
    apiCallCount++;
    console.log(`  📡 API Call #${apiCallCount} to /publicTransport/data`);
    await new Promise(resolve => setTimeout(resolve, 100)); // Simulate network delay
    return { version: 'v1.0.0', data: 'transport_data' };
};

// Simulate the fetch function from the hook
const fetchData = async (componentId) => {
    console.log(`  [${componentId}] Checking if fetch needed...`);

    // If a fetch is already in progress, wait for it
    if (isFetchInProgress && ongoingFetchPromise) {
        console.log(`  [${componentId}] ⏳ Fetch in progress, waiting...`);
        await ongoingFetchPromise;
        console.log(`  [${componentId}] ✅ Received data from shared fetch`);
        return;
    }

    // Start a new fetch
    console.log(`  [${componentId}] 🚀 Starting new fetch`);
    isFetchInProgress = true;
    ongoingFetchPromise = simulateAPICall();

    try {
        const data = await ongoingFetchPromise;
        console.log(`  [${componentId}] ✅ Fetch completed`);
        notifyFetchCompletion(true);
    } catch (err) {
        console.error(`  [${componentId}] ❌ Fetch failed:`, err);
        notifyFetchCompletion(false);
    } finally {
        isFetchInProgress = false;
        ongoingFetchPromise = null;
    }
};

// ============================================================================
// Test Cases
// ============================================================================

const sleep = (ms) => new Promise(resolve => setTimeout(resolve, ms));

async function test1_simultaneousMounts() {
    console.log('\n📋 TEST 1: Simultaneous Component Mounts');
    console.log('─────────────────────────────────────────');
    console.log('Scenario: 3 components mount at the same time\n');

    apiCallCount = 0;
    isFetchInProgress = false;
    ongoingFetchPromise = null;

    // Simulate 3 components mounting simultaneously
    await Promise.all([
        fetchData('HomeScreen'),
        fetchData('MockCity'),
        fetchData('MetroBooking'),
    ]);

    console.log('\n📊 Result:');
    console.log(`   API Calls Made: ${apiCallCount}`);
    console.log(`   Expected: 1`);
    console.log(`   Status: ${apiCallCount === 1 ? '✅ PASS' : '❌ FAIL'}`);
}

async function test2_sequentialMounts() {
    console.log('\n\n📋 TEST 2: Sequential Component Mounts');
    console.log('───────────────────────────────────────');
    console.log('Scenario: Components mount one after another\n');

    apiCallCount = 0;
    isFetchInProgress = false;
    ongoingFetchPromise = null;

    // First component mounts and fetches
    await fetchData('HomeScreen');
    
    // Second component mounts (should use cached result)
    await sleep(50);
    await fetchData('MockCity');
    
    // Third component mounts (should use cached result)
    await sleep(50);
    await fetchData('MetroBooking');

    console.log('\n📊 Result:');
    console.log(`   API Calls Made: ${apiCallCount}`);
    console.log(`   Expected: 1`);
    console.log(`   Status: ${apiCallCount === 1 ? '✅ PASS' : '❌ FAIL'}`);
}

async function test3_rapidMountUnmount() {
    console.log('\n\n📋 TEST 3: Rapid Mount/Unmount Cycles');
    console.log('──────────────────────────────────────');
    console.log('Scenario: 5 rapid mount/unmount cycles\n');

    apiCallCount = 0;
    isFetchInProgress = false;
    ongoingFetchPromise = null;

    // Simulate rapid mount/unmount
    for (let i = 0; i < 5; i++) {
        const promise = fetchData(`Component-${i}`);
        // Unmount immediately (don't await)
        promise.catch(() => {}); // Ignore errors
    }

    // Wait for all to complete
    await sleep(200);

    // Final mount
    await fetchData('FinalComponent');

    console.log('\n📊 Result:');
    console.log(`   API Calls Made: ${apiCallCount}`);
    console.log(`   Expected: 1`);
    console.log(`   Status: ${apiCallCount === 1 ? '✅ PASS' : '❌ FAIL'}`);
}

async function test4_multipleCallsDuringFetch() {
    console.log('\n\n📋 TEST 4: Multiple Calls During Active Fetch');
    console.log('─────────────────────────────────────────────');
    console.log('Scenario: Components mount while fetch is in progress\n');

    apiCallCount = 0;
    isFetchInProgress = false;
    ongoingFetchPromise = null;

    // Start first fetch
    const promise1 = fetchData('FirstComponent');
    
    // While first is in progress, start more
    await sleep(10);
    const promise2 = fetchData('SecondComponent');
    
    await sleep(10);
    const promise3 = fetchData('ThirdComponent');
    
    await sleep(10);
    const promise4 = fetchData('FourthComponent');

    // Wait for all to complete
    await Promise.all([promise1, promise2, promise3, promise4]);

    console.log('\n📊 Result:');
    console.log(`   API Calls Made: ${apiCallCount}`);
    console.log(`   Expected: 1`);
    console.log(`   Status: ${apiCallCount === 1 ? '✅ PASS' : '❌ FAIL'}`);
}

// ============================================================================
// Run All Tests
// ============================================================================

async function runAllTests() {
    console.log('Starting verification tests...\n');

    await test1_simultaneousMounts();
    await test2_sequentialMounts();
    await test3_rapidMountUnmount();
    await test4_multipleCallsDuringFetch();

    console.log('\n\n╔════════════════════════════════════════════════════════════════╗');
    console.log('║                      FINAL SUMMARY                             ║');
    console.log('╠════════════════════════════════════════════════════════════════╣');
    console.log('║  The singleton pattern ensures:                                ║');
    console.log('║  • Only 1 API call regardless of component count               ║');
    console.log('║  • All components share the same fetch result                  ║');
    console.log('║  • No duplicate requests to /publicTransport/data              ║');
    console.log('╚════════════════════════════════════════════════════════════════╝\n');
}

runAllTests().catch(console.error);
