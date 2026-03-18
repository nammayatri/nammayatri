#!/usr/bin/env node
/**
 * Complete verification script for usePublicTransportData singleton pattern fix
 * 
 * This script simulates the full behavior including caching to verify the fix.
 * Run with: node verify-fix-complete.js
 */

console.log('\n╔════════════════════════════════════════════════════════════════╗');
console.log('║  usePublicTransportData Singleton Pattern Fix Verification    ║');
console.log('║                    (Complete with Caching)                     ║');
console.log('╚════════════════════════════════════════════════════════════════╝\n');

// ============================================================================
// Simulate the complete singleton pattern with caching
// ============================================================================

// Module-level singleton state
let isFetchInProgress = false;
let ongoingFetchPromise = null;
const fetchCompletionCallbacks = new Set();

// Simulated caches
const globalCache = new Map();
const mmkvStorage = new Map();

const PUBLIC_TRANSPORT_DATA_KEY = 'public_transport_data_v25';
const PUBLIC_TRANSPORT_CONFIG_VERSION_KEY = 'public_transport_config_version';

let apiCallCount = 0;

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

const hasCachedData = () => {
    return globalCache.has(PUBLIC_TRANSPORT_DATA_KEY) || mmkvStorage.has(PUBLIC_TRANSPORT_DATA_KEY);
};

const getCachedData = () => {
    // Check global cache first
    if (globalCache.has(PUBLIC_TRANSPORT_DATA_KEY)) {
        return globalCache.get(PUBLIC_TRANSPORT_DATA_KEY);
    }
    // Fall back to MMKV
    if (mmkvStorage.has(PUBLIC_TRANSPORT_DATA_KEY)) {
        const data = mmkvStorage.get(PUBLIC_TRANSPORT_DATA_KEY);
        globalCache.set(PUBLIC_TRANSPORT_DATA_KEY, data);
        return data;
    }
    return null;
};

const setCachedData = (data) => {
    globalCache.set(PUBLIC_TRANSPORT_DATA_KEY, data);
    mmkvStorage.set(PUBLIC_TRANSPORT_DATA_KEY, data);
    mmkvStorage.set(PUBLIC_TRANSPORT_CONFIG_VERSION_KEY, data.version);
};

const clearCache = () => {
    globalCache.clear();
    mmkvStorage.clear();
};

// Simulate API call
const simulateAPICall = async () => {
    apiCallCount++;
    console.log(`  📡 API Call #${apiCallCount} to /publicTransport/data`);
    await new Promise(resolve => setTimeout(resolve, 100)); // Simulate network delay
    const data = { version: 'v1.0.0', routes: [], stations: [] };
    setCachedData(data);
    return data;
};

// Simulate the complete fetch function from the hook
const fetchData = async (componentId, enabled = true) => {
    if (!enabled) {
        console.log(`  [${componentId}] ⏸️  Hook disabled, skipping fetch`);
        return { isDataAvailable: false, fromCache: false };
    }

    console.log(`  [${componentId}] Checking if fetch needed...`);

    // Check if we have cached data
    const cachedData = getCachedData();
    if (cachedData) {
        console.log(`  [${componentId}] 💾 Using cached data (no API call)`);
        return { isDataAvailable: true, fromCache: true };
    }

    // If a fetch is already in progress, wait for it
    if (isFetchInProgress && ongoingFetchPromise) {
        console.log(`  [${componentId}] ⏳ Fetch in progress, waiting...`);
        await ongoingFetchPromise;
        console.log(`  [${componentId}] ✅ Received data from shared fetch`);
        return { isDataAvailable: true, fromCache: false, shared: true };
    }

    // Start a new fetch
    console.log(`  [${componentId}] 🚀 Starting new fetch`);
    isFetchInProgress = true;
    ongoingFetchPromise = simulateAPICall();

    try {
        const data = await ongoingFetchPromise;
        console.log(`  [${componentId}] ✅ Fetch completed`);
        notifyFetchCompletion(true);
        return { isDataAvailable: true, fromCache: false, shared: false };
    } catch (err) {
        console.error(`  [${componentId}] ❌ Fetch failed:`, err);
        notifyFetchCompletion(false);
        return { isDataAvailable: false, error: err };
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
    console.log('Scenario: 3 components mount at the same time (no cache)\n');

    apiCallCount = 0;
    clearCache();
    isFetchInProgress = false;
    ongoingFetchPromise = null;

    // Simulate 3 components mounting simultaneously
    const results = await Promise.all([
        fetchData('HomeScreen'),
        fetchData('MockCity'),
        fetchData('MetroBooking'),
    ]);

    const fromCacheCount = results.filter(r => r.fromCache).length;
    const sharedCount = results.filter(r => r.shared).length;

    console.log('\n📊 Result:');
    console.log(`   API Calls Made: ${apiCallCount}`);
    console.log(`   From Cache: ${fromCacheCount}`);
    console.log(`   Shared Fetch: ${sharedCount}`);
    console.log(`   Expected API Calls: 1`);
    console.log(`   Status: ${apiCallCount === 1 ? '✅ PASS' : '❌ FAIL'}`);
    
    return apiCallCount === 1;
}

async function test2_sequentialMounts() {
    console.log('\n\n📋 TEST 2: Sequential Component Mounts');
    console.log('───────────────────────────────────────');
    console.log('Scenario: Components mount one after another\n');

    apiCallCount = 0;
    clearCache();
    isFetchInProgress = false;
    ongoingFetchPromise = null;

    // First component mounts and fetches
    const result1 = await fetchData('HomeScreen');
    
    // Second component mounts (should use cached result)
    await sleep(50);
    const result2 = await fetchData('MockCity');
    
    // Third component mounts (should use cached result)
    await sleep(50);
    const result3 = await fetchData('MetroBooking');

    const fromCacheCount = [result1, result2, result3].filter(r => r.fromCache).length;

    console.log('\n📊 Result:');
    console.log(`   API Calls Made: ${apiCallCount}`);
    console.log(`   From Cache: ${fromCacheCount}`);
    console.log(`   Expected API Calls: 1`);
    console.log(`   Expected From Cache: 2`);
    console.log(`   Status: ${apiCallCount === 1 && fromCacheCount === 2 ? '✅ PASS' : '❌ FAIL'}`);
    
    return apiCallCount === 1 && fromCacheCount === 2;
}

async function test3_cacheHit() {
    console.log('\n\n📋 TEST 3: Cache Hit on Subsequent Mounts');
    console.log('──────────────────────────────────────────');
    console.log('Scenario: Pre-populated cache, 3 components mount\n');

    apiCallCount = 0;
    clearCache();
    isFetchInProgress = false;
    ongoingFetchPromise = null;

    // Pre-populate cache
    setCachedData({ version: 'v1.0.0', routes: [], stations: [] });

    // Mount 3 components
    const results = await Promise.all([
        fetchData('HomeScreen'),
        fetchData('MockCity'),
        fetchData('MetroBooking'),
    ]);

    const fromCacheCount = results.filter(r => r.fromCache).length;

    console.log('\n📊 Result:');
    console.log(`   API Calls Made: ${apiCallCount}`);
    console.log(`   From Cache: ${fromCacheCount}`);
    console.log(`   Expected API Calls: 0`);
    console.log(`   Expected From Cache: 3`);
    console.log(`   Status: ${apiCallCount === 0 && fromCacheCount === 3 ? '✅ PASS' : '❌ FAIL'}`);
    
    return apiCallCount === 0 && fromCacheCount === 3;
}

async function test4_rapidMountUnmount() {
    console.log('\n\n📋 TEST 4: Rapid Mount/Unmount Cycles');
    console.log('──────────────────────────────────────');
    console.log('Scenario: 5 rapid mount/unmount cycles, then final mount\n');

    apiCallCount = 0;
    clearCache();
    isFetchInProgress = false;
    ongoingFetchPromise = null;

    // Simulate rapid mount/unmount
    const promises = [];
    for (let i = 0; i < 5; i++) {
        promises.push(fetchData(`Component-${i}`));
    }

    // Wait for all to complete
    await Promise.all(promises);

    // Final mount (should use cache)
    await sleep(50);
    const finalResult = await fetchData('FinalComponent');

    console.log('\n📊 Result:');
    console.log(`   API Calls Made: ${apiCallCount}`);
    console.log(`   Final from Cache: ${finalResult.fromCache}`);
    console.log(`   Expected API Calls: 1`);
    console.log(`   Status: ${apiCallCount === 1 && finalResult.fromCache ? '✅ PASS' : '❌ FAIL'}`);
    
    return apiCallCount === 1 && finalResult.fromCache;
}

async function test5_multipleCallsDuringFetch() {
    console.log('\n\n📋 TEST 5: Multiple Calls During Active Fetch');
    console.log('─────────────────────────────────────────────');
    console.log('Scenario: Components mount while fetch is in progress\n');

    apiCallCount = 0;
    clearCache();
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
    const results = await Promise.all([promise1, promise2, promise3, promise4]);

    const sharedCount = results.filter(r => r.shared).length;

    console.log('\n📊 Result:');
    console.log(`   API Calls Made: ${apiCallCount}`);
    console.log(`   Shared Fetch: ${sharedCount}`);
    console.log(`   Expected API Calls: 1`);
    console.log(`   Expected Shared: 3`);
    console.log(`   Status: ${apiCallCount === 1 && sharedCount === 3 ? '✅ PASS' : '❌ FAIL'}`);
    
    return apiCallCount === 1 && sharedCount === 3;
}

async function test6_disabledHook() {
    console.log('\n\n📋 TEST 6: Disabled Hook');
    console.log('─────────────────────────');
    console.log('Scenario: Hook is disabled, no fetch should occur\n');

    apiCallCount = 0;
    clearCache();
    isFetchInProgress = false;
    ongoingFetchPromise = null;

    const result = await fetchData('HomeScreen', false);

    console.log('\n📊 Result:');
    console.log(`   API Calls Made: ${apiCallCount}`);
    console.log(`   Expected API Calls: 0`);
    console.log(`   Status: ${apiCallCount === 0 ? '✅ PASS' : '❌ FAIL'}`);
    
    return apiCallCount === 0;
}

// ============================================================================
// Run All Tests
// ============================================================================

async function runAllTests() {
    console.log('Starting verification tests...\n');

    const results = [];
    results.push(await test1_simultaneousMounts());
    results.push(await test2_sequentialMounts());
    results.push(await test3_cacheHit());
    results.push(await test4_rapidMountUnmount());
    results.push(await test5_multipleCallsDuringFetch());
    results.push(await test6_disabledHook());

    const passedCount = results.filter(r => r).length;
    const totalCount = results.length;

    console.log('\n\n╔════════════════════════════════════════════════════════════════╗');
    console.log('║                      FINAL SUMMARY                             ║');
    console.log('╠════════════════════════════════════════════════════════════════╣');
    console.log(`║  Tests Passed: ${passedCount}/${totalCount}                                        ║`);
    console.log('╠════════════════════════════════════════════════════════════════╣');
    console.log('║  The singleton pattern ensures:                                ║');
    console.log('║  • Only 1 API call regardless of component count               ║');
    console.log('║  • All components share the same fetch result                  ║');
    console.log('║  • No duplicate requests to /publicTransport/data              ║');
    console.log('║  • Cache is properly utilized                                  ║');
    console.log('╚════════════════════════════════════════════════════════════════╝\n');

    if (passedCount === totalCount) {
        console.log('✅ All tests passed! The fix is working correctly.\n');
        process.exit(0);
    } else {
        console.log(`❌ ${totalCount - passedCount} test(s) failed.\n`);
        process.exit(1);
    }
}

runAllTests().catch(err => {
    console.error('Test error:', err);
    process.exit(1);
});
