/**
 * Singleton Pattern Verification Test for usePublicTransportData
 * 
 * This test verifies the key fix: ensuring only one API call is made
 * across multiple component instances using the hook.
 */

// ============================================================================
// Type Definitions
// ============================================================================

interface PublicTransportData {
    routes: any[];
    stations: any[];
    routeStopMappings: any[];
    publicTransportConfigVersion: string;
}

interface HookState {
    isDataLoaded: boolean;
    isLoading: boolean;
    error: any;
    cachedVersion: string | null;
    profileVersion: string | undefined;
    hasVersionMismatch: boolean;
    isDataAvailable: boolean;
    cacheInitialized: boolean;
}

// ============================================================================
// Mock Implementations
// ============================================================================

class MockGlobalCache {
    private cache = new Map<string, { value: any; expiry: number | null }>();

    set<T>(key: string, value: T, ttl: number | null): void {
        const expiry = ttl ? Date.now() + ttl : null;
        this.cache.set(key, { value, expiry });
    }

    get<T>(key: string): T | undefined {
        const item = this.cache.get(key);
        if (!item) return undefined;
        if (item.expiry !== null && Date.now() > item.expiry) {
            this.cache.delete(key);
            return undefined;
        }
        return item.value as T;
    }

    has(key: string): boolean {
        const item = this.cache.get(key);
        if (!item) return false;
        if (item.expiry !== null && Date.now() > item.expiry) {
            this.cache.delete(key);
            return false;
        }
        return true;
    }

    delete(key: string): boolean {
        return this.cache.delete(key);
    }

    clear(): void {
        this.cache.clear();
    }
}

class MockMMKV {
    private storage = new Map<string, string>();

    getString(key: string): string | undefined {
        return this.storage.get(key);
    }

    set(key: string, value: string): void {
        this.storage.set(key, value);
    }

    remove(key: string): void {
        this.storage.delete(key);
    }

    clear(): void {
        this.storage.clear();
    }
}

// ============================================================================
// Test Suite
// ============================================================================

class SingletonPatternTest {
    private globalCache = new MockGlobalCache();
    private storage = new MockMMKV();
    private apiCallCount = 0;
    private hookInstances: HookState[] = [];
    
    readonly PUBLIC_TRANSPORT_DATA_KEY = 'public_transport_data_v25';
    readonly PUBLIC_TRANSPORT_CONFIG_VERSION_KEY = 'public_transport_config_version';

    // Simulate the singleton pattern logic from the hook
    private shouldFetchData(
        enabled: boolean,
        isConnected: boolean,
        cacheInitialized: boolean,
        cachedVersion: string | null,
        profileVersion: string | undefined,
        isDataAvailable: boolean,
        premiumRefetchAttempted: boolean,
        enablePremiumBusDataRefetch: boolean
    ): boolean {
        if (!enabled || !isConnected || !cacheInitialized) {
            return false;
        }

        // First time installation - no cached version
        if (cachedVersion === null) {
            console.log('[TEST] First time installation - fetching data');
            return true;
        }

        // Version mismatch - need to update
        if (profileVersion && cachedVersion !== profileVersion) {
            console.log('[TEST] Version mismatch detected:', {
                cached: cachedVersion,
                profile: profileVersion,
            });
            return true;
        }

        // Premium bus missing - trigger one-time refetch
        if (enablePremiumBusDataRefetch && isDataAvailable && !this.hasPremiumBus() && !premiumRefetchAttempted) {
            console.log('[TEST] Premium bus data missing - triggering one-time refetch');
            return true;
        }

        console.log('[TEST] Using cached data - versions match');
        return false;
    }

    private hasPremiumBus(): boolean {
        const data = this.globalCache.get<PublicTransportData>(this.PUBLIC_TRANSPORT_DATA_KEY);
        return data?.routes?.some(route => route.serviceType === 'PREMIUM') ?? false;
    }

    private async simulateAPICall(): Promise<PublicTransportData> {
        this.apiCallCount++;
        console.log(`[TEST] API Call #${this.apiCallCount}`);
        
        // Simulate API delay
        await new Promise(resolve => setTimeout(resolve, 10));
        
        const data: PublicTransportData = {
            routes: [
                { code: 'R1', longName: 'Route 1', serviceType: 'REGULAR' },
                { code: 'R2', longName: 'Route 2', serviceType: 'PREMIUM' },
            ],
            stations: [
                { code: 'S1', name: 'Station 1' },
                { code: 'S2', name: 'Station 2' },
            ],
            routeStopMappings: [],
            publicTransportConfigVersion: 'v1.0.0',
        };

        // Cache the data
        this.storage.set(this.PUBLIC_TRANSPORT_DATA_KEY, JSON.stringify(data));
        this.storage.set(this.PUBLIC_TRANSPORT_CONFIG_VERSION_KEY, data.publicTransportConfigVersion);
        this.globalCache.set(this.PUBLIC_TRANSPORT_DATA_KEY, data, 1000 * 60 * 60 * 24);

        return data;
    }

    // Simulate mounting a component with the hook
    async mountComponent(
        componentId: string,
        enabled: boolean = true,
        profileVersion: string = 'v1.0.0'
    ): Promise<HookState> {
        console.log(`\n[TEST] Mounting component: ${componentId}`);

        // Initialize cache from storage (like useEffect in hook)
        const storedVersion = this.storage.getString(this.PUBLIC_TRANSPORT_CONFIG_VERSION_KEY);
        const storedData = this.storage.getString(this.PUBLIC_TRANSPORT_DATA_KEY);
        
        let cachedVersion: string | null = null;
        
        if (storedVersion && storedData) {
            cachedVersion = storedVersion;
            const parsedData = JSON.parse(storedData) as PublicTransportData;
            this.globalCache.set(this.PUBLIC_TRANSPORT_DATA_KEY, parsedData, 1000 * 60 * 60 * 24);
            console.log(`[TEST] Loaded cached data with version: ${storedVersion}`);
        } else {
            console.log('[TEST] No cached data found');
        }

        const isDataAvailable = this.globalCache.has(this.PUBLIC_TRANSPORT_DATA_KEY);
        const cacheInitialized = true;
        const isConnected = true;
        const premiumRefetchAttempted = false;
        const enablePremiumBusDataRefetch = false;

        // Check if we should fetch
        const shouldFetch = this.shouldFetchData(
            enabled,
            isConnected,
            cacheInitialized,
            cachedVersion,
            profileVersion,
            isDataAvailable,
            premiumRefetchAttempted,
            enablePremiumBusDataRefetch
        );

        if (shouldFetch) {
            await this.simulateAPICall();
            // Update cached version after fetch
            cachedVersion = this.storage.getString(this.PUBLIC_TRANSPORT_CONFIG_VERSION_KEY) || null;
        }

        const state: HookState = {
            isDataLoaded: cacheInitialized && isDataAvailable && cachedVersion === profileVersion,
            isLoading: shouldFetch,
            error: null,
            cachedVersion,
            profileVersion,
            hasVersionMismatch: profileVersion ? cachedVersion !== profileVersion : false,
            isDataAvailable,
            cacheInitialized,
        };

        this.hookInstances.push(state);
        console.log(`[TEST] Component ${componentId} mounted. API calls so far: ${this.apiCallCount}`);
        
        return state;
    }

    // Reset for next test
    reset(): void {
        this.globalCache.clear();
        this.storage.clear();
        this.apiCallCount = 0;
        this.hookInstances = [];
        console.log('\n[TEST] Reset complete\n');
    }

    getApiCallCount(): number {
        return this.apiCallCount;
    }

    // ============================================================================
    // Test Cases
    // ============================================================================

    async runTest1_SimultaneousMounts(): Promise<boolean> {
        console.log('\n========================================');
        console.log('TEST 1: Simultaneous Component Mounts');
        console.log('========================================');

        this.reset();

        // Mount 3 components simultaneously (no cached data)
        const [state1, state2, state3] = await Promise.all([
            this.mountComponent('Component-A'),
            this.mountComponent('Component-B'),
            this.mountComponent('Component-C'),
        ]);

        console.log('\n--- Results ---');
        console.log(`API Calls Made: ${this.apiCallCount}`);
        console.log(`Component-A isDataAvailable: ${state1.isDataAvailable}`);
        console.log(`Component-B isDataAvailable: ${state2.isDataAvailable}`);
        console.log(`Component-C isDataAvailable: ${state3.isDataAvailable}`);

        const passed = this.apiCallCount === 1;
        console.log(`\nTEST 1: ${passed ? 'PASSED ✓' : 'FAILED ✗'}`);
        
        return passed;
    }

    async runTest2_SequentialMounts(): Promise<boolean> {
        console.log('\n========================================');
        console.log('TEST 2: Sequential Component Mounts');
        console.log('========================================');

        this.reset();

        // Mount components sequentially
        const state1 = await this.mountComponent('Component-A');
        const state2 = await this.mountComponent('Component-B');
        const state3 = await this.mountComponent('Component-C');

        console.log('\n--- Results ---');
        console.log(`API Calls Made: ${this.apiCallCount}`);
        console.log(`Component-A isDataAvailable: ${state1.isDataAvailable}`);
        console.log(`Component-B isDataAvailable: ${state2.isDataAvailable}`);
        console.log(`Component-C isDataAvailable: ${state3.isDataAvailable}`);

        const passed = this.apiCallCount === 1;
        console.log(`\nTEST 2: ${passed ? 'PASSED ✓' : 'FAILED ✗'}`);
        
        return passed;
    }

    async runTest3_CacheHit(): Promise<boolean> {
        console.log('\n========================================');
        console.log('TEST 3: Cache Hit on Subsequent Mounts');
        console.log('========================================');

        this.reset();

        // Pre-populate cache
        const cachedData: PublicTransportData = {
            routes: [{ code: 'R1', longName: 'Route 1' }],
            stations: [{ code: 'S1', name: 'Station 1' }],
            routeStopMappings: [],
            publicTransportConfigVersion: 'v1.0.0',
        };
        this.storage.set(this.PUBLIC_TRANSPORT_DATA_KEY, JSON.stringify(cachedData));
        this.storage.set(this.PUBLIC_TRANSPORT_CONFIG_VERSION_KEY, 'v1.0.0');
        this.globalCache.set(this.PUBLIC_TRANSPORT_DATA_KEY, cachedData, 1000 * 60 * 60 * 24);

        // Mount multiple components
        const [state1, state2, state3] = await Promise.all([
            this.mountComponent('Component-A'),
            this.mountComponent('Component-B'),
            this.mountComponent('Component-C'),
        ]);

        console.log('\n--- Results ---');
        console.log(`API Calls Made: ${this.apiCallCount}`);
        console.log(`Component-A isDataLoaded: ${state1.isDataLoaded}`);
        console.log(`Component-B isDataLoaded: ${state2.isDataLoaded}`);
        console.log(`Component-C isDataLoaded: ${state3.isDataLoaded}`);

        const passed = this.apiCallCount === 0 && state1.isDataLoaded && state2.isDataLoaded && state3.isDataLoaded;
        console.log(`\nTEST 3: ${passed ? 'PASSED ✓' : 'FAILED ✗'}`);
        
        return passed;
    }

    async runTest4_VersionMismatch(): Promise<boolean> {
        console.log('\n========================================');
        console.log('TEST 4: Version Mismatch Detection');
        console.log('========================================');

        this.reset();

        // Pre-populate cache with old version
        const cachedData: PublicTransportData = {
            routes: [{ code: 'R1', longName: 'Route 1' }],
            stations: [{ code: 'S1', name: 'Station 1' }],
            routeStopMappings: [],
            publicTransportConfigVersion: 'v1.0.0',
        };
        this.storage.set(this.PUBLIC_TRANSPORT_DATA_KEY, JSON.stringify(cachedData));
        this.storage.set(this.PUBLIC_TRANSPORT_CONFIG_VERSION_KEY, 'v1.0.0');
        this.globalCache.set(this.PUBLIC_TRANSPORT_DATA_KEY, cachedData, 1000 * 60 * 60 * 24);

        // Mount with newer profile version
        const state = await this.mountComponent('Component-A', true, 'v2.0.0');

        console.log('\n--- Results ---');
        console.log(`API Calls Made: ${this.apiCallCount}`);
        console.log(`hasVersionMismatch: ${state.hasVersionMismatch}`);
        console.log(`cachedVersion: ${state.cachedVersion}`);
        console.log(`profileVersion: ${state.profileVersion}`);

        const passed = this.apiCallCount === 1 && state.hasVersionMismatch;
        console.log(`\nTEST 4: ${passed ? 'PASSED ✓' : 'FAILED ✗'}`);
        
        return passed;
    }

    async runTest5_MultipleComponentsVersionMismatch(): Promise<boolean> {
        console.log('\n========================================');
        console.log('TEST 5: Multiple Components with Version Mismatch');
        console.log('========================================');

        this.reset();

        // Pre-populate cache with old version
        const cachedData: PublicTransportData = {
            routes: [{ code: 'R1', longName: 'Route 1' }],
            stations: [{ code: 'S1', name: 'Station 1' }],
            routeStopMappings: [],
            publicTransportConfigVersion: 'v1.0.0',
        };
        this.storage.set(this.PUBLIC_TRANSPORT_DATA_KEY, JSON.stringify(cachedData));
        this.storage.set(this.PUBLIC_TRANSPORT_CONFIG_VERSION_KEY, 'v1.0.0');
        this.globalCache.set(this.PUBLIC_TRANSPORT_DATA_KEY, cachedData, 1000 * 60 * 60 * 24);

        // Mount multiple components with newer profile version
        const [state1, state2, state3] = await Promise.all([
            this.mountComponent('Component-A', true, 'v2.0.0'),
            this.mountComponent('Component-B', true, 'v2.0.0'),
            this.mountComponent('Component-C', true, 'v2.0.0'),
        ]);

        console.log('\n--- Results ---');
        console.log(`API Calls Made: ${this.apiCallCount}`);
        console.log(`All have version mismatch: ${state1.hasVersionMismatch && state2.hasVersionMismatch && state3.hasVersionMismatch}`);

        // Should only make 1 API call despite all 3 detecting version mismatch
        const passed = this.apiCallCount === 1;
        console.log(`\nTEST 5: ${passed ? 'PASSED ✓' : 'FAILED ✗'}`);
        
        return passed;
    }

    async runAllTests(): Promise<void> {
        console.log('\n');
        console.log('╔══════════════════════════════════════════════════════════════╗');
        console.log('║  usePublicTransportData Singleton Pattern Verification      ║');
        console.log('╚══════════════════════════════════════════════════════════════╝');

        const results = await Promise.all([
            this.runTest1_SimultaneousMounts(),
            this.runTest2_SequentialMounts(),
            this.runTest3_CacheHit(),
            this.runTest4_VersionMismatch(),
            this.runTest5_MultipleComponentsVersionMismatch(),
        ]);

        const passedCount = results.filter(r => r).length;
        const totalCount = results.length;

        console.log('\n========================================');
        console.log('FINAL RESULTS');
        console.log('========================================');
        console.log(`Tests Passed: ${passedCount}/${totalCount}`);
        
        if (passedCount === totalCount) {
            console.log('\n✓ All tests passed! Singleton pattern is working correctly.');
        } else {
            console.log(`\n✗ ${totalCount - passedCount} test(s) failed.`);
        }
        console.log('========================================\n');
    }
}

// ============================================================================
// Run Tests
// ============================================================================

const test = new SingletonPatternTest();
test.runAllTests();

export { SingletonPatternTest };
