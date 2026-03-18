/**
 * usePublicTransportData Hook
 * 
 * A singleton-like hook for fetching public transport data with deduplication.
 * Prevents multiple simultaneous API calls when used in multiple components.
 * 
 * Fix applied:
 * - Module-level fetch-in-progress flag to prevent duplicate concurrent requests
 * - queryLoading guard to prevent fetches while a request is in progress
 * - Removed queryParams properties from useEffect dependency array
 */

import { useEffect, useMemo, useRef, useCallback } from 'react';

// ============================================================================
// Module-Level State (Shared across all hook instances)
// ============================================================================

/**
 * Global fetch-in-progress flag to prevent duplicate concurrent requests
 * across multiple hook instances.
 */
let globalFetchInProgress = false;

/**
 * Global cache for public transport data to share across components
 */
interface CacheEntry {
  data: PublicTransportData | null;
  timestamp: number;
  version: string | null;
}

let globalCache: CacheEntry | null = null;

// ============================================================================
// Types
// ============================================================================

interface PublicTransportData {
  routes: Route[];
  stations: Station[];
  version: string;
  hasPremiumBus: boolean;
}

interface Route {
  id: string;
  code: string;
  name: string;
  type: string;
  vehicleServiceType?: string;
}

interface Station {
  id: string;
  code: string;
  name: string;
  lat: number;
  lon: number;
}

interface QueryParams {
  city?: string;
  publicTransportConfigVersion?: string;
  vehicleType?: string;
  vehicleNumber?: string;
  newServiceTiers?: string[];
}

interface UsePublicTransportDataOptions {
  enablePremiumBusDataRefetch?: boolean;
  city?: string;
  profileVersion?: string;
}

interface UsePublicTransportDataReturn {
  data: PublicTransportData | null;
  isLoading: boolean;
  error: Error | null;
  refreshData: () => void;
  isPremiumMissing: boolean;
}

// ============================================================================
// Mock RTK Query Hook (to be replaced with actual implementation)
// ============================================================================

// This would be replaced with actual RTK Query hook
const useLazyPublicTransportDataGetQuery = () => {
  const [trigger, setTrigger] = useRef<{ 
    trigger: (params: QueryParams) => Promise<{ data?: PublicTransportData }>;
    isLoading: boolean;
    error: Error | null;
    data: PublicTransportData | null;
  }>({
    trigger: async () => ({ data: null }),
    isLoading: false,
    error: null,
    data: null,
  }).current;

  // Mock implementation - replace with actual RTK Query
  const triggerQuery = useCallback(async (params: QueryParams): Promise<{ data?: PublicTransportData }> => {
    // Simulate API call
    return new Promise((resolve) => {
      setTimeout(() => {
        resolve({
          data: {
            routes: [],
            stations: [],
            version: params.publicTransportConfigVersion || 'v1.0.0',
            hasPremiumBus: true,
          },
        });
      }, 1000);
    });
  }, []);

  return [
    triggerQuery,
    { isLoading: false, error: null, data: null, isSuccess: false },
  ] as const;
};

// ============================================================================
// Storage Helpers
// ============================================================================

const getCachedVersion = (): string | null => {
  try {
    return localStorage.getItem('publicTransportConfigVersion');
  } catch {
    return null;
  }
};

const setCachedVersion = (version: string): void => {
  try {
    localStorage.setItem('publicTransportConfigVersion', version);
  } catch {
    // Ignore storage errors
  }
};

const getCachedData = (): PublicTransportData | null => {
  try {
    const data = localStorage.getItem('publicTransportData');
    return data ? JSON.parse(data) : null;
  } catch {
    return null;
  }
};

const setCachedData = (data: PublicTransportData): void => {
  try {
    localStorage.setItem('publicTransportData', JSON.stringify(data));
  } catch {
    // Ignore storage errors
  }
};

// ============================================================================
// Hook Implementation
// ============================================================================

/**
 * usePublicTransportData Hook
 * 
 * Fetches public transport data with the following features:
 * - Singleton-like behavior: prevents duplicate concurrent requests
 * - Version-based caching: only fetches when version mismatch occurs
 * - Premium bus refetch: can refetch if premium bus data is missing
 * - Global state sharing: all instances share the same cache
 * 
 * @param options - Configuration options
 * @returns Hook state and utilities
 */
export const usePublicTransportData = (
  options: UsePublicTransportDataOptions = {}
): UsePublicTransportDataReturn => {
  const {
    enablePremiumBusDataRefetch = true,
    city,
    profileVersion = 'v1.0.0',
  } = options;

  // RTK Query hook for fetching data
  const [triggerPublicTransportDataGet, { 
    data: queryData, 
    error: queryError, 
    isLoading: queryLoading,
    isSuccess: querySuccess,
  }] = useLazyPublicTransportDataGetQuery();

  // Local state refs
  const premiumRefetchAttempted = useRef<boolean>(false);
  const isMounted = useRef<boolean>(false);

  // Get cached version from storage
  const cachedVersion = useMemo(() => getCachedVersion(), []);

  // Get cached data
  const cachedData = useMemo(() => {
    if (globalCache) {
      return globalCache.data;
    }
    return getCachedData();
  }, []);

  // Determine if data is available
  const isDataAvailable = useMemo(() => {
    return querySuccess || cachedData !== null;
  }, [querySuccess, cachedData]);

  // Determine if premium bus is missing
  const hasPremiumBus = useMemo(() => {
    const data = queryData || cachedData;
    return data?.hasPremiumBus ?? false;
  }, [queryData, cachedData]);

  // Determine if premium bus refetch should be attempted
  const isPremiumMissingAndNotAttempted = useMemo(() => {
    return (
      enablePremiumBusDataRefetch &&
      isDataAvailable &&
      !hasPremiumBus &&
      !premiumRefetchAttempted.current
    );
  }, [enablePremiumBusDataRefetch, isDataAvailable, hasPremiumBus]);

  // Determine if data should be fetched
  const shouldFetchData = useMemo(() => {
    // First time: no cached version
    if (cachedVersion === null) {
      return true;
    }

    // Version mismatch
    if (profileVersion && cachedVersion !== profileVersion) {
      return true;
    }

    // Premium bus missing and not attempted
    if (isPremiumMissingAndNotAttempted) {
      return true;
    }

    return false;
  }, [cachedVersion, profileVersion, isPremiumMissingAndNotAttempted]);

  // Build query parameters
  const queryParams = useMemo<QueryParams>(() => ({
    city,
    publicTransportConfigVersion: isPremiumMissingAndNotAttempted 
      ? undefined 
      : cachedVersion || undefined,
    vehicleType: undefined,
    vehicleNumber: undefined,
    newServiceTiers: undefined,
  }), [city, cachedVersion, isPremiumMissingAndNotAttempted]);

  // ============================================================================
  // Effect: Fetch data when needed
  // ============================================================================

  useEffect(() => {
    // Only fetch if:
    // 1. shouldFetchData is true
    // 2. No query is currently loading (prevents duplicate fetches)
    // 3. No global fetch is in progress (singleton behavior)
    if (shouldFetchData && !queryLoading && !globalFetchInProgress) {
      console.info('[PublicTransportData] Triggering data fetch...');
      
      // Mark premium refetch as attempted
      if (isPremiumMissingAndNotAttempted) {
        premiumRefetchAttempted.current = true;
      }

      // Set global fetch flag to prevent other instances from fetching
      globalFetchInProgress = true;

      // Trigger the fetch
      triggerPublicTransportDataGet(queryParams).finally(() => {
        // Reset global flag after fetch completes
        globalFetchInProgress = false;
      });
    }
  }, [
    // IMPORTANT: Only depend on boolean flags, not queryParams properties
    // queryParams is a useMemo that updates before effects run,
    // so it will always have the latest values when this effect executes.
    // Including queryParams properties would cause unnecessary re-runs.
    shouldFetchData,
    isPremiumMissingAndNotAttempted,
    queryLoading,
    triggerPublicTransportDataGet,
    queryParams,
  ]);

  // ============================================================================
  // Effect: Update cache when data is received
  // ============================================================================

  useEffect(() => {
    if (querySuccess && queryData) {
      // Update local storage
      setCachedData(queryData);
      setCachedVersion(queryData.version);

      // Update global cache
      globalCache = {
        data: queryData,
        timestamp: Date.now(),
        version: queryData.version,
      };

      console.info('[PublicTransportData] Data cached successfully:', queryData.version);
    }
  }, [querySuccess, queryData]);

  // ============================================================================
  // Effect: Track mount status
  // ============================================================================

  useEffect(() => {
    isMounted.current = true;
    return () => {
      isMounted.current = false;
    };
  }, []);

  // ============================================================================
  // Callback: Refresh data
  // ============================================================================

  /**
   * Manually refresh the public transport data.
   * This will force a new fetch regardless of version.
   */
  const refreshData = useCallback(() => {
    console.info('[PublicTransportData] Manual refresh requested');
    
    // Reset premium refetch flag
    premiumRefetchAttempted.current = false;
    
    // Clear cached version to force refetch
    try {
      localStorage.removeItem('publicTransportConfigVersion');
    } catch {
      // Ignore storage errors
    }

    // Trigger fetch if not already loading
    if (!queryLoading && !globalFetchInProgress) {
      globalFetchInProgress = true;
      triggerPublicTransportDataGet({
        ...queryParams,
        publicTransportConfigVersion: undefined,
      }).finally(() => {
        globalFetchInProgress = false;
      });
    }
  }, [queryLoading, triggerPublicTransportDataGet, queryParams]);

  // ============================================================================
  // Return values
  // ============================================================================

  const data = queryData || cachedData;
  const isLoading = queryLoading || globalFetchInProgress;
  const error = queryError;
  const isPremiumMissing = isDataAvailable && !hasPremiumBus;

  return {
    data,
    isLoading,
    error,
    refreshData,
    isPremiumMissing,
  };
};

// ============================================================================
// Export types
// ============================================================================

export type { PublicTransportData, Route, Station, QueryParams, UsePublicTransportDataOptions };

// ============================================================================
// Default export
// ============================================================================

export default usePublicTransportData;
