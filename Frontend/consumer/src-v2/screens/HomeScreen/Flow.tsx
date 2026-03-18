/**
 * HomeScreen Flow Component
 * 
 * This is the main home screen component that uses the usePublicTransportData hook.
 * 
 * Fix applied: Reviewed and optimized refreshData() usage on city change.
 * The hook now handles city change internally, so we don't need manual refresh calls.
 */

import React, { useEffect, useState, useCallback } from 'react';
import {
  View,
  Text,
  StyleSheet,
  TouchableOpacity,
  ScrollView,
  RefreshControl,
  ActivityIndicator,
} from 'react-native';
import { usePublicTransportData } from '../../multimodal/hooks/usePublicTransportData';

// ============================================================================
// Types
// ============================================================================

interface City {
  id: string;
  name: string;
  code: string;
}

// ============================================================================
// Constants
// ============================================================================

const AVAILABLE_CITIES: City[] = [
  { id: '1', name: 'Bangalore', code: 'bangalore' },
  { id: '2', name: 'Delhi', code: 'delhi' },
  { id: '3', name: 'Mumbai', code: 'mumbai' },
  { id: '4', name: 'Chennai', code: 'chennai' },
  { id: '5', name: 'Hyderabad', code: 'hyderabad' },
];

// ============================================================================
// Component
// ============================================================================

export const HomeScreenFlow: React.FC = () => {
  // ============================================================================
  // State
  // ============================================================================

  const [selectedCity, setSelectedCity] = useState<City>(AVAILABLE_CITIES[0]);
  const [showCitySelector, setShowCitySelector] = useState<boolean>(false);
  const [manualRefreshing, setManualRefreshing] = useState<boolean>(false);

  // ============================================================================
  // Hooks
  // ============================================================================

  // Use the public transport data hook with city parameter
  // The hook will automatically refetch when the city changes
  const {
    data: publicTransportData,
    isLoading,
    error,
    refreshData,
    isPremiumMissing,
  } = usePublicTransportData({
    enablePremiumBusDataRefetch: true,
    city: selectedCity.code,
    profileVersion: 'v1.0.0',
  });

  // ============================================================================
  // Handlers
  // ============================================================================

  /**
   * Handle city selection
   * This will trigger a refetch via the hook's internal logic
   */
  const handleCitySelect = useCallback((city: City) => {
    console.log('[HomeScreen] City selected:', city.name);
    setSelectedCity(city);
    setShowCitySelector(false);
    // Note: No need to call refreshData() here
    // The hook will automatically detect the city change and refetch
  }, []);

  /**
   * Handle manual refresh (pull-to-refresh)
   * This is the appropriate place to call refreshData()
   */
  const handleManualRefresh = useCallback(async () => {
    console.log('[HomeScreen] Manual refresh triggered');
    setManualRefreshing(true);
    
    // Call refreshData for explicit user-initiated refresh
    refreshData();
    
    // Simulate delay for UX
    setTimeout(() => {
      setManualRefreshing(false);
    }, 1000);
  }, [refreshData]);

  // ============================================================================
  // Effects
  // ============================================================================

  /**
   * Log when data changes
   */
  useEffect(() => {
    if (publicTransportData) {
      console.log('[HomeScreen] Public transport data updated:', {
        version: publicTransportData.version,
        routesCount: publicTransportData.routes.length,
        stationsCount: publicTransportData.stations.length,
        hasPremiumBus: publicTransportData.hasPremiumBus,
      });
    }
  }, [publicTransportData]);

  /**
   * Log errors
   */
  useEffect(() => {
    if (error) {
      console.error('[HomeScreen] Error loading public transport data:', error);
    }
  }, [error]);

  // ============================================================================
  // REMOVED: Redundant city change effect
  // ============================================================================

  /*
   * BEFORE (caused duplicate API calls):
   * 
   * useEffect(() => {
   *   // This was redundant because:
   *   // 1. The hook already watches the city parameter
   *   // 2. The hook's internal useEffect triggers the fetch
   *   // 3. Calling refreshData() here caused a duplicate fetch
   *   refreshData();
   * }, [selectedCity, refreshData]);
   */

  /*
   * CORRECT BEHAVIOR:
   * 
   * 1. User selects a new city
   * 2. setSelectedCity() is called
   * 3. Component re-renders with new city
   * 4. usePublicTransportData hook receives new city parameter
   * 5. Hook's internal useMemo for queryParams updates
   * 6. Hook's internal useEffect detects shouldFetchData = true
   * 7. Hook triggers fetch (if not already loading)
   * 
   * This ensures only ONE fetch occurs per city change.
   */

  // ============================================================================
  // Render Helpers
  // ============================================================================

  const renderCitySelector = () => (
    <View style={styles.citySelectorOverlay}>
      <View style={styles.citySelectorContainer}>
        <Text style={styles.citySelectorTitle}>Select City</Text>
        <ScrollView>
          {AVAILABLE_CITIES.map((city) => (
            <TouchableOpacity
              key={city.id}
              style={[
                styles.cityOption,
                city.id === selectedCity.id && styles.cityOptionSelected,
              ]}
              onPress={() => handleCitySelect(city)}
            >
              <Text
                style={[
                  styles.cityOptionText,
                  city.id === selectedCity.id && styles.cityOptionTextSelected,
                ]}
              >
                {city.name}
              </Text>
            </TouchableOpacity>
          ))}
        </ScrollView>
        <TouchableOpacity
          style={styles.closeButton}
          onPress={() => setShowCitySelector(false)}
        >
          <Text style={styles.closeButtonText}>Cancel</Text>
        </TouchableOpacity>
      </View>
    </View>
  );

  const renderHeader = () => (
    <View style={styles.header}>
      <Text style={styles.headerTitle}>Public Transport</Text>
      <TouchableOpacity
        style={styles.cityButton}
        onPress={() => setShowCitySelector(true)}
      >
        <Text style={styles.cityButtonText}>{selectedCity.name}</Text>
        <Text style={styles.cityButtonIcon}>▼</Text>
      </TouchableOpacity>
    </View>
  );

  const renderContent = () => {
    if (isLoading && !publicTransportData) {
      return (
        <View style={styles.loadingContainer}>
          <ActivityIndicator size="large" color="#3498db" />
          <Text style={styles.loadingText}>Loading transport data...</Text>
        </View>
      );
    }

    if (error && !publicTransportData) {
      return (
        <View style={styles.errorContainer}>
          <Text style={styles.errorText}>Failed to load transport data</Text>
          <TouchableOpacity style={styles.retryButton} onPress={refreshData}>
            <Text style={styles.retryButtonText}>Retry</Text>
          </TouchableOpacity>
        </View>
      );
    }

    return (
      <ScrollView
        style={styles.content}
        refreshControl={
          <RefreshControl
            refreshing={manualRefreshing}
            onRefresh={handleManualRefresh}
            colors={['#3498db']}
          />
        }
      >
        {/* Data Version Info */}
        <View style={styles.infoCard}>
          <Text style={styles.infoLabel}>Data Version</Text>
          <Text style={styles.infoValue}>
            {publicTransportData?.version || 'N/A'}
          </Text>
        </View>

        {/* Premium Bus Status */}
        {isPremiumMissing && (
          <View style={styles.warningCard}>
            <Text style={styles.warningText}>
              ⚠️ Premium bus data is being updated. Please check back later.
            </Text>
          </View>
        )}

        {/* Routes Summary */}
        <View style={styles.section}>
          <Text style={styles.sectionTitle}>Available Routes</Text>
          <Text style={styles.sectionCount}>
            {publicTransportData?.routes.length || 0} routes
          </Text>
        </View>

        {/* Stations Summary */}
        <View style={styles.section}>
          <Text style={styles.sectionTitle}>Stations</Text>
          <Text style={styles.sectionCount}>
            {publicTransportData?.stations.length || 0} stations
          </Text>
        </View>

        {/* Quick Actions */}
        <View style={styles.quickActions}>
          <TouchableOpacity style={styles.actionButton}>
            <Text style={styles.actionButtonText}>Book Metro Ticket</Text>
          </TouchableOpacity>
          <TouchableOpacity style={styles.actionButton}>
            <Text style={styles.actionButtonText}>View Bus Routes</Text>
          </TouchableOpacity>
        </View>
      </ScrollView>
    );
  };

  // ============================================================================
  // Render
  // ============================================================================

  return (
    <View style={styles.screen}>
      {renderHeader()}
      {renderContent()}
      {showCitySelector && renderCitySelector()}
    </View>
  );
};

// ============================================================================
// Styles
// ============================================================================

const styles = StyleSheet.create({
  screen: {
    flex: 1,
    backgroundColor: '#f5f5f5',
  },
  header: {
    backgroundColor: '#3498db',
    padding: 16,
    paddingTop: 48,
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
  },
  headerTitle: {
    fontSize: 20,
    fontWeight: 'bold',
    color: '#fff',
  },
  cityButton: {
    flexDirection: 'row',
    alignItems: 'center',
    backgroundColor: 'rgba(255,255,255,0.2)',
    paddingHorizontal: 12,
    paddingVertical: 6,
    borderRadius: 16,
  },
  cityButtonText: {
    color: '#fff',
    fontSize: 14,
    fontWeight: '600',
  },
  cityButtonIcon: {
    color: '#fff',
    fontSize: 10,
    marginLeft: 4,
  },
  content: {
    flex: 1,
    padding: 16,
  },
  loadingContainer: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
  },
  loadingText: {
    marginTop: 16,
    fontSize: 16,
    color: '#666',
  },
  errorContainer: {
    flex: 1,
    justifyContent: 'center',
    alignItems: 'center',
    padding: 32,
  },
  errorText: {
    fontSize: 16,
    color: '#e74c3c',
    marginBottom: 16,
    textAlign: 'center',
  },
  retryButton: {
    backgroundColor: '#3498db',
    paddingHorizontal: 24,
    paddingVertical: 12,
    borderRadius: 8,
  },
  retryButtonText: {
    color: '#fff',
    fontSize: 16,
    fontWeight: '600',
  },
  infoCard: {
    backgroundColor: '#fff',
    padding: 16,
    borderRadius: 8,
    marginBottom: 12,
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
  },
  infoLabel: {
    fontSize: 14,
    color: '#666',
  },
  infoValue: {
    fontSize: 14,
    fontWeight: '600',
    color: '#333',
  },
  warningCard: {
    backgroundColor: '#fff3cd',
    padding: 16,
    borderRadius: 8,
    marginBottom: 12,
    borderLeftWidth: 4,
    borderLeftColor: '#ffc107',
  },
  warningText: {
    fontSize: 14,
    color: '#856404',
  },
  section: {
    backgroundColor: '#fff',
    padding: 16,
    borderRadius: 8,
    marginBottom: 12,
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'center',
  },
  sectionTitle: {
    fontSize: 16,
    fontWeight: '600',
    color: '#333',
  },
  sectionCount: {
    fontSize: 14,
    color: '#666',
  },
  quickActions: {
    marginTop: 8,
  },
  actionButton: {
    backgroundColor: '#27ae60',
    padding: 16,
    borderRadius: 8,
    marginBottom: 12,
    alignItems: 'center',
  },
  actionButtonText: {
    color: '#fff',
    fontSize: 16,
    fontWeight: '600',
  },
  citySelectorOverlay: {
    position: 'absolute',
    top: 0,
    left: 0,
    right: 0,
    bottom: 0,
    backgroundColor: 'rgba(0,0,0,0.5)',
    justifyContent: 'flex-end',
  },
  citySelectorContainer: {
    backgroundColor: '#fff',
    borderTopLeftRadius: 16,
    borderTopRightRadius: 16,
    padding: 16,
    maxHeight: '70%',
  },
  citySelectorTitle: {
    fontSize: 18,
    fontWeight: 'bold',
    marginBottom: 16,
    color: '#333',
  },
  cityOption: {
    padding: 16,
    borderBottomWidth: 1,
    borderBottomColor: '#eee',
  },
  cityOptionSelected: {
    backgroundColor: '#e3f2fd',
  },
  cityOptionText: {
    fontSize: 16,
    color: '#333',
  },
  cityOptionTextSelected: {
    color: '#3498db',
    fontWeight: '600',
  },
  closeButton: {
    marginTop: 16,
    padding: 16,
    alignItems: 'center',
  },
  closeButtonText: {
    fontSize: 16,
    color: '#666',
  },
});

// ============================================================================
// Export
// ============================================================================

export default HomeScreenFlow;
