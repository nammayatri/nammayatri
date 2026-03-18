/**
 * MetroSubwayBooking Flow Component
 * 
 * This component handles the metro/subway booking flow.
 * Uses usePublicTransportData hook for public transport data.
 * 
 * Fix applied: Removed redundant refreshData() call that was causing
 * duplicate API requests when state changes occurred.
 */

import React, { useEffect, useState, useCallback } from 'react';
import { View, Text, StyleSheet, TouchableOpacity, ScrollView } from 'react-native';
import { usePublicTransportData } from '../../hooks/usePublicTransportData';

// ============================================================================
// Types
// ============================================================================

interface Station {
  id: string;
  code: string;
  name: string;
  lat: number;
  lon: number;
}

interface Route {
  id: string;
  code: string;
  name: string;
  type: string;
}

interface BookingState {
  selectedStation: Station | null;
  selectedRoute: Route | null;
  step: 'select_station' | 'select_route' | 'confirm';
}

// ============================================================================
// Component
// ============================================================================

export const MetroSubwayBookingFlow: React.FC = () => {
  // ============================================================================
  // State
  // ============================================================================

  const [bookingState, setBookingState] = useState<BookingState>({
    selectedStation: null,
    selectedRoute: null,
    step: 'select_station',
  });

  const [city, setCity] = useState<string>('bangalore');

  // ============================================================================
  // Hooks
  // ============================================================================

  // Use the public transport data hook
  // The hook handles deduplication internally - no need for manual refresh calls
  const { 
    data: publicTransportData, 
    isLoading, 
    error,
    refreshData,
    isPremiumMissing,
  } = usePublicTransportData({
    enablePremiumBusDataRefetch: true,
    city,
    profileVersion: 'v1.0.0',
  });

  // ============================================================================
  // Handlers
  // ============================================================================

  const handleStationSelect = useCallback((station: Station) => {
    setBookingState(prev => ({
      ...prev,
      selectedStation: station,
      step: 'select_route',
    }));
  }, []);

  const handleRouteSelect = useCallback((route: Route) => {
    setBookingState(prev => ({
      ...prev,
      selectedRoute: route,
      step: 'confirm',
    }));
  }, []);

  const handleBack = useCallback(() => {
    setBookingState(prev => ({
      ...prev,
      step: prev.step === 'confirm' ? 'select_route' : 'select_station',
      selectedRoute: prev.step === 'confirm' ? null : prev.selectedRoute,
      selectedStation: prev.step === 'select_route' ? null : prev.selectedStation,
    }));
  }, []);

  const handleCityChange = useCallback((newCity: string) => {
    setCity(newCity);
    // City change will trigger a refetch via the hook's internal logic
    // No need to manually call refreshData() here
  }, []);

  // ============================================================================
  // REMOVED: Redundant refreshData() effect
  // ============================================================================
  
  /*
   * BEFORE (caused duplicate API calls):
   * 
   * useEffect(() => {
   *   // This was causing duplicate fetches because:
   *   // 1. The hook already fetches when city changes
   *   // 2. This effect was calling refreshData() on every state change
   *   refreshData();
   * }, [bookingState.step, city, refreshData]);
   */

  /*
   * CORRECT APPROACH:
   * 
   * The usePublicTransportData hook internally handles:
   * - Fetching when city changes (via queryParams)
   * - Version-based caching
   * - Deduplication of concurrent requests
   * 
   * Manual refreshData() should only be called:
   * - On explicit user action (pull-to-refresh)
   * - When the user explicitly requests a refresh
   * - Not on every state change
   */

  // ============================================================================
  // Effects
  // ============================================================================

  // Only log state changes for debugging
  useEffect(() => {
    console.log('[MetroSubwayBooking] State changed:', bookingState.step);
  }, [bookingState.step]);

  // ============================================================================
  // Render Helpers
  // ============================================================================

  const renderStationSelection = () => (
    <View style={styles.container}>
      <Text style={styles.title}>Select Station</Text>
      {isLoading ? (
        <Text style={styles.loadingText}>Loading stations...</Text>
      ) : error ? (
        <View style={styles.errorContainer}>
          <Text style={styles.errorText}>Failed to load stations</Text>
          <TouchableOpacity style={styles.retryButton} onPress={refreshData}>
            <Text style={styles.retryButtonText}>Retry</Text>
          </TouchableOpacity>
        </View>
      ) : (
        <ScrollView style={styles.list}>
          {publicTransportData?.stations.map((station) => (
            <TouchableOpacity
              key={station.id}
              style={styles.listItem}
              onPress={() => handleStationSelect(station)}
            >
              <Text style={styles.listItemText}>{station.name}</Text>
              <Text style={styles.listItemSubtext}>{station.code}</Text>
            </TouchableOpacity>
          ))}
        </ScrollView>
      )}
    </View>
  );

  const renderRouteSelection = () => (
    <View style={styles.container}>
      <Text style={styles.title}>Select Route</Text>
      <Text style={styles.subtitle}>
        From: {bookingState.selectedStation?.name}
      </Text>
      <TouchableOpacity style={styles.backButton} onPress={handleBack}>
        <Text style={styles.backButtonText}>← Back to Stations</Text>
      </TouchableOpacity>
      
      {isLoading ? (
        <Text style={styles.loadingText}>Loading routes...</Text>
      ) : (
        <ScrollView style={styles.list}>
          {publicTransportData?.routes.map((route) => (
            <TouchableOpacity
              key={route.id}
              style={styles.listItem}
              onPress={() => handleRouteSelect(route)}
            >
              <Text style={styles.listItemText}>{route.name}</Text>
              <Text style={styles.listItemSubtext}>{route.type}</Text>
            </TouchableOpacity>
          ))}
        </ScrollView>
      )}
    </View>
  );

  const renderConfirmation = () => (
    <View style={styles.container}>
      <Text style={styles.title}>Confirm Booking</Text>
      <View style={styles.confirmationCard}>
        <Text style={styles.confirmationLabel}>Station:</Text>
        <Text style={styles.confirmationValue}>
          {bookingState.selectedStation?.name}
        </Text>
        
        <Text style={styles.confirmationLabel}>Route:</Text>
        <Text style={styles.confirmationValue}>
          {bookingState.selectedRoute?.name}
        </Text>
        
        <Text style={styles.confirmationLabel}>Type:</Text>
        <Text style={styles.confirmationValue}>
          {bookingState.selectedRoute?.type}
        </Text>
      </View>
      
      <TouchableOpacity style={styles.backButton} onPress={handleBack}>
        <Text style={styles.backButtonText}>← Back to Routes</Text>
      </TouchableOpacity>
      
      <TouchableOpacity style={styles.confirmButton}>
        <Text style={styles.confirmButtonText}>Confirm Booking</Text>
      </TouchableOpacity>
    </View>
  );

  // ============================================================================
  // Render
  // ============================================================================

  return (
    <View style={styles.screen}>
      {bookingState.step === 'select_station' && renderStationSelection()}
      {bookingState.step === 'select_route' && renderRouteSelection()}
      {bookingState.step === 'confirm' && renderConfirmation()}
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
  container: {
    flex: 1,
    padding: 16,
  },
  title: {
    fontSize: 24,
    fontWeight: 'bold',
    marginBottom: 16,
    color: '#333',
  },
  subtitle: {
    fontSize: 16,
    color: '#666',
    marginBottom: 12,
  },
  loadingText: {
    fontSize: 16,
    color: '#666',
    textAlign: 'center',
    marginTop: 32,
  },
  errorContainer: {
    alignItems: 'center',
    marginTop: 32,
  },
  errorText: {
    fontSize: 16,
    color: '#e74c3c',
    marginBottom: 16,
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
  list: {
    flex: 1,
  },
  listItem: {
    backgroundColor: '#fff',
    padding: 16,
    marginBottom: 8,
    borderRadius: 8,
    shadowColor: '#000',
    shadowOffset: { width: 0, height: 1 },
    shadowOpacity: 0.1,
    shadowRadius: 2,
    elevation: 2,
  },
  listItemText: {
    fontSize: 16,
    fontWeight: '600',
    color: '#333',
  },
  listItemSubtext: {
    fontSize: 14,
    color: '#666',
    marginTop: 4,
  },
  backButton: {
    marginBottom: 16,
  },
  backButtonText: {
    fontSize: 16,
    color: '#3498db',
  },
  confirmationCard: {
    backgroundColor: '#fff',
    padding: 20,
    borderRadius: 12,
    marginBottom: 20,
    shadowColor: '#000',
    shadowOffset: { width: 0, height: 2 },
    shadowOpacity: 0.1,
    shadowRadius: 4,
    elevation: 3,
  },
  confirmationLabel: {
    fontSize: 14,
    color: '#666',
    marginTop: 12,
  },
  confirmationValue: {
    fontSize: 18,
    fontWeight: '600',
    color: '#333',
    marginTop: 4,
  },
  confirmButton: {
    backgroundColor: '#27ae60',
    padding: 16,
    borderRadius: 8,
    alignItems: 'center',
    marginTop: 'auto',
  },
  confirmButtonText: {
    color: '#fff',
    fontSize: 18,
    fontWeight: '600',
  },
});

// ============================================================================
// Export
// ============================================================================

export default MetroSubwayBookingFlow;
