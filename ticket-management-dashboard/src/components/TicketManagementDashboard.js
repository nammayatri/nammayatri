import React, { useState, useEffect } from 'react';
import { Container, Row, Col, Button, ButtonGroup, Nav, Alert, Spinner } from 'react-bootstrap';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faSun, faMoon, faDownload, faUpload, faList, faProjectDiagram, faTrash, faCode, faSync, faSave } from '@fortawesome/free-solid-svg-icons';
import { useTheme } from '../contexts/ThemeContext';
import { useTicket } from '../contexts/TicketContext';
import TicketPlaceList from './TicketPlaces/TicketPlaceList';
import TreeViewDashboard from './TreeView/TreeViewDashboard';
import TicketPlaceForm from './TicketPlace/TicketPlaceForm';
import BusinessHoursList from './BusinessHours/BusinessHoursList';
import ServicesList from './Services/ServicesList';
import ServiceCategoryList from './Categories/ServiceCategoryList';
import ServicePeopleCategoryList from './Categories/ServicePeopleCategoryList';
import SpecialOccasionsList from './SpecialOccasions/SpecialOccasionsList';
import JsonPreview from './Common/JsonPreview';
import './TicketManagementDashboard.css';

// Add the overlay component with transition message
const LoadingOverlay = ({ isTransitioning = false }) => (
  <div 
    style={{
      position: 'fixed',
      top: 0,
      left: 0,
      right: 0,
      bottom: 0,
      backgroundColor: isTransitioning ? 'rgba(0, 123, 255, 0.3)' : 'rgba(0, 123, 255, 0.2)', // slightly darker blue for transition
      display: 'flex',
      justifyContent: 'center',
      alignItems: 'center',
      zIndex: 9999,
      backdropFilter: isTransitioning ? 'blur(4px)' : 'blur(2px)' // more blur during transition
    }}
  >
    <div className="text-center">
      <Spinner animation="border" role="status" variant="primary" style={{ width: '4rem', height: '4rem' }} />
      <h4 className="mt-3 text-primary">
        {isTransitioning ? 'Preparing Dashboard View...' : 'Loading data...'}
      </h4>
      <p className="text-muted">
        {isTransitioning ? 'Please wait while we set up your dashboard.' : 'Please wait while we fetch the ticket data.'}
      </p>
    </div>
  </div>
);

const TicketManagementDashboard = () => {
  const { theme, toggleTheme } = useTheme();
  const {
    ticketPlaceIds,
    ticketPlaceMap,
    activeTicketPlaceId,
    setActiveTicketPlace,
    downloadJsonFile,
    loadJsonFromFile,
    clearAllTicketData,
    fetchTicketPlaces,
    fetchTicketPlaceDetails,
    isLoading,
    setIsLoading,
    error,
    saveCurrentTicketPlace,
    generateApiJson
  } = useTicket();

  // State to track if data is ready
  const [isSaving, setIsSaving] = useState(false);
  const [showDashboard, setShowDashboard] = useState(false);
  const [fileInput, setFileInput] = useState(null);
  const [activeView, setActiveView] = useState('list');

  // State for JSON view toggle
  const [showJsonView, setShowJsonView] = useState(() => {
    // Load JSON view state from localStorage or default to false
    return localStorage.getItem('showJsonView') === 'true';
  });
  const [activeSection, setActiveSection] = useState(() => {
    // Load active section from localStorage or default to 'ticketPlace'
    return localStorage.getItem('activeSection') || 'ticketPlace';
  });
  const [navbarCollapsed, setNavbarCollapsed] = useState(false);
  const [isTransitioning, setIsTransitioning] = useState(false);

  // We don't need to check for an active ticket place on initial load
  // as we always want to start with the ticket places list

  // Fetch ticket places when component mounts
  useEffect(() => {
    console.log('TicketManagementDashboard mounted, fetching ticket places...');
    fetchTicketPlaces();
  }, [fetchTicketPlaces]);

  // Handle file upload
  const handleFileUpload = (event) => {
    const file = event.target.files[0];
    if (!file) return;

    const reader = new FileReader();
    reader.onload = (e) => {
      try {
        const jsonData = JSON.parse(e.target.result);
        loadJsonFromFile(jsonData);
      } catch (error) {
        console.error('Error parsing JSON file:', error);
        alert('Invalid JSON file');
      }
    };
    reader.readAsText(file);
  };

  // Handle file input click
  const handleFileInputClick = () => {
    if (fileInput) {
      fileInput.click();
    }
  };

  // Handle clearing all ticket data
  const handleClearData = () => {
    if (window.confirm('Are you sure you want to clear all ticket data? This will reset to the default 3 ticket places.')) {
      clearAllTicketData();
      window.location.reload(); // Reload the page to reflect changes
    }
  };

  // Toggle JSON view
  const toggleJsonView = () => {
    setShowJsonView(prev => !prev);
  };

  // Handle place selection
  const handlePlaceSelect = async (place) => {
    try {
      setIsLoading(true);
      setIsTransitioning(true);
      console.log('Starting place selection for:', place.id);

      // Function to check if data is complete
      const isDataComplete = (data) => {
        if (!data) {
          console.error('No data received from API');
          return false;
        }
        
        const requiredSections = [
          'services',
          'businessHours',
          'serviceCategories',
          'servicePeopleCategories',
          'specialOccasions'
        ];
        
        let missingSections = [];
        let emptySections = [];
        
        for (const section of requiredSections) {
          if (!data[section]) {
            missingSections.push(section);
            continue;
          }
          if (!Array.isArray(data[section])) {
            console.error(`${section} is not an array:`, data[section]);
            return false;
          }
          if (data[section].length === 0) {
            emptySections.push(section);
          }
        }

        if (missingSections.length > 0) {
          console.error('Missing sections:', missingSections);
          return false;
        }

        if (emptySections.length > 0) {
          console.warn('Empty sections:', emptySections);
        }

        return true;
      };

      // Step 1: Set active place first and wait for it to complete
      console.log('Setting active place...');
      await new Promise(resolve => {
        setActiveTicketPlace(place);
        setTimeout(resolve, 100); // Small delay to ensure state is updated
      });

      // Step 2: Fetch data
      console.log('Fetching ticket place details...');
      const data = await fetchTicketPlaceDetails(place.id);
      console.log('Received data:', data);
      
      // Step 3: Validate data
      if (!isDataComplete(data)) {
        throw new Error('Incomplete data received');
      }
      
      // Step 4: Wait a moment before transitioning
      await new Promise(resolve => setTimeout(resolve, 500));
      
      console.log('Data is complete, transitioning to dashboard view...');
      
      // Step 5: Set dashboard view
      setShowDashboard(true);
      
      // Step 6: Final verification
      console.log('Transition complete. Active place:', place.id);
      
    } catch (error) {
      console.error('Error in handlePlaceSelect:', error);
      alert('Failed to load complete ticket place details. Please try again.');
      // Reset states on error
      setActiveTicketPlace(null);
      setShowDashboard(false);
    } finally {
      // Step 7: Clear loading states after a small delay
      setTimeout(() => {
        setIsLoading(false);
        setIsTransitioning(false);
        console.log('Loading states cleared');
      }, 300);
    }
  };

  // Handle back to places list
  const handleBackToPlaces = () => {
    setShowDashboard(false);
  };

  // Toggle navbar collapse
  const toggleNavbar = () => {
    setNavbarCollapsed(!navbarCollapsed);
  };

  // Handle navigation link click
  const handleNavLinkClick = (section) => {
    setActiveSection(section);
    // Scroll to the section
    const element = document.getElementById(section);
    if (element) {
      element.scrollIntoView({ behavior: 'smooth' });
    }
  };

  // Add scroll event listener to update active section based on scroll position
  useEffect(() => {
    if (activeView === 'list' && showDashboard) {
      const handleScroll = () => {
        const sections = [
          'ticketPlace',
          'services',
          'businessHours',
          'serviceCategories',
          'peopleCategories',
          'specialOccasions',
          'jsonPreview'
        ];

        // Find the section that is currently in view
        for (const section of sections) {
          const element = document.getElementById(section);
          if (element) {
            const rect = element.getBoundingClientRect();
            if (rect.top <= 100 && rect.bottom >= 100) {
              setActiveSection(section);
              break;
            }
          }
        }
      };

      window.addEventListener('scroll', handleScroll);
      return () => {
        window.removeEventListener('scroll', handleScroll);
      };
    }
  }, [activeView, showDashboard]);

  // Save active view to localStorage when it changes
  useEffect(() => {
    localStorage.setItem('activeView', activeView);
    console.log(`View changed to ${activeView}`);
  }, [activeView]);

  // Save active section to localStorage when it changes
  useEffect(() => {
    localStorage.setItem('activeSection', activeSection);
  }, [activeSection]);

  // Save JSON view state to localStorage when it changes
  useEffect(() => {
    localStorage.setItem('showJsonView', showJsonView.toString());
  }, [showJsonView]);

  // Save showDashboard state to localStorage when it changes
  useEffect(() => {
    localStorage.setItem('showDashboard', showDashboard.toString());
  }, [showDashboard]);

  return (
    <Container fluid className="ticket-management-dashboard">
      {(isLoading || isTransitioning) && <LoadingOverlay isTransitioning={isTransitioning} />}
      {/* Header */}
      <Row className="header mb-3">
        <Col>
          <div className="d-flex justify-content-between align-items-center">
            <h1 className="dashboard-title">Ticket Management Dashboard</h1>
            <div>
              <ButtonGroup className="me-2">
                <Button
                  variant={theme === 'light' ? 'primary' : 'outline-primary'}
                  onClick={toggleTheme}
                  title={theme === 'light' ? 'Switch to Dark Mode' : 'Switch to Light Mode'}
                >
                  <FontAwesomeIcon icon={theme === 'light' ? faMoon : faSun} />
                </Button>
                <Button
                  variant="outline-primary"
                  onClick={() => downloadJsonFile()}
                  title="Download JSON"
                >
                  <FontAwesomeIcon icon={faDownload} />
                </Button>
                <Button
                  variant="outline-primary"
                  onClick={handleFileInputClick}
                  title="Upload JSON"
                >
                  <FontAwesomeIcon icon={faUpload} />
                  <input
                    type="file"
                    accept=".json"
                    style={{ display: 'none' }}
                    onChange={handleFileUpload}
                    ref={input => setFileInput(input)}
                  />
                </Button>
              </ButtonGroup>
              <Button
                variant="danger"
                onClick={handleClearData}
                title="Reset to default ticket places"
                className="ms-2"
              >
                <FontAwesomeIcon icon={faTrash} /> Reset Data
              </Button>
            </div>
          </div>
        </Col>
      </Row>

      {/* Main Content */}
      {showDashboard ? (
        <>
          <Row className="mb-3">
            <Col>
              <div className="d-flex justify-content-between align-items-center">
                <Button
                  variant="outline-secondary"
                  onClick={handleBackToPlaces}
                  className="back-button"
                >
                  ← Back to Ticket Places
                </Button>
                <div className="flex-grow-1 d-flex justify-content-center">
                  {/* Remove ButtonGroup and keep only the Save Changes and JSON View buttons */}
                </div>
                <div>
                  <Button
                    variant="success"
                    onClick={async () => {
                      setIsSaving(true);
                      try {
                        // Generate the API JSON before saving
                        generateApiJson();

                        // Save all changes to the API
                        const result = await saveCurrentTicketPlace();
                        if (result) {
                          alert('Changes saved successfully!');
                        } else {
                          alert('Failed to save changes. Please try again.');
                        }
                      } catch (error) {
                        console.error('Error saving changes:', error);
                        alert('An error occurred while saving changes.');
                      } finally {
                        setIsSaving(false);
                      }
                    }}
                    className="me-2"
                    disabled={isSaving}
                  >
                    <FontAwesomeIcon icon={faSave} className="me-1" /> {isSaving ? 'Saving...' : 'Save Changes'}
                  </Button>
                  <Button
                    variant={showJsonView ? 'primary' : 'outline-primary'}
                    onClick={toggleJsonView}
                    title={showJsonView ? 'Hide JSON View' : 'Show JSON View'}
                  >
                    <FontAwesomeIcon icon={faCode} className="me-1" /> {showJsonView ? 'Hide JSON' : 'Show JSON'}
                  </Button>
                </div>
              </div>
            </Col>
          </Row>

          {/* Comment out or remove the old loading view */}
          {/* {isLoading ? (
            <Row className="justify-content-center mt-5">
              <Col xs="auto">
                <div className="text-center">
                  <Spinner animation="border" role="status" variant="primary" style={{ width: '3rem', height: '3rem' }} />
                  <h4 className="mt-3">Loading data...</h4>
                  <p className="text-muted">Please wait while we fetch the ticket data.</p>
                </div>
              </Col>
            </Row>
          ) : ( */}
            <div className="list-view-container">
              {/* Horizontal Navigation Bar */}
              <div className={`horizontal-navbar ${navbarCollapsed ? 'navbar-collapsed' : ''}`}>
                <div className="navbar-header d-flex justify-content-between align-items-center">
                  <h5 className="mb-0">Navigation</h5>
                  <Button
                    variant="light"
                    size="sm"
                    className="navbar-toggle-btn"
                    onClick={toggleNavbar}
                  >
                    <FontAwesomeIcon icon={navbarCollapsed ? faList : faList} />
                    <span className="ms-2">{navbarCollapsed ? 'Show Menu' : 'Hide Menu'}</span>
                  </Button>
                </div>
                <Nav className={`horizontal-nav ${navbarCollapsed ? 'd-none' : ''}`}>
                  <Nav.Link
                    className={activeSection === 'ticketPlace' ? 'active' : ''}
                    onClick={() => handleNavLinkClick('ticketPlace')}
                  >Ticket Place</Nav.Link>
                  <Nav.Link
                    className={activeSection === 'services' ? 'active' : ''}
                    onClick={() => handleNavLinkClick('services')}
                  >Services</Nav.Link>
                  <Nav.Link
                    className={activeSection === 'businessHours' ? 'active' : ''}
                    onClick={() => handleNavLinkClick('businessHours')}
                  >Business Hours</Nav.Link>
                  <Nav.Link
                    className={activeSection === 'serviceCategories' ? 'active' : ''}
                    onClick={() => handleNavLinkClick('serviceCategories')}
                  >Service Categories</Nav.Link>
                  <Nav.Link
                    className={activeSection === 'peopleCategories' ? 'active' : ''}
                    onClick={() => handleNavLinkClick('peopleCategories')}
                  >People Categories</Nav.Link>
                  <Nav.Link
                    className={activeSection === 'specialOccasions' ? 'active' : ''}
                    onClick={() => handleNavLinkClick('specialOccasions')}
                  >Special Occasions</Nav.Link>
                  <Nav.Link
                    className={activeSection === 'jsonPreview' ? 'active' : ''}
                    onClick={() => handleNavLinkClick('jsonPreview')}
                  >JSON View</Nav.Link>
                </Nav>
              </div>

              {/* Main Content */}
              <div className="main-content-wrapper">
                <Row>
                  <Col md={showJsonView ? 8 : 12} className="main-content-col">
                    <div id="ticketPlace">
                      <TicketPlaceForm />
                    </div>

                    <div id="services">
                      <ServicesList />
                    </div>

                    <div id="businessHours">
                      <BusinessHoursList />
                    </div>

                    <div id="serviceCategories">
                      <ServiceCategoryList />
                    </div>

                    <div id="peopleCategories">
                      <ServicePeopleCategoryList />
                    </div>

                    <div id="specialOccasions">
                      <SpecialOccasionsList />
                    </div>

                    {!showJsonView && (
                      <div id="jsonPreview">
                        <JsonPreview />
                      </div>
                    )}

                  </Col>
                  {showJsonView && (
                    <Col md={4} className="json-preview-col">
                      <div className="sticky-top pt-3">
                        <JsonPreview />
                      </div>
                    </Col>
                  )}
                </Row>
              </div>
            </div>
          {/* )} */}
        </>
      ) : (
        <TicketPlaceList onSelectPlace={handlePlaceSelect} />
      )}
    </Container>
  );
};

export default TicketManagementDashboard;
