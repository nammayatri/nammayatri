import React, { useState, useEffect } from 'react';
import { Container, Row, Col, Card, Button, ButtonGroup } from 'react-bootstrap';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faSync, faDownload, faUpload, faCode, faSun, faMoon, faTicketAlt, faSave } from '@fortawesome/free-solid-svg-icons';
import { useTheme } from '../../contexts/ThemeContext';
import TicketSystemTree from './TicketSystemTree';
import EntityDetailsModal from './EntityDetailsModal';
import { useTicket } from '../../contexts/TicketContext';
import ServiceModal from '../Services/ServiceModal';
import ServiceCategoryModal from '../Categories/ServiceCategoryModal';
import ServicePeopleCategoryModal from '../Categories/ServicePeopleCategoryModal';
import BusinessHoursModal from '../BusinessHours/BusinessHoursModal';
import TicketPlaceModal from '../TicketPlace/TicketPlaceModal';
import JsonPreview from '../Common/JsonPreview';
import './TreeViewDashboard.css';

const TreeViewDashboard = () => {
  const {
    getActiveTicketPlace,
    generateApiJson,
    downloadJsonFile,
    loadJsonFromFile,
    fetchTicketPlaceDetails,
    saveCurrentTicketPlace,
    isLoading,
    setIsLoading
  } = useTicket();

  const { theme, toggleTheme } = useTheme();

  const [selectedNode, setSelectedNode] = useState(null);
  const [showDetailsModal, setShowDetailsModal] = useState(false);
  const [showTicketPlaceModal, setShowTicketPlaceModal] = useState(false);
  const [showServiceModal, setShowServiceModal] = useState(false);
  const [showCategoryModal, setShowCategoryModal] = useState(false);
  const [showPeopleCategoryModal, setShowPeopleCategoryModal] = useState(false);
  const [showBusinessHourModal, setShowBusinessHourModal] = useState(false);

  // Use a ref to track if we've already fetched the data
  const dataFetchedRef = React.useRef(false);

  // Get the active place directly from context
  const activePlace = getActiveTicketPlace();

  // Fetch the latest ticket place details when the component mounts
  // Fetch data only once when the component mounts
  useEffect(() => {
    // Only fetch data if we haven't already
    if (!dataFetchedRef.current && activePlace && activePlace.id) {
      // Set loading state
      setIsLoading(true);

      // Mark that we're fetching data
      dataFetchedRef.current = true;

      console.log('TreeViewDashboard: Fetching latest data for place:', activePlace.id);

      // Set a timeout to clear the loading state if the request takes too long
      const loadingTimeout = setTimeout(() => {
        setIsLoading(false);
      }, 10000); // 10 seconds timeout

      fetchTicketPlaceDetails(activePlace.id)
        .then(updatedPlace => {
          if (updatedPlace) {
            console.log('TreeViewDashboard: Successfully fetched latest data');
          } else {
            console.error('TreeViewDashboard: Failed to fetch latest data');
            // Reset the flag if fetch fails
            dataFetchedRef.current = false;
          }
        })
        .catch(err => {
          console.error('TreeViewDashboard: Error fetching latest data:', err);
          // Reset the flag if fetch fails
          dataFetchedRef.current = false;
        })
        .finally(() => {
          // Clear loading state and timeout
          clearTimeout(loadingTimeout);
          setIsLoading(false);
        });
    }
  }, []);
  const [currentTicketPlace, setCurrentTicketPlace] = useState(null);
  const [currentService, setCurrentService] = useState(null);
  const [currentCategory, setCurrentCategory] = useState(null);
  const [currentPeopleCategory, setCurrentPeopleCategory] = useState(null);
  const [currentBusinessHour, setCurrentBusinessHour] = useState(null);
  const [parentEntity, setParentEntity] = useState(null);
  const [activeTab, setActiveTab] = useState('tree'); // 'tree' or 'json'

  // Handle node selection from tree
  const handleNodeSelect = (nodeType, node) => {
    setSelectedNode({ type: nodeType, data: node });
    setShowDetailsModal(true);
  };

  // Handle closing the details modal
  const handleCloseDetailsModal = () => {
    setShowDetailsModal(false);
  };

  // Handle edit action
  const handleEdit = (entityType, entity) => {
    switch (entityType) {
      case 'place':
        setCurrentTicketPlace(entity);
        setShowTicketPlaceModal(true);
        break;
      case 'service':
        setCurrentService(entity);
        setShowServiceModal(true);
        break;
      case 'category':
        setCurrentCategory(entity);
        setShowCategoryModal(true);
        break;
      case 'peopleCategory':
        setCurrentPeopleCategory(entity);
        setShowPeopleCategoryModal(true);
        break;
      case 'businessHour':
        setCurrentBusinessHour(entity);
        setShowBusinessHourModal(true);
        break;
      default:
        console.log(`Edit not implemented for ${entityType}`);
    }
  };

  // Handle delete action
  const handleDelete = (entityType, entity) => {
    // Implement delete confirmation and action
    if (window.confirm(`Are you sure you want to delete this ${entityType}?`)) {
      console.log(`Delete ${entityType}:`, entity);
      // Call appropriate delete function based on entity type
    }
  };

  // Handle add action
  const handleAdd = (entityType, parentEntity = null) => {
    setParentEntity(parentEntity);

    switch (entityType) {
      case 'service':
        setCurrentService(null);
        setShowServiceModal(true);
        break;
      case 'category':
        setCurrentCategory(null);
        setShowCategoryModal(true);
        break;
      case 'peopleCategory':
        setCurrentPeopleCategory(null);
        setShowPeopleCategoryModal(true);
        break;
      case 'businessHour':
        setCurrentBusinessHour(null);
        setShowBusinessHourModal(true);
        break;
      default:
        console.log(`Add not implemented for ${entityType}`);
    }
  };

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

  // Handle JSON generation and download
  const handleGenerateJson = () => {
    generateApiJson();
    downloadJsonFile();
  };

  // Handle tab change
  const handleTabChange = (tab) => {
    setActiveTab(tab);
    if (tab === 'json') {
      generateApiJson(); // Generate fresh JSON when switching to JSON tab
    }
  };



  return (
    <Container fluid className="tree-view-dashboard">
      <Row className="mb-3">
        <Col>
          <Card>
            <Card.Body className="d-flex justify-content-between align-items-center">
              <h4 className="mb-0">Ticket Management System</h4>
              <div className="d-flex align-items-center">
                <Button
                  variant={theme === 'light' ? 'outline-dark' : 'outline-light'}
                  className="me-3"
                  onClick={toggleTheme}
                  aria-label="Toggle theme"
                >
                  <FontAwesomeIcon icon={theme === 'light' ? faMoon : faSun} />
                </Button>
                <ButtonGroup className="me-3">
                  <Button
                    variant={activeTab === 'tree' ? 'primary' : 'outline-primary'}
                    onClick={() => handleTabChange('tree')}
                  >
                    <FontAwesomeIcon icon={faTicketAlt} className="me-1" /> Tree View
                  </Button>
                  <Button
                    variant={activeTab === 'json' ? 'primary' : 'outline-primary'}
                    onClick={() => handleTabChange('json')}
                  >
                    <FontAwesomeIcon icon={faCode} className="me-1" /> JSON View
                  </Button>
                </ButtonGroup>
                <ButtonGroup>
                  <Button
                    variant="outline-primary"
                    onClick={handleGenerateJson}
                  >
                    <FontAwesomeIcon icon={faDownload} className="me-1" /> Download JSON
                  </Button>
                  <label className="btn btn-outline-secondary mb-0">
                    <FontAwesomeIcon icon={faUpload} className="me-1" /> Upload JSON
                    <input
                      type="file"
                      accept=".json"
                      style={{ display: 'none' }}
                      onChange={handleFileUpload}
                    />
                  </label>
                </ButtonGroup>
              </div>
            </Card.Body>
          </Card>
        </Col>
      </Row>

      {activeTab === 'tree' ? (
        <Row>
          {/* Full Screen Tree View */}
          <Col md={12} className="full-screen-tree-col">
            <Card className="full-screen-tree-card">
              <Card.Body>
                <div className="d-flex justify-content-end mb-3">
                  <Button
                    variant="outline-primary"
                    size="sm"
                    className="me-2"
                    onClick={async () => {
                      if (isLoading) return;

                      const result = await saveCurrentTicketPlace();
                      if (result) {
                        alert('Changes saved successfully!');
                      } else {
                        alert('Failed to save changes. Please try again.');
                      }
                    }}
                    disabled={isLoading}
                  >
                    <FontAwesomeIcon icon={faSave} className="me-1" />
                    Save Changes
                  </Button>

                  <Button
                    variant="outline-secondary"
                    size="sm"
                    onClick={() => {
                      // Reset the data fetched flag to allow a manual refresh
                      dataFetchedRef.current = false;

                      // Use the activePlace value from above
                      if (activePlace && activePlace.id) {
                        // Set loading state
                        setIsLoading(true);

                        // Mark that we're fetching data
                        dataFetchedRef.current = true;

                        console.log('Manual refresh: Fetching latest data for place:', activePlace.id);

                        // Set a timeout to clear the loading state if the request takes too long
                        const loadingTimeout = setTimeout(() => {
                          setIsLoading(false);
                        }, 10000); // 10 seconds timeout

                        fetchTicketPlaceDetails(activePlace.id)
                          .then(updatedPlace => {
                            if (updatedPlace) {
                              console.log('Manual refresh: Successfully fetched latest data');
                            } else {
                              console.error('Manual refresh: Failed to fetch latest data');
                              // Reset the flag if fetch fails
                              dataFetchedRef.current = false;
                            }
                          })
                          .catch(err => {
                            console.error('Manual refresh: Error fetching latest data:', err);
                            // Reset the flag if fetch fails
                            dataFetchedRef.current = false;
                          })
                          .finally(() => {
                            // Clear loading state and timeout
                            clearTimeout(loadingTimeout);
                            setIsLoading(false);
                          });
                      }
                    }}
                    disabled={isLoading}
                  >
                    <FontAwesomeIcon icon={faSync} className={isLoading ? "fa-spin me-1" : "me-1"} />
                    {isLoading ? 'Loading...' : 'Refresh Data'}
                  </Button>
                </div>
                <TicketSystemTree onNodeSelect={handleNodeSelect} />
              </Card.Body>
            </Card>
          </Col>
        </Row>
      ) : (
        <Row>
          <Col>
            <Card>
              <Card.Header>
                <h5 className="mb-0">JSON Preview</h5>
              </Card.Header>
              <Card.Body>
                <JsonPreview />
              </Card.Body>
            </Card>
          </Col>
        </Row>
      )}

      {/* Modals */}
      <EntityDetailsModal
        show={showDetailsModal}
        onHide={handleCloseDetailsModal}
        selectedNode={selectedNode}
        onEdit={handleEdit}
        onDelete={handleDelete}
        onAdd={handleAdd}
      />

      <TicketPlaceModal
        show={showTicketPlaceModal}
        onHide={() => setShowTicketPlaceModal(false)}
        place={currentTicketPlace}
      />

      <ServiceModal
        show={showServiceModal}
        onHide={() => setShowServiceModal(false)}
        service={currentService}
      />

      <ServiceCategoryModal
        show={showCategoryModal}
        onHide={() => setShowCategoryModal(false)}
        category={currentCategory}
      />

      <ServicePeopleCategoryModal
        show={showPeopleCategoryModal}
        onHide={() => setShowPeopleCategoryModal(false)}
        peopleCategory={currentPeopleCategory}
      />

      <BusinessHoursModal
        show={showBusinessHourModal}
        onHide={() => setShowBusinessHourModal(false)}
        businessHour={currentBusinessHour}
      />
    </Container>
  );
};

export default TreeViewDashboard;
