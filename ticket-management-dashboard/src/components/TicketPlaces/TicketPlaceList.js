import React, { useEffect, useState } from 'react';
import { Spinner, Alert, Table, Button, Form, Container, Row, Col } from 'react-bootstrap';
import { useTicket } from '../../contexts/TicketContext';
import TicketPlaceModal from '../TicketPlace/TicketPlaceModal';

const inputStyles = {
  // Remove up/down arrows
  WebkitAppearance: 'none',
  MozAppearance: 'textfield',
  appearance: 'textfield',
  margin: 0,
  width: '80px'
};

const TicketPlaceList = ({ onSelectPlace }) => {
  const { 
    ticketPlaceIds,
    ticketPlaceMap,
    isLoading, 
    fetchTicketPlaces, 
    error: contextError,
    statusFilter,
    setStatusFilter,
    updateTicketPlace,
    updateApiConfig
  } = useTicket();
  const [error, setError] = useState(null);
  const [configError, setConfigError] = useState(null);
  const [editingPriority, setEditingPriority] = useState(null);
  const [priorityValue, setPriorityValue] = useState('');
  const [showModal, setShowModal] = useState(false);
  const [currentPlace, setCurrentPlace] = useState(null);
  const [baseUrl, setBaseUrl] = useState(() => localStorage.getItem('apiBaseUrl') || 'http://localhost:8017/bap/YATRI/Kochi');
  const [token, setToken] = useState(() => localStorage.getItem('apiToken') || '0466f4fb-6af8-49f5-8d0c-9196101afdc4');

  useEffect(() => {
    const loadData = async () => {
      try {
        await fetchTicketPlaces(statusFilter);
      } catch (error) {
        console.error('Error fetching ticket places:', error);
        setError(error.message);
      }
    };

    loadData();
  }, [statusFilter, fetchTicketPlaces]);

  const handleStatusChange = (event) => {
    setStatusFilter(event.target.value);
  };

  const handlePriorityEdit = (placeId, currentPriority) => {
    setEditingPriority(placeId);
    setPriorityValue(currentPriority.toString());
  };

  const handlePriorityChange = (e) => {
    const value = e.target.value;
    // Only allow non-negative whole numbers
    if (value === '' || /^\d+$/.test(value)) {
      setPriorityValue(value);
    }
  };

  const handlePrioritySave = async (place) => {
    try {
      if (priorityValue === '') {
        throw new Error('Priority cannot be empty');
      }
      const newPriority = parseInt(priorityValue, 10);
      if (isNaN(newPriority)) {
        throw new Error('Priority must be a valid number');
      }
      await updateTicketPlace({ ...place, priority: newPriority });
      setEditingPriority(null);
      setPriorityValue('');
    } catch (error) {
      console.error('Error updating priority:', error);
      setError(error.message);
    }
  };

  const handlePriorityKeyPress = (e, place) => {
    if (e.key === 'Enter') {
      handlePrioritySave(place);
    } else if (e.key === 'Escape') {
      setEditingPriority(null);
      setPriorityValue('');
    }
  };

  const handleAddClick = () => {
    setCurrentPlace(null);
    setShowModal(true);
  };

  const handleCloseModal = () => {
    setShowModal(false);
  };

  const handleSave = async (savedPlace) => {
    try {
      // Refresh the list after save
      await fetchTicketPlaces(statusFilter);
    } catch (error) {
      console.error('Error refreshing ticket places:', error);
    }
  };

  const validateBaseUrl = (url) => {
    try {
      new URL(url);
      return true;
    } catch (err) {
      return false;
    }
  };

  const validateToken = (token) => {
    // Token should be a UUID format
    const uuidRegex = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;
    return uuidRegex.test(token);
  };

  const handleConfigUpdate = async () => {
    setConfigError(null);

    // Validate base URL
    if (!validateBaseUrl(baseUrl)) {
      setConfigError('Please enter a valid URL (e.g., http://localhost:8017/bap/YATRI/Kochi)');
      return;
    }

    // Validate token
    if (!validateToken(token)) {
      setConfigError('Please enter a valid token (UUID format)');
      return;
    }

    try {
      localStorage.setItem('apiBaseUrl', baseUrl);
      localStorage.setItem('apiToken', token);
      await updateApiConfig({ baseUrl, token });
    } catch (err) {
      setConfigError('Failed to connect to the server. Please check your base URL and token.');
    }
  };

  const handleBaseUrlChange = (e) => {
    setBaseUrl(e.target.value);
    setConfigError(null);
  };

  const handleTokenChange = (e) => {
    setToken(e.target.value);
    setConfigError(null);
  };

  // Filter ticket places based on status
  const filteredPlaces = ticketPlaceIds
    .map(id => ticketPlaceMap[id])
    .filter(place => place && place.status === statusFilter);

  return (
    <Container fluid>
      {/* API Configuration Section - Always visible */}
      <Row className="mb-4">
        <Col>
          <h2>API Configuration</h2>
        </Col>
      </Row>

      <Row className="mb-4">
        <Col md={6}>
          <Form.Group className="mb-3">
            <Form.Label>Base URL</Form.Label>
            <Form.Control
              type="text"
              value={baseUrl}
              onChange={handleBaseUrlChange}
              placeholder="Enter API base URL"
              isInvalid={configError && !validateBaseUrl(baseUrl)}
            />
            <Form.Text className="text-muted">
              Example: http://localhost:8017/bap/YATRI/Kochi
            </Form.Text>
          </Form.Group>
        </Col>
        <Col md={6}>
          <Form.Group className="mb-3">
            <Form.Label>API Token</Form.Label>
            <Form.Control
              type="text"
              value={token}
              onChange={handleTokenChange}
              placeholder="Enter API token"
              isInvalid={configError && !validateToken(token)}
            />
            <Form.Text className="text-muted">
              Example: 0466f4fb-6af8-49f5-8d0c-9196101afdc4
            </Form.Text>
          </Form.Group>
        </Col>
      </Row>

      {configError && (
        <Row className="mb-4">
          <Col>
            <Alert variant="danger">
              {configError}
            </Alert>
          </Col>
        </Row>
      )}

      <Row className="mb-4">
        <Col>
          <Button variant="primary" onClick={handleConfigUpdate}>
            Update Configuration & Refresh
          </Button>
        </Col>
      </Row>

      {/* Loading State */}
      {isLoading && (
        <div className="text-center mt-5">
          <Spinner animation="border" role="status">
            <span className="visually-hidden">Loading...</span>
          </Spinner>
        </div>
      )}

      {/* Error State */}
      {(error || contextError) && (
        <div className="m-3">
          <Alert variant="danger">
            {error || contextError}
          </Alert>
          <Button variant="primary" onClick={() => fetchTicketPlaces(statusFilter)} className="mt-2">
            Try Again
          </Button>
        </div>
      )}

      {/* Only show ticket places section if no error and not loading */}
      {!isLoading && !error && !contextError && (
        <>
          <div className="d-flex justify-content-between align-items-center mb-3">
            <h2>Ticket Places</h2>
            <div className="d-flex align-items-center">
              <Form.Select 
                value={statusFilter} 
                onChange={handleStatusChange}
                className="me-3"
                style={{ width: 'auto' }}
              >
                <option value="Active">Active</option>
                <option value="Inactive">Inactive</option>
                <option value="ComingSoon">Coming Soon</option>
                <option value="Ended">Ended</option>
              </Form.Select>
              <Button variant="outline-primary" onClick={() => fetchTicketPlaces(statusFilter)} className="me-2">
                Refresh
              </Button>
              <Button variant="primary" onClick={handleAddClick}>
                Create Ticket Place
              </Button>
            </div>
          </div>

          {filteredPlaces.length === 0 ? (
            <div className="text-center py-5 bg-light rounded">
              <h4>No Ticket Places Found</h4>
              <p className="text-muted mb-4">Get started by creating your first ticket place</p>
              <Button variant="primary" size="lg" onClick={handleAddClick}>
                Create Ticket Place
              </Button>
            </div>
          ) : (
            <Table striped bordered hover>
              <thead>
                <tr>
                  <th>Name</th>
                  <th>Description</th>
                  <th>Type</th>
                  <th>Status</th>
                  <th>Priority</th>
                </tr>
              </thead>
              <tbody>
                {filteredPlaces.map(place => (
                  <tr 
                    key={place.id} 
                    onClick={editingPriority !== place.id ? () => onSelectPlace(place) : undefined}
                    style={{ cursor: editingPriority !== place.id ? 'pointer' : 'default' }}
                    className="hover-highlight"
                  >
                    <td>{place.name}</td>
                    <td>{place.description}</td>
                    <td>{place.placeType}</td>
                    <td>{place.status}</td>
                    <td onClick={(e) => e.stopPropagation()}>
                      {editingPriority === place.id ? (
                        <div className="d-flex align-items-center">
                          <Form.Control
                            type="text"
                            pattern="[0-9]*"
                            inputMode="numeric"
                            value={priorityValue}
                            onChange={handlePriorityChange}
                            onKeyDown={(e) => handlePriorityKeyPress(e, place)}
                            onBlur={() => handlePrioritySave(place)}
                            autoFocus
                            style={inputStyles}
                          />
                        </div>
                      ) : (
                        <div 
                          onClick={() => handlePriorityEdit(place.id, place.priority)}
                          style={{ cursor: 'text' }}
                        >
                          {place.priority}
                        </div>
                      )}
                    </td>
                  </tr>
                ))}
              </tbody>
            </Table>
          )}
        </>
      )}

      <TicketPlaceModal
        show={showModal}
        onHide={handleCloseModal}
        place={currentPlace}
        onSave={handleSave}
      />
    </Container>
  );
};

export default TicketPlaceList; 