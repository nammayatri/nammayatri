import React from 'react';
import { Modal, Button, Badge, ListGroup } from 'react-bootstrap';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import {
  faEdit,
  faTrash,
  faPlus,
  faMapMarkerAlt,
  faTicketAlt,
  faTag,
  faUsers,
  faClock
} from '@fortawesome/free-solid-svg-icons';
import { useTicket } from '../../contexts/TicketContext';
import { formatTime, findEntityById } from '../../utils/helpers';
import './EntityDetailsModal.css';

const EntityDetailsModal = ({ show, onHide, selectedNode, onEdit, onDelete, onAdd }) => {
  const { getActiveTicketPlace } = useTicket();
  const activePlace = getActiveTicketPlace();

  if (!selectedNode || !selectedNode.data) {
    return null;
  }

  const { type, data } = selectedNode;

  // Get icon for entity type
  const getEntityIcon = () => {
    switch (type) {
      case 'place':
        return faMapMarkerAlt;
      case 'service':
        return faTicketAlt;
      case 'category':
        return faTag;
      case 'peopleCategory':
        return faUsers;
      case 'businessHour':
        return faClock;
      default:
        return faMapMarkerAlt;
    }
  };

  // Get title for entity type
  const getEntityTitle = () => {
    switch (type) {
      case 'place':
        return 'Ticket Place';
      case 'service':
        return 'Service';
      case 'category':
        return 'Service Category';
      case 'peopleCategory':
        return 'People Category';
      case 'businessHour':
        return 'Business Hour';
      default:
        return 'Entity';
    }
  };

  // Get action buttons based on entity type
  const getActionButtons = () => {
    return (
      <div className="mb-3">
        {onEdit && (
          <Button
            variant="outline-primary"
            size="sm"
            className="me-2"
            onClick={() => {
              onEdit(type, data);
              onHide();
            }}
          >
            <FontAwesomeIcon icon={faEdit} className="me-1" /> Edit
          </Button>
        )}

        {onDelete && (
          <Button
            variant="outline-danger"
            size="sm"
            className="me-2"
            onClick={() => {
              onDelete(type, data);
              onHide();
            }}
          >
            <FontAwesomeIcon icon={faTrash} className="me-1" /> Delete
          </Button>
        )}

        {onAdd && type === 'place' && (
          <Button
            variant="outline-success"
            size="sm"
            onClick={() => {
              onAdd('service');
              onHide();
            }}
          >
            <FontAwesomeIcon icon={faPlus} className="me-1" /> Add Service
          </Button>
        )}

        {onAdd && type === 'service' && (
          <>
            <Button
              variant="outline-success"
              size="sm"
              className="me-2"
              onClick={() => {
                onAdd('category', data);
                onHide();
              }}
            >
              <FontAwesomeIcon icon={faPlus} className="me-1" /> Add Category
            </Button>
            <Button
              variant="outline-success"
              size="sm"
              onClick={() => {
                onAdd('businessHour', data);
                onHide();
              }}
            >
              <FontAwesomeIcon icon={faPlus} className="me-1" /> Add Business Hour
            </Button>
          </>
        )}

        {onAdd && type === 'category' && (
          <Button
            variant="outline-success"
            size="sm"
            onClick={() => {
              onAdd('peopleCategory', data);
              onHide();
            }}
          >
            <FontAwesomeIcon icon={faPlus} className="me-1" /> Add People Category
          </Button>
        )}
      </div>
    );
  };

  // Render place details
  const renderPlaceDetails = () => {
    const place = data;
    return (
      <>
        <ListGroup variant="flush">
          <ListGroup.Item>
            <strong>Name:</strong> {place.name}
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Description:</strong> {place.description}
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Address:</strong> {place.address}
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Coordinates:</strong> {place.latitude}, {place.longitude}
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Status:</strong> <Badge bg="success">{place.status}</Badge>
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Place Type:</strong> {place.placeType}
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Allow Same Day Booking:</strong> {place.allowSameDayBooking ? 'Yes' : 'No'}
          </ListGroup.Item>
        </ListGroup>
      </>
    );
  };

  // Helper to render expiry
  const renderExpiry = (expiry) => {
    if (!expiry) return 'None';

    if (expiry.type === 'VisitDate') {
      return `Visit Date (${expiry.time})`;
    } else if (expiry.type === 'InstantExpiry') {
      return `Instant (${expiry.minutes} minutes)`;
    }

    return JSON.stringify(expiry);
  };

  // Render service details
  const renderServiceDetails = () => {
    const service = data;
    return (
      <>
        <ListGroup variant="flush">
          <ListGroup.Item>
            <strong>Name:</strong> {service.service}
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Description:</strong> {service.shortDesc}
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Max Verification:</strong> {service.maxVerification}
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Allow Future Booking:</strong> {service.allowFutureBooking ? 'Yes' : 'No'}
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Allow Cancellation:</strong> {service.allowCancellation ? 'Yes' : 'No'}
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Expiry:</strong> {renderExpiry(service.expiry)}
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Operational Days:</strong>
            <div>
              {service.operationalDays && service.operationalDays.map((day, index) => (
                <Badge key={index} bg="info" className="me-1 mb-1">{day}</Badge>
              ))}
            </div>
          </ListGroup.Item>
        </ListGroup>

        {/* Business Hours */}
        {service.businessHours && activePlace && activePlace.businessHours && (
          <div className="mt-3">
            <h6>Business Hours:</h6>
            <ListGroup>
              {service.businessHours.map(hourId => {
                const hour = activePlace.businessHours.find(h => h.id === hourId);
                if (!hour) return null;

                return (
                  <ListGroup.Item key={hour.id} className="d-flex align-items-center">
                    <FontAwesomeIcon icon={faClock} className="me-2 text-warning" />
                    {hour.name}
                    {hour.btype && hour.btype.type === 'Slot' && ` (${hour.btype.time})`}
                    {hour.btype && hour.btype.type === 'Duration' && ` (${hour.btype.startTime} - ${hour.btype.endTime})`}
                  </ListGroup.Item>
                );
              })}
            </ListGroup>
          </div>
        )}

        {/* Categories */}
        {activePlace && activePlace.serviceCategories && (
          <div className="mt-3">
            <h6>Service Categories:</h6>
            <ListGroup>
              {activePlace.serviceCategories
                .filter(category => {
                  // Find if this category is associated with this service
                  // through business hours
                  if (!category.id || !service.businessHours) return false;

                  const relevantHours = activePlace.businessHours.filter(
                    hour => service.businessHours.includes(hour.id) &&
                    hour.categoryId && hour.categoryId.includes(category.id)
                  );

                  return relevantHours.length > 0;
                })
                .map(category => (
                  <ListGroup.Item key={category.id} className="d-flex align-items-center">
                    <FontAwesomeIcon icon={faTag} className="me-2 text-success" />
                    {category.name}
                  </ListGroup.Item>
                ))
              }
            </ListGroup>
          </div>
        )}
      </>
    );
  };

  // Render category details
  const renderCategoryDetails = () => {
    const category = data;
    return (
      <>
        <ListGroup variant="flush">
          <ListGroup.Item>
            <strong>Name:</strong> {category.name}
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Description:</strong> {category.description}
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Available Seats:</strong> {category.availableSeats}
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Allowed Seats:</strong> {category.allowedSeats}
          </ListGroup.Item>
        </ListGroup>

        {/* People Categories */}
        {category.peopleCategory && activePlace && activePlace.servicePeopleCategories && (
          <div className="mt-3">
            <h6>People Categories:</h6>
            <ListGroup>
              {category.peopleCategory.map(peopleId => {
                const peopleCategory = activePlace.servicePeopleCategories.find(pc => pc.id === peopleId);
                if (!peopleCategory) return null;

                return (
                  <ListGroup.Item key={peopleCategory.id} className="d-flex align-items-center">
                    <FontAwesomeIcon icon={faUsers} className="me-2 text-primary" />
                    {peopleCategory.name} - {peopleCategory.priceAmount} {peopleCategory.priceCurrency}
                  </ListGroup.Item>
                );
              })}
            </ListGroup>
          </div>
        )}
      </>
    );
  };

  // Render people category details
  const renderPeopleCategoryDetails = () => {
    const peopleCategory = data;
    return (
      <ListGroup variant="flush">
        <ListGroup.Item>
          <strong>Name:</strong> {peopleCategory.name}
        </ListGroup.Item>
        <ListGroup.Item>
          <strong>Description:</strong> {peopleCategory.description}
        </ListGroup.Item>
        <ListGroup.Item>
          <strong>Price:</strong> {peopleCategory.priceAmount} {peopleCategory.priceCurrency}
        </ListGroup.Item>
        <ListGroup.Item>
          <strong>Pricing Type:</strong> {peopleCategory.pricingType}
        </ListGroup.Item>
      </ListGroup>
    );
  };

  // Render business hour details
  const renderBusinessHourDetails = () => {
    const businessHour = data;
    return (
      <>
        <ListGroup variant="flush">
          <ListGroup.Item>
            <strong>Name:</strong> {businessHour.name}
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Type:</strong> {businessHour.btype?.type || 'Unknown'}
          </ListGroup.Item>
          {businessHour.btype?.type === 'Slot' && (
            <ListGroup.Item>
              <strong>Time:</strong> {businessHour.btype.time}
            </ListGroup.Item>
          )}
          {businessHour.btype?.type === 'Duration' && (
            <>
              <ListGroup.Item>
                <strong>Start Time:</strong> {businessHour.btype.startTime}
              </ListGroup.Item>
              <ListGroup.Item>
                <strong>End Time:</strong> {businessHour.btype.endTime}
              </ListGroup.Item>
            </>
          )}
          <ListGroup.Item>
            <strong>Booking Closing Time:</strong> {businessHour.bookingClosingTime}
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Days:</strong>
            <div>
              {businessHour.days && businessHour.days.map((day, index) => (
                <Badge key={index} bg="info" className="me-1 mb-1">{day}</Badge>
              ))}
            </div>
          </ListGroup.Item>
        </ListGroup>

        {/* Categories */}
        {businessHour.categoryId && activePlace && activePlace.serviceCategories && (
          <div className="mt-3">
            <h6>Categories:</h6>
            <ListGroup>
              {businessHour.categoryId.map(categoryId => {
                const category = activePlace.serviceCategories.find(c => c.id === categoryId);
                if (!category) return null;

                return (
                  <ListGroup.Item key={category.id} className="d-flex align-items-center">
                    <FontAwesomeIcon icon={faTag} className="me-2 text-success" />
                    {category.name}
                  </ListGroup.Item>
                );
              })}
            </ListGroup>
          </div>
        )}
      </>
    );
  };

  // Render details based on entity type
  const renderEntityDetails = () => {
    switch (type) {
      case 'place':
        return renderPlaceDetails();
      case 'service':
        return renderServiceDetails();
      case 'category':
        return renderCategoryDetails();
      case 'peopleCategory':
        return renderPeopleCategoryDetails();
      case 'businessHour':
        return renderBusinessHourDetails();
      default:
        return <p>Unknown entity type</p>;
    }
  };

  return (
    <Modal
      show={show}
      onHide={onHide}
      size="lg"
      centered
      className="entity-details-modal"
    >
      <Modal.Header closeButton>
        <Modal.Title className="d-flex align-items-center">
          <FontAwesomeIcon icon={getEntityIcon()} className="me-2" />
          {getEntityTitle()}: {data.name || data.service}
        </Modal.Title>
      </Modal.Header>
      <Modal.Body>
        {getActionButtons()}
        {renderEntityDetails()}
      </Modal.Body>
      <Modal.Footer>
        <Button variant="secondary" onClick={onHide}>
          Close
        </Button>
      </Modal.Footer>
    </Modal>
  );
};

export default EntityDetailsModal;
