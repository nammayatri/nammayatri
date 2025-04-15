import React from 'react';
import { Card, Button, Badge, ListGroup } from 'react-bootstrap';
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
import './EntityDetailsPanel.css';

const EntityDetailsPanel = ({ selectedNode, onEdit, onDelete, onAdd }) => {
  const { ticketData } = useTicket();

  if (!selectedNode || !selectedNode.data) {
    return (
      <Card className="entity-details-panel">
        <Card.Header className="text-center">
          <h5 className="mb-0 card-title">Entity Details</h5>
        </Card.Header>
        <Card.Body className="text-center">
          <p className="text-muted">Select an item from the tree to view details</p>
        </Card.Body>
      </Card>
    );
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
            onClick={() => onEdit(type, data)}
          >
            <FontAwesomeIcon icon={faEdit} className="me-1" /> Edit
          </Button>
        )}

        {onDelete && (
          <Button
            variant="outline-danger"
            size="sm"
            className="me-2"
            onClick={() => onDelete(type, data)}
          >
            <FontAwesomeIcon icon={faTrash} className="me-1" /> Delete
          </Button>
        )}

        {onAdd && type === 'place' && (
          <Button
            variant="outline-success"
            size="sm"
            onClick={() => onAdd('service')}
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
              onClick={() => onAdd('category', data)}
            >
              <FontAwesomeIcon icon={faPlus} className="me-1" /> Add Category
            </Button>
            <Button
              variant="outline-success"
              size="sm"
              onClick={() => onAdd('businessHour', data)}
            >
              <FontAwesomeIcon icon={faPlus} className="me-1" /> Add Business Hour
            </Button>
          </>
        )}

        {onAdd && type === 'category' && (
          <Button
            variant="outline-success"
            size="sm"
            onClick={() => onAdd('peopleCategory', data)}
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

        {ticketData.services && ticketData.services.length > 0 && (
          <div className="mt-3">
            <h6>Services:</h6>
            <ListGroup>
              {ticketData.services.map(service => (
                <ListGroup.Item key={service.id} className="d-flex align-items-center">
                  <FontAwesomeIcon icon={faTicketAlt} className="me-2 text-primary" />
                  {service.service}
                </ListGroup.Item>
              ))}
            </ListGroup>
          </div>
        )}
      </>
    );
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
              {service.operationalDays && service.operationalDays.map(day => (
                <Badge key={day} bg="secondary" className="me-1">{day}</Badge>
              ))}
            </div>
          </ListGroup.Item>
        </ListGroup>

        {/* Business Hours */}
        {service.businessHours && service.businessHours.length > 0 && (
          <div className="mt-3">
            <h6>Business Hours:</h6>
            <ListGroup>
              {service.businessHours.map(hourId => {
                const hour = findEntityById(ticketData.businessHours, hourId);
                return hour ? (
                  <ListGroup.Item key={hourId} className="d-flex align-items-center">
                    <FontAwesomeIcon icon={faClock} className="me-2 text-warning" />
                    {hour.name} - {renderBusinessHourTime(hour)}
                  </ListGroup.Item>
                ) : null;
              })}
            </ListGroup>
          </div>
        )}

        {/* Categories */}
        {ticketData.serviceCategories && (
          <div className="mt-3">
            <h6>Service Categories:</h6>
            <ListGroup>
              {ticketData.serviceCategories
                .filter(category => {
                  // Find if this category is associated with this service
                  // through business hours
                  if (!category.id || !service.businessHours) return false;

                  const relevantHours = ticketData.businessHours.filter(
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
        {category.peopleCategory && category.peopleCategory.length > 0 && (
          <div className="mt-3">
            <h6>People Categories:</h6>
            <ListGroup>
              {category.peopleCategory.map(peopleId => {
                const people = findEntityById(ticketData.servicePeopleCategories, peopleId);
                return people ? (
                  <ListGroup.Item key={peopleId} className="d-flex align-items-center">
                    <FontAwesomeIcon icon={faUsers} className="me-2 text-purple" />
                    {people.name} - {people.priceAmount} {people.priceCurrency}
                  </ListGroup.Item>
                ) : null;
              })}
            </ListGroup>
          </div>
        )}

        {/* Associated Business Hours */}
        {ticketData.businessHours && (
          <div className="mt-3">
            <h6>Associated Business Hours:</h6>
            <ListGroup>
              {ticketData.businessHours
                .filter(hour => hour.categoryId && hour.categoryId.includes(category.id))
                .map(hour => (
                  <ListGroup.Item key={hour.id} className="d-flex align-items-center">
                    <FontAwesomeIcon icon={faClock} className="me-2 text-warning" />
                    {hour.name} - {renderBusinessHourTime(hour)}
                  </ListGroup.Item>
                ))
              }
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
            <strong>Type:</strong> {businessHour.btype?.type}
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Time:</strong> {renderBusinessHourTime(businessHour)}
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Booking Closing Time:</strong> {formatTime(businessHour.bookingClosingTime)}
          </ListGroup.Item>
          <ListGroup.Item>
            <strong>Days:</strong>
            <div>
              {businessHour.days && businessHour.days.map(day => (
                <Badge key={day} bg="secondary" className="me-1">{day}</Badge>
              ))}
            </div>
          </ListGroup.Item>
        </ListGroup>

        {/* Associated Categories */}
        {businessHour.categoryId && businessHour.categoryId.length > 0 && (
          <div className="mt-3">
            <h6>Associated Categories:</h6>
            <ListGroup>
              {businessHour.categoryId.map(catId => {
                const category = findEntityById(ticketData.serviceCategories, catId);
                return category ? (
                  <ListGroup.Item key={catId} className="d-flex align-items-center">
                    <FontAwesomeIcon icon={faTag} className="me-2 text-success" />
                    {category.name}
                  </ListGroup.Item>
                ) : null;
              })}
            </ListGroup>
          </div>
        )}

        {/* Associated Services */}
        {ticketData.services && (
          <div className="mt-3">
            <h6>Associated Services:</h6>
            <ListGroup>
              {ticketData.services
                .filter(service => service.businessHours && service.businessHours.includes(businessHour.id))
                .map(service => (
                  <ListGroup.Item key={service.id} className="d-flex align-items-center">
                    <FontAwesomeIcon icon={faTicketAlt} className="me-2 text-primary" />
                    {service.service}
                  </ListGroup.Item>
                ))
              }
            </ListGroup>
          </div>
        )}
      </>
    );
  };

  // Helper to render business hour time
  const renderBusinessHourTime = (businessHour) => {
    if (!businessHour.btype) return 'N/A';

    if (businessHour.btype.type === 'Slot') {
      return formatTime(businessHour.btype.time);
    } else if (businessHour.btype.type === 'Duration') {
      return `${formatTime(businessHour.btype.startTime)} - ${formatTime(businessHour.btype.endTime)}`;
    }

    return 'N/A';
  };

  // Helper to render expiry
  const renderExpiry = (expiry) => {
    if (!expiry) return 'N/A';

    if (expiry.type === 'VisitDate') {
      return `Visit Date (${expiry.time})`;
    } else if (expiry.type === 'InstantExpiry') {
      return `Instant (${expiry.minutes} minutes)`;
    }

    return 'N/A';
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
    <Card className="entity-details-panel">
      <Card.Header className="d-flex align-items-center">
        <FontAwesomeIcon icon={getEntityIcon()} className="me-2" />
        <h5 className="mb-0 card-title">{getEntityTitle()}: {data.name || data.service}</h5>
      </Card.Header>
      <Card.Body>
        {getActionButtons()}
        {renderEntityDetails()}
      </Card.Body>
    </Card>
  );
};

export default EntityDetailsPanel;
