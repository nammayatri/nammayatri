import React, { useState, useEffect } from 'react';
import { Modal, Button, Form, Row, Col } from 'react-bootstrap';
import { useTicket } from '../../contexts/TicketContext';
import { getBusinessHourTypes } from '../../utils/helpers';

const BusinessHoursModal = ({ show, onHide, businessHour }) => {
  const { getActiveTicketPlace, addBusinessHour, updateBusinessHour } = useTicket();
  const activePlace = getActiveTicketPlace();
  const [formData, setFormData] = useState({
    name: '',
    btypeType: 'Slot',
    slotTime: '09:00:00',
    durationStartTime: '09:00:00',
    durationEndTime: '17:00:00',
    bookingClosingTime: '08:00:00',
    categoryId: []
    // Note: 'days' field was removed as it doesn't exist in the backend BusinessHour type
  });

  // Initialize form data when editing an existing business hour
  useEffect(() => {
    if (businessHour) {
      const btypeType = businessHour.btype?.tag || 'Slot';

      setFormData({
        name: businessHour.name || '',
        btypeType,
        slotTime: btypeType === 'Slot' ? businessHour.btype?.contents || '09:00:00' : '09:00:00',
        durationStartTime: btypeType === 'Duration' ? businessHour.btype?.contents[0] || '09:00:00' : '09:00:00',
        durationEndTime: btypeType === 'Duration' ? businessHour.btype?.contents[1] || '17:00:00' : '17:00:00',
        bookingClosingTime: businessHour.bookingClosingTime || '08:00:00',
        categoryId: businessHour.categoryId || []
      });
    } else {
      // Reset form for new business hour
      setFormData({
        name: '',
        btypeType: 'Slot',
        slotTime: '09:00:00',
        durationStartTime: '09:00:00',
        durationEndTime: '17:00:00',
        bookingClosingTime: '08:00:00',
        categoryId: []
      });
    }
  }, [businessHour, show]);

  const handleChange = (e) => {
    const { name, value } = e.target;
    setFormData(prev => ({
      ...prev,
      [name]: value
    }));
  };

  // Note: handleDayChange function was removed as the 'days' field doesn't exist in the backend BusinessHour type

  const handleCategoryChange = (id) => {
    setFormData(prev => {
      const currentSelection = [...prev.categoryId];

      if (currentSelection.includes(id)) {
        // Remove if already selected
        return {
          ...prev,
          categoryId: currentSelection.filter(item => item !== id)
        };
      } else {
        // Add if not selected
        return {
          ...prev,
          categoryId: [...currentSelection, id]
        };
      }
    });
  };

  const handleSubmit = () => {
    // Validate form
    if (!formData.name) {
      alert('Please enter a name for the business hours');
      return;
    }

    // Validate time values
    if (formData.btypeType === 'Slot' && !formData.slotTime) {
      alert('Please enter a valid slot time');
      return;
    }

    if (formData.btypeType === 'Duration') {
      if (!formData.durationStartTime) {
        alert('Please enter a valid start time');
        return;
      }
      if (!formData.durationEndTime) {
        alert('Please enter a valid end time');
        return;
      }
    }

    if (!formData.bookingClosingTime) {
      alert('Please enter a valid booking closing time');
      return;
    }

    // Ensure time values are in proper format (HH:mm:ss)
    const formatTimeValue = (timeValue) => {
      if (!timeValue) return '00:00:00';
      // If time doesn't include seconds, add :00
      return timeValue.split(':').length === 2 ? `${timeValue}:00` : timeValue;
    };

    // Prepare btype based on type
    let btype;
    if (formData.btypeType === 'Slot') {
      btype = {
        tag: 'Slot',
        contents: formatTimeValue(formData.slotTime)
      };
    } else {
      btype = {
        tag: 'Duration',
        contents: [
          formatTimeValue(formData.durationStartTime),
          formatTimeValue(formData.durationEndTime)
        ]
      };
    }

    // Prepare business hour data
    const businessHourData = {
      name: formData.name,
      btype,
      bookingClosingTime: formatTimeValue(formData.bookingClosingTime),
      categoryId: formData.categoryId || []
    };

    // Save business hour
    if (businessHour) {
      updateBusinessHour(businessHour.id, businessHourData);
    } else {
      addBusinessHour(businessHourData);
    }

    onHide();
  };

  return (
    <Modal show={show} onHide={onHide} size="lg">
      <Modal.Header closeButton>
        <Modal.Title>
          {businessHour ? 'Edit Business Hours' : 'Add Business Hours'}
        </Modal.Title>
      </Modal.Header>
      <Modal.Body>
        <Form>
          <Form.Group className="mb-3">
            <Form.Label>Name <span className="text-danger">*</span></Form.Label>
            <Form.Control
              type="text"
              name="name"
              value={formData.name}
              onChange={handleChange}
              placeholder="e.g., Morning Hours"
            />
          </Form.Group>

          <Form.Group className="mb-3">
            <Form.Label>Type</Form.Label>
            <Form.Select
              name="btypeType"
              value={formData.btypeType}
              onChange={handleChange}
            >
              <option value="Slot">Slot (Specific Time)</option>
              <option value="Duration">Duration (Time Range)</option>
            </Form.Select>
          </Form.Group>

          {formData.btypeType === 'Slot' ? (
            <Form.Group className="mb-3">
              <Form.Label>Slot Time</Form.Label>
              <Form.Control
                type="time"
                name="slotTime"
                value={formData.slotTime}
                onChange={handleChange}
              />
            </Form.Group>
          ) : (
            <Row className="mb-3">
              <Col>
                <Form.Group>
                  <Form.Label>Start Time</Form.Label>
                  <Form.Control
                    type="time"
                    name="durationStartTime"
                    value={formData.durationStartTime}
                    onChange={handleChange}
                  />
                </Form.Group>
              </Col>
              <Col>
                <Form.Group>
                  <Form.Label>End Time</Form.Label>
                  <Form.Control
                    type="time"
                    name="durationEndTime"
                    value={formData.durationEndTime}
                    onChange={handleChange}
                  />
                </Form.Group>
              </Col>
            </Row>
          )}

          <Form.Group className="mb-3">
            <Form.Label>Booking Closing Time</Form.Label>
            <Form.Control
              type="time"
              name="bookingClosingTime"
              value={formData.bookingClosingTime}
              onChange={handleChange}
            />
            <Form.Text className="text-muted">
              Time after which bookings are no longer accepted for this business hour
            </Form.Text>
          </Form.Group>

          <Form.Group className="mb-3">
            <Form.Label>Service Categories</Form.Label>
            <div className="border rounded p-3" style={{ maxHeight: '200px', overflowY: 'auto' }}>
              {activePlace && activePlace.serviceCategories && activePlace.serviceCategories.length > 0 ? (
                activePlace.serviceCategories.map(category => (
                  <Form.Check
                    key={category.id}
                    type="checkbox"
                    id={`category-${category.id}`}
                    label={
                      <>
                        {category.name ? (
                          <>
                            {category.name}
                            <span className="text-muted ms-2">({category.id})</span>
                          </>
                        ) : (
                          category.id
                        )}
                      </>
                    }
                    checked={formData.categoryId.includes(category.id)}
                    onChange={() => handleCategoryChange(category.id)}
                    className="mb-2"
                  />
                ))
              ) : (
                <p className="text-muted mb-0">No service categories available. Please create some first.</p>
              )}
            </div>
            {activePlace && activePlace.serviceCategories && activePlace.serviceCategories.length > 0 && (
              <div className="d-flex justify-content-between mt-2">
                <Form.Text className="text-muted">
                  Select all applicable service categories for this business hour
                </Form.Text>
                <Button
                  variant="link"
                  size="sm"
                  onClick={() => {
                    // Toggle all: If all are selected, deselect all. Otherwise, select all.
                    const allIds = activePlace.serviceCategories.map(cat => cat.id);
                    const allSelected = allIds.every(id => formData.categoryId.includes(id));

                    setFormData(prev => ({
                      ...prev,
                      categoryId: allSelected ? [] : allIds
                    }));
                  }}
                >
                  {formData.categoryId.length === activePlace.serviceCategories.length ? 'Deselect All' : 'Select All'}
                </Button>
              </div>
            )}
          </Form.Group>

          {/* Note: Days selection was removed as the 'days' field doesn't exist in the backend BusinessHour type */}
        </Form>
      </Modal.Body>
      <Modal.Footer>
        <Button variant="secondary" onClick={onHide}>
          Cancel
        </Button>
        <Button variant="primary" onClick={handleSubmit}>
          {businessHour ? 'Update' : 'Add'}
        </Button>
      </Modal.Footer>
    </Modal>
  );
};

export default BusinessHoursModal;
