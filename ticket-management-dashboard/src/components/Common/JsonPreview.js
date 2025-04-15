import React, { useState, useEffect, useRef } from 'react';
import { Card, Button, ButtonGroup, Form, Alert } from 'react-bootstrap';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome';
import { faDownload, faSync, faSave, faEdit } from '@fortawesome/free-solid-svg-icons';
import { useTicket } from '../../contexts/TicketContext';
import './JsonPreview.css';

const JsonPreview = () => {
  const { jsonPreview, setJsonPreview, downloadJsonFile, generateApiJson, updateTicketPlace, getActiveTicketPlace } = useTicket();
  const [isFlashing, setIsFlashing] = useState(false);
  const [isEditing, setIsEditing] = useState(false);
  const [editableJson, setEditableJson] = useState('');
  const [error, setError] = useState(null);
  const prevJsonRef = useRef('');
  const jsonOutputRef = useRef(null);
  const textareaRef = useRef(null);

  // Flash the JSON preview when it changes
  useEffect(() => {
    if (prevJsonRef.current !== jsonPreview) {
      setIsFlashing(true);
      const timer = setTimeout(() => {
        setIsFlashing(false);
      }, 500);

      prevJsonRef.current = jsonPreview;

      return () => clearTimeout(timer);
    }
  }, [jsonPreview]);

  const handleDownload = () => {
    downloadJsonFile();
  };

  const handleRefresh = () => {
    generateApiJson();
    setIsFlashing(true);
    setTimeout(() => {
      setIsFlashing(false);
    }, 500);
  };

  const handleEdit = () => {
    setEditableJson(jsonPreview);
    setIsEditing(true);
    setError(null);

    // Focus the textarea after it's rendered
    setTimeout(() => {
      if (textareaRef.current) {
        textareaRef.current.focus();
      }
    }, 100);
  };

  const handleSave = () => {
    try {
      // Parse the JSON to validate it
      const parsedJson = JSON.parse(editableJson);

      // Check if it has an id
      if (!parsedJson.id) {
        setError('JSON must contain an id field');
        return;
      }

      // Update the ticket place
      updateTicketPlace(parsedJson);

      // Update the JSON preview
      setJsonPreview(JSON.stringify(parsedJson, null, 2));

      // Exit edit mode
      setIsEditing(false);
      setError(null);

      // Flash to indicate success
      setIsFlashing(true);
      setTimeout(() => {
        setIsFlashing(false);
      }, 500);
    } catch (error) {
      setError(`Invalid JSON: ${error.message}`);
    }
  };

  const handleCancel = () => {
    setIsEditing(false);
    setError(null);
  };

  return (
    <Card className="mb-4">
      <Card.Header className="d-flex justify-content-between align-items-center">
        <h5 className="mb-0">JSON View</h5>
        <ButtonGroup>
          {isEditing ? (
            <>
              <Button variant="success" size="sm" onClick={handleSave} title="Save Changes">
                <FontAwesomeIcon icon={faSave} /> Save
              </Button>
              <Button variant="outline-secondary" size="sm" onClick={handleCancel} title="Cancel Editing">
                Cancel
              </Button>
            </>
          ) : (
            <>
              <Button variant="outline-primary" size="sm" onClick={handleEdit} title="Edit JSON">
                <FontAwesomeIcon icon={faEdit} /> Edit
              </Button>
              <Button variant="outline-primary" size="sm" onClick={handleRefresh} title="Refresh JSON">
                <FontAwesomeIcon icon={faSync} /> Refresh
              </Button>
              <Button variant="primary" size="sm" onClick={handleDownload} title="Download JSON">
                <FontAwesomeIcon icon={faDownload} /> Download
              </Button>
            </>
          )}
        </ButtonGroup>
      </Card.Header>
      <Card.Body>
        {error && (
          <Alert variant="danger" className="mb-3">
            {error}
          </Alert>
        )}
        <div className="alert alert-info">
          <i className="bi bi-info-circle"></i> {isEditing ? 'Edit the JSON below. Changes will be applied when you click Save.' : 'This is the JSON representation of the current ticket place. Click Edit to modify it directly.'}
        </div>
        {isEditing ? (
          <Form.Control
            as="textarea"
            ref={textareaRef}
            value={editableJson}
            onChange={(e) => setEditableJson(e.target.value)}
            className="json-editor"
            style={{
              fontFamily: 'monospace',
              whiteSpace: 'pre-wrap',
              padding: '15px',
              borderRadius: '5px',
              minHeight: '500px',
              fontSize: '14px',
              lineHeight: '1.5',
              width: '100%'
            }}
          />
        ) : (
          <pre
            ref={jsonOutputRef}
            className={`json-output ${isFlashing ? 'flashing' : ''}`}
          >
            {jsonPreview || '// No JSON data available. Select a ticket place to see its JSON representation.'}
          </pre>
        )}
      </Card.Body>
    </Card>
  );
};

export default JsonPreview;
