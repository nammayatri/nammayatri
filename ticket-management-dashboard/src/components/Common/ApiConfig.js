import React, { useState, useEffect } from 'react';
import { Form, Button, Card, Alert } from 'react-bootstrap';

const ApiConfig = ({ onSave }) => {
  const [baseUrl, setBaseUrl] = useState(() => localStorage.getItem('apiBaseUrl') || 'http://localhost:8017/bap/YATRI/Kochi');
  const [token, setToken] = useState(() => localStorage.getItem('apiToken') || '0466f4fb-6af8-49f5-8d0c-9196101afdc4');
  const [showSuccess, setShowSuccess] = useState(false);

  useEffect(() => {
    // Initialize API config in parent component
    if (onSave && baseUrl && token) {
      onSave({ baseUrl, token });
    }
  }, []);

  const handleSave = () => {
    // Save to localStorage
    localStorage.setItem('apiBaseUrl', baseUrl);
    localStorage.setItem('apiToken', token);

    // Notify parent component
    if (onSave) {
      onSave({ baseUrl, token });
    }

    // Show success message
    setShowSuccess(true);
    setTimeout(() => setShowSuccess(false), 3000);
  };

  return (
    <Card className="mb-4">
      <Card.Header>
        <h4>API Configuration</h4>
      </Card.Header>
      <Card.Body>
        {showSuccess && (
          <Alert variant="success" onClose={() => setShowSuccess(false)} dismissible>
            API configuration saved successfully!
          </Alert>
        )}

        <Form>
          <Form.Group className="mb-3">
            <Form.Label>Base URL</Form.Label>
            <Form.Control
              type="text"
              value={baseUrl}
              onChange={(e) => setBaseUrl(e.target.value)}
              placeholder="Enter API base URL"
            />
            <Form.Text className="text-muted">
              Example: http://localhost:8017/bap/YATRI/Kochi
            </Form.Text>
          </Form.Group>

          <Form.Group className="mb-3">
            <Form.Label>API Token</Form.Label>
            <Form.Control
              type="text"
              value={token}
              onChange={(e) => setToken(e.target.value)}
              placeholder="Enter API token"
            />
            <Form.Text className="text-muted">
              Example: 15117c69-6cb9-4199-b614-23a44c899dc3
            </Form.Text>
          </Form.Group>

          <Button variant="primary" onClick={handleSave}>
            Save Configuration
          </Button>
        </Form>
      </Card.Body>
    </Card>
  );
};

export default ApiConfig;
