/**
 * Types Tests
 * Validates TypeScript type definitions and interfaces
 */

import {
  StepStatus,
  Step,
  StepResult,
  Scenario,
  FlowGroup,
  Config,
  LogEntry,
} from './index';

describe('Type Definitions', () => {
  describe('StepStatus', () => {
    it('should accept valid step statuses', () => {
      const statuses: StepStatus[] = ['pending', 'running', 'pass', 'fail', 'skip'];
      expect(statuses).toHaveLength(5);
    });

    it('should validate step status values', () => {
      const validStatuses = ['pending', 'running', 'pass', 'fail', 'skip'];
      validStatuses.forEach(status => {
        expect(['pending', 'running', 'pass', 'fail', 'skip']).toContain(status);
      });
    });
  });

  describe('Step Interface', () => {
    it('should create a valid Step object', () => {
      const step: Step = {
        id: 'test-step',
        name: 'Test Step',
        method: 'GET',
        service: 'rider',
        path: '/test',
      };

      expect(step.id).toBe('test-step');
      expect(step.name).toBe('Test Step');
      expect(step.method).toBe('GET');
      expect(step.service).toBe('rider');
      expect(step.path).toBe('/test');
    });

    it('should create a Step with all optional fields', () => {
      const step: Step = {
        id: 'full-step',
        name: 'Full Step',
        method: 'POST',
        service: 'driver',
        path: (ctx) => `/dynamic/${ctx.id}`,
        auth: true,
        body: (ctx: Record<string, any>) => ({ data: ctx.data }),
        assert: (data) => data.success ? null : 'Failed',
        save: (data, ctx) => { ctx.saved = data; },
        note: 'Test note',
        extraHeaders: { 'X-Custom': 'value' },
        skip: false,
        useCatalog: 'catalog-id',
        poll: { intervalMs: 1000, timeoutMs: 30000 },
        summary: (data, ctx) => `Result: ${data.status}`,
      };

      expect(step.auth).toBe(true);
      expect(step.note).toBe('Test note');
      expect(step.skip).toBe(false);
      expect(step.useCatalog).toBe('catalog-id');
      expect(step.poll).toEqual({ intervalMs: 1000, timeoutMs: 30000 });
    });

    it('should support all HTTP methods', () => {
      const methods: Array<'GET' | 'POST' | 'PUT' | 'DELETE'> = ['GET', 'POST', 'PUT', 'DELETE'];

      methods.forEach(method => {
        const step: Step = {
          id: `step-${method}`,
          name: `Step ${method}`,
          method,
          service: 'rider',
          path: '/test',
        };
        expect(step.method).toBe(method);
      });
    });

    it('should support all service types', () => {
      const services: Array<Step['service']> = [
        'rider',
        'driver',
        'lts',
        'internal',
        'provider-dashboard',
        'mock-idfy',
      ];

      services.forEach(service => {
        const step: Step = {
          id: `step-${service}`,
          name: `Step ${service}`,
          method: 'GET',
          service,
          path: '/test',
        };
        expect(step.service).toBe(service);
      });
    });

    it('should support dynamic path function', () => {
      const step: Step = {
        id: 'dynamic-path',
        name: 'Dynamic Path',
        method: 'GET',
        service: 'rider',
        path: (ctx) => `/items/${ctx.itemId}`,
      };

      const ctx = { itemId: '123' };
      const resolvedPath = typeof step.path === 'function' ? step.path(ctx) : step.path;
      expect(resolvedPath).toBe('/items/123');
    });

    it('should support dynamic body function', () => {
      const step: Step = {
        id: 'dynamic-body',
        name: 'Dynamic Body',
        method: 'POST',
        service: 'rider',
        path: '/test',
        body: (ctx: Record<string, any>) => ({ id: ctx.id, name: ctx.name }),
      };

      const ctx = { id: '1', name: 'Test' };
      const body = typeof step.body === 'function' ? step.body(ctx) : step.body;
      expect(body).toEqual({ id: '1', name: 'Test' });
    });

    it('should support skip as function', () => {
      const step: Step = {
        id: 'conditional-skip',
        name: 'Conditional Skip',
        method: 'GET',
        service: 'rider',
        path: '/test',
        skip: (ctx) => ctx.shouldSkip === true,
      };

      expect(typeof step.skip).toBe('function');
      expect(step.skip).toBeDefined();
    });
  });

  describe('StepResult Interface', () => {
    it('should create a valid StepResult', () => {
      const result: StepResult = {
        stepId: 'step-1',
        status: 'pass',
        durationMs: 150,
        response: { data: 'test' },
        statusCode: 200,
        summary: 'Success',
      };

      expect(result.stepId).toBe('step-1');
      expect(result.status).toBe('pass');
      expect(result.durationMs).toBe(150);
      expect(result.response).toEqual({ data: 'test' });
      expect(result.statusCode).toBe(200);
      expect(result.summary).toBe('Success');
    });

    it('should create a failed StepResult with error', () => {
      const result: StepResult = {
        stepId: 'step-2',
        status: 'fail',
        durationMs: 50,
        error: 'Network error',
        statusCode: 500,
      };

      expect(result.status).toBe('fail');
      expect(result.error).toBe('Network error');
      expect(result.response).toBeUndefined();
    });
  });

  describe('Scenario Interface', () => {
    it('should create a valid Scenario', () => {
      const scenario: Scenario = {
        id: 'scenario-1',
        name: 'Test Scenario',
        description: 'A test scenario',
        steps: [
          {
            id: 'step-1',
            name: 'Step 1',
            method: 'GET',
            service: 'rider',
            path: '/test',
          },
        ],
      };

      expect(scenario.id).toBe('scenario-1');
      expect(scenario.name).toBe('Test Scenario');
      expect(scenario.description).toBe('A test scenario');
      expect(scenario.steps).toHaveLength(1);
    });

    it('should support empty steps array', () => {
      const scenario: Scenario = {
        id: 'empty-scenario',
        name: 'Empty Scenario',
        description: 'No steps',
        steps: [],
      };

      expect(scenario.steps).toHaveLength(0);
    });
  });

  describe('FlowGroup Interface', () => {
    it('should create a valid FlowGroup', () => {
      const flowGroup: FlowGroup = {
        id: 'flow-1',
        name: 'Test Flow',
        scenarios: [
          {
            id: 'scenario-1',
            name: 'Scenario 1',
            description: 'First scenario',
            steps: [],
          },
        ],
      };

      expect(flowGroup.id).toBe('flow-1');
      expect(flowGroup.name).toBe('Test Flow');
      expect(flowGroup.scenarios).toHaveLength(1);
    });
  });

  describe('Config Interface', () => {
    it('should create a valid Config', () => {
      const config: Config = {
        riderUrl: 'http://localhost:8013',
        driverUrl: 'http://localhost:8016',
        token: 'test-token',
        stripeUrl: 'http://localhost:8080',
      };

      expect(config.riderUrl).toBe('http://localhost:8013');
      expect(config.driverUrl).toBe('http://localhost:8016');
      expect(config.token).toBe('test-token');
      expect(config.stripeUrl).toBe('http://localhost:8080');
    });
  });

  describe('LogEntry Interface', () => {
    it('should create a valid LogEntry', () => {
      const logEntry: LogEntry = {
        time: '2024-01-01T00:00:00Z',
        level: 'info',
        message: 'Test message',
      };

      expect(logEntry.time).toBe('2024-01-01T00:00:00Z');
      expect(logEntry.level).toBe('info');
      expect(logEntry.message).toBe('Test message');
    });

    it('should support all log levels', () => {
      const levels: Array<LogEntry['level']> = ['info', 'success', 'error', 'warn', 'req'];

      levels.forEach(level => {
        const logEntry: LogEntry = {
          time: new Date().toISOString(),
          level,
          message: `Test ${level}`,
        };
        expect(logEntry.level).toBe(level);
      });
    });

    it('should create LogEntry with request and response details', () => {
      const logEntry: LogEntry = {
        time: new Date().toISOString(),
        level: 'req',
        message: 'API Request',
        request: {
          method: 'POST',
          url: '/api/test',
          body: { data: 'test' },
        },
        response: {
          status: 200,
          body: { result: 'success' },
        },
      };

      expect(logEntry.request).toBeDefined();
      expect(logEntry.request?.method).toBe('POST');
      expect(logEntry.response).toBeDefined();
      expect(logEntry.response?.status).toBe(200);
    });
  });
});
