# Account Management System - Test Suite

## Overview

This test suite provides comprehensive unit and integration testing for the Node.js Account Management System, directly mirroring the 60 test cases documented in [../../docs/TESTPLAN.md](../../docs/TESTPLAN.md).

## Test Framework

- **Framework:** Jest 29.7.0
- **Test Type:** Unit and Integration Tests
- **Total Test Cases:** 70+ (covering all 60 test plan scenarios plus additional validation tests)
- **Coverage Target:** 100% of business logic

## Installation

The test framework is already configured. To install dependencies:

```bash
cd src/accounting
npm install
```

## Running Tests

### Run All Tests

```bash
npm test
```

### Run Tests in Watch Mode

```bash
npm run test:watch
```

### Run Tests with Coverage Report

```bash
npm run test:coverage
```

## Test Structure

The test suite is organized into 10 main categories, matching the COBOL test plan:

### 1. Initial Setup and Balance Tests
- **TC-001:** Verify initial account balance ($1,000.00)
- **TC-002:** Verify balance precision (2 decimal places)

### 2. View Balance Functionality Tests
- **TC-003 to TC-006:** View balance operations and persistence

### 3. Credit Account Operation Tests
- **TC-007 to TC-014:** Credit operations including edge cases and boundaries

### 4. Debit Account Operation Tests (Sufficient Funds)
- **TC-015 to TC-021:** Debit operations with valid amounts

### 5. Insufficient Funds Validation Tests
- **TC-022 to TC-027:** Critical business rule - preventing overdrafts

### 6. Mixed Operations Tests
- **TC-028 to TC-033:** Combined credit/debit sequences

### 7. Menu Navigation and User Input Tests
- **TC-034 to TC-040:** User interface and input validation

### 8. Data Type and Boundary Tests
- **TC-041 to TC-048:** Precision, limits, and format validation

### 9. Data Persistence and State Management Tests
- **TC-049 to TC-052:** Data integrity across operations

### 10. Edge Cases and Error Scenarios
- **TC-053 to TC-060:** Error handling and edge cases

### Additional Validation Tests
- Input parsing validation
- Currency formatting validation
- Boundary value testing

## Test Coverage by Component

### DataProgram Class (Data Layer)
✅ Initial balance initialization  
✅ Read operations  
✅ Write operations  
✅ State persistence  

### Operations Class (Business Logic Layer)
✅ View balance functionality  
✅ Credit account operations  
✅ Debit account operations  
✅ Insufficient funds validation (critical)  
✅ Amount parsing and validation  
✅ Currency formatting  
✅ Decimal precision handling  

### MainProgram Class (UI Layer)
✅ Menu choice processing  
✅ Invalid input handling  
✅ Continue flag management  
✅ Exit functionality  

## Critical Business Rules Validated

The test suite validates all critical business rules from the COBOL system:

1. ✅ **Initial Balance:** Exactly $1,000.00
2. ✅ **Insufficient Funds Protection:** Debit rejected if amount > balance
3. ✅ **No Negative Balances:** Balance never goes below $0.00
4. ✅ **Decimal Precision:** All amounts maintain 2 decimal places
5. ✅ **Data Persistence:** Balance changes persist across operations
6. ✅ **Atomic Operations:** Operations complete fully or not at all
7. ✅ **Maximum Balance:** $999,999.99 limit enforced

## Example Test Output

```
PASS  ./index.test.js
  1. Initial Setup and Balance Tests
    ✓ TC-001: Verify initial account balance (3 ms)
    ✓ TC-002: Verify account balance data type precision (1 ms)
  2. View Balance Functionality Tests
    ✓ TC-003: View balance on first access (2 ms)
    ✓ TC-004: View balance after credit operation (1 ms)
    ✓ TC-005: View balance after debit operation (1 ms)
    ✓ TC-006: View balance multiple times in sequence (1 ms)
  3. Credit Account Operation Tests
    ✓ TC-007: Credit account with valid amount (1 ms)
    ✓ TC-008: Credit account with small amount (1 ms)
    ...

Test Suites: 1 passed, 1 total
Tests:       70 passed, 70 total
```

## Test-Driven Development Workflow

1. **Run tests before changes:**
   ```bash
   npm test
   ```

2. **Make code changes**

3. **Run tests again to verify:**
   ```bash
   npm test
   ```

4. **Check coverage:**
   ```bash
   npm run test:coverage
   ```

## Continuous Integration

These tests can be integrated into CI/CD pipelines:

```yaml
# Example GitHub Actions workflow
- name: Run Tests
  run: |
    cd src/accounting
    npm install
    npm test
```

## Test Maintenance

When adding new features:

1. Add corresponding test case(s) to `index.test.js`
2. Update test plan in `docs/TESTPLAN.md`
3. Ensure all tests pass before merging
4. Maintain >90% code coverage

## Comparing with COBOL

Each test case is tagged with its corresponding COBOL test plan ID (TC-001 through TC-060), making it easy to:

- Verify parity between COBOL and Node.js implementations
- Cross-reference test results with business stakeholders
- Document migration completeness

## Troubleshooting

### Tests Not Running

If you see "Error: Cannot use import statement outside a module":
- Ensure `"type": "module"` is in package.json
- Run tests with the correct command: `npm test`

### Mock Issues

If console mocks aren't clearing:
- Each test suite uses `beforeEach` and `afterEach` hooks
- Mocks are restored after each test

### Coverage Reports

Coverage reports are generated in the `coverage/` directory:
- Open `coverage/lcov-report/index.html` in a browser for detailed coverage view

## Future Enhancements

Planned test improvements:
- [ ] Integration tests with actual database
- [ ] API endpoint tests (when REST API is added)
- [ ] Performance/load testing
- [ ] E2E tests with UI automation
- [ ] Mutation testing
- [ ] Snapshot testing for outputs

## Contributing

When adding new tests:
1. Follow the existing naming convention (TC-XXX)
2. Include descriptive test names
3. Add comments explaining complex scenarios
4. Group related tests in describe blocks
5. Ensure tests are isolated and idempotent

---

**Test Suite Version:** 1.0  
**Last Updated:** February 22, 2026  
**Maintained By:** Development Team  
**Based On:** COBOL Test Plan v1.0
