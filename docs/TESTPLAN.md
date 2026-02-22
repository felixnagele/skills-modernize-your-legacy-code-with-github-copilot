# Test Plan - Account Management System

## Overview

This test plan covers all business logic and functionality of the COBOL Account Management System. It is designed to validate the current implementation with business stakeholders and will be used as a baseline for creating unit and integration tests during the migration to Node.js.

**Application Under Test:** COBOL Account Management System  
**Version:** Legacy COBOL Implementation  
**Test Plan Version:** 1.0  
**Date:** February 22, 2026  
**Prepared For:** Node.js Migration Project

---

## Test Scope

- Initial account setup and balance
- View balance functionality
- Credit account operations
- Debit account operations
- Insufficient funds validation
- Menu navigation and user input validation
- Data persistence across operations
- Edge cases and boundary conditions

---

## Test Cases

### 1. Initial Setup and Balance Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-001 | Verify initial account balance | Application freshly started | 1. Start application<br/>2. Select option 1 (View Balance) | Display shows "Current balance: 1000.00" | | | Initial balance must be exactly $1,000.00 |
| TC-002 | Verify account balance data type precision | Application running | 1. Start application<br/>2. View balance | Balance displays with 2 decimal places (cents precision) | | | Critical for currency handling |

---

### 2. View Balance Functionality Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-003 | View balance on first access | Fresh application start | 1. Start application<br/>2. Select option 1 | System displays current balance without errors | | | Tests READ operation from data layer |
| TC-004 | View balance after credit operation | Account has been credited | 1. Credit account with $250.00<br/>2. Select option 1 (View Balance) | Display shows updated balance (1250.00) | | | Validates data persistence |
| TC-005 | View balance after debit operation | Account has been debited | 1. Debit account with $300.00<br/>2. Select option 1 (View Balance) | Display shows updated balance (700.00) | | | Validates debit persistence |
| TC-006 | View balance multiple times in sequence | Application running | 1. Select option 1<br/>2. Return to menu<br/>3. Select option 1 again | Same balance displayed both times (no unintended changes) | | | Tests data stability |

---

### 3. Credit Account Operation Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-007 | Credit account with valid amount | Initial balance: $1,000.00 | 1. Select option 2 (Credit Account)<br/>2. Enter amount: 250.00<br/>3. Confirm | System displays "Amount credited. New balance: 1250.00" | | | Standard credit operation |
| TC-008 | Credit account with small amount | Current balance: $1,000.00 | 1. Select option 2<br/>2. Enter amount: 0.01 | Balance increases to $1,000.01 | | | Tests minimum credit amount |
| TC-009 | Credit account with large amount | Current balance: $1,000.00 | 1. Select option 2<br/>2. Enter amount: 50000.00 | Balance increases to $51,000.00 | | | Tests large transaction handling |
| TC-010 | Credit account with decimal precision | Current balance: $1,000.00 | 1. Select option 2<br/>2. Enter amount: 123.45 | Balance shows $1,123.45 with exact precision | | | Tests decimal handling |
| TC-011 | Multiple credit operations in sequence | Initial balance: $1,000.00 | 1. Credit $100.00<br/>2. Credit $200.00<br/>3. Credit $50.00<br/>4. View balance | Final balance shows $1,350.00 | | | Tests cumulative credits |
| TC-012 | Credit account with zero amount | Current balance: $1,000.00 | 1. Select option 2<br/>2. Enter amount: 0.00 | Balance remains $1,000.00 (or appropriate handling) | | | Edge case: zero credit |
| TC-013 | Credit operation data persistence | Balance after credit: $1,250.00 | 1. Credit account<br/>2. Return to menu<br/>3. View balance | Balance correctly persisted and retrievable | | | Tests WRITE operation |
| TC-014 | Credit near maximum balance | Current balance: $999,000.00 | 1. Select option 2<br/>2. Enter amount: 999.99 | Balance updates to $999,999.99 or handles max limit | | | Tests maximum balance boundary |

---

### 4. Debit Account Operation Tests (Sufficient Funds)

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-015 | Debit account with valid amount | Current balance: $1,000.00 | 1. Select option 3 (Debit Account)<br/>2. Enter amount: 300.00 | System displays "Amount debited. New balance: 700.00" | | | Standard debit operation |
| TC-016 | Debit account with small amount | Current balance: $1,000.00 | 1. Select option 3<br/>2. Enter amount: 0.01 | Balance decreases to $999.99 | | | Tests minimum debit amount |
| TC-017 | Debit entire balance | Current balance: $1,000.00 | 1. Select option 3<br/>2. Enter amount: 1000.00 | Balance becomes $0.00 | | | Tests complete withdrawal |
| TC-018 | Debit with decimal precision | Current balance: $1,000.00 | 1. Select option 3<br/>2. Enter amount: 123.45 | Balance shows $876.55 with exact precision | | | Tests decimal calculation |
| TC-019 | Multiple debit operations in sequence | Current balance: $1,000.00 | 1. Debit $100.00<br/>2. Debit $200.00<br/>3. Debit $50.00<br/>4. View balance | Final balance shows $650.00 | | | Tests cumulative debits |
| TC-020 | Debit operation data persistence | Balance after debit: $700.00 | 1. Debit account<br/>2. Return to menu<br/>3. View balance | Balance correctly persisted and retrievable | | | Tests data layer WRITE |
| TC-021 | Partial balance debit | Current balance: $1,000.00 | 1. Select option 3<br/>2. Enter amount: 999.99 | Balance becomes $0.01 | | | Tests near-zero balance |

---

### 5. Insufficient Funds Validation Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-022 | Debit amount exceeds balance | Current balance: $1,000.00 | 1. Select option 3<br/>2. Enter amount: 1500.00 | System displays "Insufficient funds for this debit."<br/>Balance remains $1,000.00 | | | **Critical business rule** |
| TC-023 | Debit slightly over balance | Current balance: $1,000.00 | 1. Select option 3<br/>2. Enter amount: 1000.01 | Error message displayed<br/>Balance remains $1,000.00 | | | Tests precision in validation |
| TC-024 | Insufficient funds - balance unchanged | Current balance: $500.00 | 1. Attempt debit of $1,000.00<br/>2. View balance after rejection | Balance still shows $500.00 (no change) | | | Validates no partial debit |
| TC-025 | Large debit on small balance | Current balance: $10.00 | 1. Select option 3<br/>2. Enter amount: 100000.00 | Insufficient funds message<br/>Balance remains $10.00 | | | Tests extreme difference |
| TC-026 | Debit after failed debit attempt | Current balance: $1,000.00 | 1. Attempt debit of $2,000.00 (fails)<br/>2. Debit $500.00 (valid) | First debit rejected, second succeeds<br/>Final balance: $500.00 | | | Tests recovery after failure |
| TC-027 | Zero balance debit attempt | Current balance: $0.00 | 1. Select option 3<br/>2. Enter amount: 0.01 | Insufficient funds message displayed | | | Tests zero balance validation |

---

### 6. Mixed Operations Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-028 | Credit then debit sequence | Initial balance: $1,000.00 | 1. Credit $500.00<br/>2. Debit $300.00<br/>3. View balance | Final balance: $1,200.00 | | | Tests operation sequence |
| TC-029 | Debit then credit sequence | Initial balance: $1,000.00 | 1. Debit $400.00<br/>2. Credit $200.00<br/>3. View balance | Final balance: $800.00 | | | Reverse order test |
| TC-030 | Multiple mixed operations | Initial balance: $1,000.00 | 1. Credit $500.00<br/>2. Debit $200.00<br/>3. Credit $100.00<br/>4. Debit $300.00<br/>5. View balance | Final balance: $1,100.00 | | | Complex transaction sequence |
| TC-031 | Failed debit between credits | Initial balance: $1,000.00 | 1. Credit $500.00<br/>2. Debit $2,000.00 (fails)<br/>3. Credit $300.00<br/>4. View balance | Final balance: $1,800.00 | | | Tests failure doesn't corrupt data |
| TC-032 | Debit to zero then credit | Initial balance: $1,000.00 | 1. Debit $1,000.00<br/>2. Credit $250.00<br/>3. View balance | Final balance: $250.00 | | | Tests operations from zero |
| TC-033 | View balance between each operation | Initial balance: $1,000.00 | 1. View balance<br/>2. Credit $100<br/>3. View balance<br/>4. Debit $50<br/>5. View balance | Each view shows correct progressive balance | | | Tests data integrity tracking |

---

### 7. Menu Navigation and User Input Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-034 | Exit application via menu | Application running | 1. Select option 4 (Exit) | System displays "Exiting the program. Goodbye!" and terminates | | | Normal exit flow |
| TC-035 | Invalid menu choice - letters | Application at main menu | 1. Enter non-numeric input (e.g., 'A') | System displays "Invalid choice, please select 1-4."<br/>Returns to menu | | | Input validation test |
| TC-036 | Invalid menu choice - out of range high | Application at main menu | 1. Enter choice: 5 | Error message displayed<br/>Menu redisplays | | | Boundary test (upper) |
| TC-037 | Invalid menu choice - out of range low | Application at main menu | 1. Enter choice: 0 | Error message displayed<br/>Menu redisplays | | | Boundary test (lower) |
| TC-038 | Invalid menu choice - negative number | Application at main menu | 1. Enter choice: -1 | Appropriate error handling | | | Negative input test |
| TC-039 | Menu redisplay after operation | Any operation completed | 1. Complete any operation<br/>2. Return to menu | Menu redisplays with all 4 options | | | Tests menu loop |
| TC-040 | Menu redisplay after invalid choice | Invalid input entered | 1. Enter invalid choice<br/>2. View menu | Menu fully redisplays without corruption | | | Error recovery test |

---

### 8. Data Type and Boundary Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-041 | Maximum balance value | Current balance: $999,999.98 | 1. Credit $0.01 | Balance becomes $999,999.99 or appropriate max handling | | | Tests PIC 9(6)V99 limit |
| TC-042 | Minimum balance value | Current balance: $0.01 | 1. Debit $0.01 | Balance becomes $0.00 | | | Tests minimum boundary |
| TC-043 | Decimal precision in calculations | Current balance: $1,000.00 | 1. Credit $0.33<br/>2. Credit $0.33<br/>3. Credit $0.34<br/>4. View balance | Balance shows $1,001.00 (proper rounding) | | | Tests decimal arithmetic |
| TC-044 | Maximum single credit | Current balance: $1,000.00 | 1. Credit $999,999.99 | Appropriate handling (success or overflow prevention) | | | Boundary test |
| TC-045 | Maximum single debit | Current balance: $999,999.99 | 1. Debit $999,999.99 | Balance becomes $0.00 | | | Maximum debit test |
| TC-046 | Balance display format | Any current balance | 1. View balance | Balance displays with 2 decimal places (e.g., 1000.00, not 1000) | | | Format validation |
| TC-047 | Negative amount input - credit | Application at credit prompt | 1. Select option 2<br/>2. Enter amount: -100.00 | Appropriate error handling or rejection | | | Negative input validation |
| TC-048 | Negative amount input - debit | Application at debit prompt | 1. Select option 3<br/>2. Enter amount: -100.00 | Appropriate error handling or rejection | | | Negative input validation |

---

### 9. Data Persistence and State Management Tests

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-049 | Balance persists across operations | Initial balance: $1,000.00 | 1. Credit $100<br/>2. Return to menu<br/>3. View balance<br/>4. Debit $50<br/>5. View balance | Balance correctly maintained: $1,100 then $1,050 | | | Core persistence test |
| TC-050 | Read operation doesn't modify balance | Current balance: $1,000.00 | 1. View balance 5 times | Balance remains $1,000.00 after all views | | | READ operation safety |
| TC-051 | Failed operation doesn't corrupt state | Current balance: $1,000.00 | 1. Attempt invalid debit<br/>2. View balance<br/>3. Perform valid credit<br/>4. View balance | Balance remains correct throughout | | | State integrity test |
| TC-052 | Storage balance initialization | Fresh application start | 1. View balance immediately | Shows initial value of $1,000.00 | | | Tests STORAGE-BALANCE initialization |

---

### 10. Edge Cases and Error Scenarios

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|--------------|----------------------|----------------|------------|-----------------|---------------|--------|----------|
| TC-053 | Empty input at menu | Application at main menu | 1. Press Enter without input | Appropriate handling (error or re-prompt) | | | Empty input test |
| TC-054 | Empty input at amount prompt | At credit/debit amount prompt | 1. Select credit/debit<br/>2. Press Enter without amount | Appropriate handling or validation error | | | Amount validation |
| TC-055 | Special characters in amount | At amount prompt | 1. Enter amount: $100.00 (with dollar sign) | Appropriate error handling | | | Format validation |
| TC-056 | Multiple decimal points | At amount prompt | 1. Enter amount: 100.00.50 | Error handling or proper parsing | | | Malformed input test |
| TC-057 | Very long numeric input | At amount prompt | 1. Enter 20-digit number | Appropriate truncation or error | | | Input length validation |
| TC-058 | Alphabetic characters in amount | At amount prompt | 1. Enter amount: ABC | Error message and re-prompt | | | Non-numeric validation |
| TC-059 | Rapid successive operations | Application running | 1. Perform 10 operations rapidly without viewing balance<br/>2. View final balance | All operations correctly applied | | | Stress test |
| TC-060 | Exit without performing operations | Fresh start | 1. Start application<br/>2. Immediately select option 4 | Clean exit, balance unchanged | | | Immediate exit test |

---

## Test Execution Notes

### Testing Environment
- **Compiler:** GnuCOBOL (cobc)
- **Execution:** Native compiled binary
- **Platform:** Linux/Unix environment

### Testing Prerequisites
1. GnuCOBOL compiler installed and accessible
2. All three COBOL source files present:
   - `src/cobol/main.cob`
   - `src/cobol/operations.cob`
   - `src/cobol/data.cob`
3. Application compiled successfully: `cobc -x src/cobol/main.cob src/cobol/operations.cob src/cobol/data.cob -o accountsystem`

### Test Data Requirements
- Initial balance: Always $1,000.00
- Test amounts: Range from $0.01 to $999,999.99
- Invalid inputs: Non-numeric, negative, out of range

### Critical Business Rules to Validate
1. **Initial Balance:** Must be exactly $1,000.00
2. **Insufficient Funds:** Debit must be rejected if amount > balance
3. **No Negative Balance:** Balance can never go below $0.00
4. **Decimal Precision:** All amounts must maintain 2 decimal places
5. **Data Persistence:** Balance changes must persist across operations
6. **Atomic Operations:** Operations must complete fully or not at all

---

## Node.js Migration Testing Notes

### Mapping to Node.js Tests

**Unit Tests** (should cover):
- Balance calculation functions (add, subtract)
- Validation functions (sufficient funds check)
- Input validation and sanitization
- Data type and format validations

**Integration Tests** (should cover):
- Complete operation flows (credit, debit, view)
- Data persistence layer operations
- API endpoint testing (if REST API is created)
- Error handling and edge cases

**End-to-End Tests** (should cover):
- Complete user workflows
- Multi-operation sequences
- State management across operations
- Menu navigation flows (if UI is created)

### Test Automation Recommendations
- Use testing frameworks: Jest, Mocha, or similar
- Implement parameterized tests for boundary values
- Create test fixtures for initial state setup
- Mock data layer for unit tests
- Use actual database for integration tests
- Implement continuous testing in CI/CD pipeline

### Data Migration Validation
Run parallel testing during migration:
1. Execute same test case on COBOL system
2. Execute same test case on Node.js system
3. Compare results for consistency
4. Document any intentional behavior changes

---

## Sign-off

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Test Plan Author | | | |
| Business Stakeholder | | | |
| Development Lead | | | |
| QA Lead | | | |

---

*Test Plan Version: 1.0*  
*Last Updated: February 22, 2026*  
*Total Test Cases: 60*
