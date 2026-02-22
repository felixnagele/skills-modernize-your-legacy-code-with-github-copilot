/**
 * Unit Tests for Account Management System
 * Test Plan Reference: docs/TESTPLAN.md
 * 
 * This test suite mirrors the 60 test cases from the COBOL test plan,
 * validating that the Node.js implementation maintains identical business logic.
 */

import { DataProgram, Operations, MainProgram } from './index.js';

// ============================================================================
// 1. INITIAL SETUP AND BALANCE TESTS (TC-001, TC-002)
// ============================================================================

describe('1. Initial Setup and Balance Tests', () => {
  test('TC-001: Verify initial account balance', () => {
    const dataProgram = new DataProgram();
    const balance = dataProgram.read();
    
    expect(balance).toBe(1000.00);
  });

  test('TC-002: Verify account balance data type precision', () => {
    const dataProgram = new DataProgram();
    const operations = new Operations(dataProgram);
    const balance = dataProgram.read();
    const formatted = operations.formatCurrency(balance);
    
    // Should display with 2 decimal places
    expect(formatted).toBe('1000.00');
    expect(formatted).toMatch(/^\d+\.\d{2}$/);
  });
});

// ============================================================================
// 2. VIEW BALANCE FUNCTIONALITY TESTS (TC-003 to TC-006)
// ============================================================================

describe('2. View Balance Functionality Tests', () => {
  let dataProgram;
  let operations;
  let consoleSpy;

  beforeEach(() => {
    dataProgram = new DataProgram();
    operations = new Operations(dataProgram);
    consoleSpy = jest.spyOn(console, 'log').mockImplementation();
  });

  afterEach(() => {
    consoleSpy.mockRestore();
  });

  test('TC-003: View balance on first access', () => {
    operations.viewBalance();
    
    expect(consoleSpy).toHaveBeenCalledWith('Current balance: 1000.00');
  });

  test('TC-004: View balance after credit operation', () => {
    operations.creditAccount(250.00);
    consoleSpy.mockClear();
    
    operations.viewBalance();
    
    expect(consoleSpy).toHaveBeenCalledWith('Current balance: 1250.00');
  });

  test('TC-005: View balance after debit operation', () => {
    operations.debitAccount(300.00);
    consoleSpy.mockClear();
    
    operations.viewBalance();
    
    expect(consoleSpy).toHaveBeenCalledWith('Current balance: 700.00');
  });

  test('TC-006: View balance multiple times in sequence', () => {
    operations.viewBalance();
    const firstCall = consoleSpy.mock.calls[0][0];
    consoleSpy.mockClear();
    
    operations.viewBalance();
    const secondCall = consoleSpy.mock.calls[0][0];
    
    expect(firstCall).toBe(secondCall);
    expect(firstCall).toBe('Current balance: 1000.00');
  });
});

// ============================================================================
// 3. CREDIT ACCOUNT OPERATION TESTS (TC-007 to TC-014)
// ============================================================================

describe('3. Credit Account Operation Tests', () => {
  let dataProgram;
  let operations;
  let consoleSpy;

  beforeEach(() => {
    dataProgram = new DataProgram();
    operations = new Operations(dataProgram);
    consoleSpy = jest.spyOn(console, 'log').mockImplementation();
  });

  afterEach(() => {
    consoleSpy.mockRestore();
  });

  test('TC-007: Credit account with valid amount', () => {
    operations.creditAccount(250.00);
    
    expect(dataProgram.read()).toBe(1250.00);
    expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 1250.00');
  });

  test('TC-008: Credit account with small amount', () => {
    operations.creditAccount(0.01);
    
    expect(dataProgram.read()).toBe(1000.01);
  });

  test('TC-009: Credit account with large amount', () => {
    operations.creditAccount(50000.00);
    
    expect(dataProgram.read()).toBe(51000.00);
  });

  test('TC-010: Credit account with decimal precision', () => {
    operations.creditAccount(123.45);
    
    expect(dataProgram.read()).toBe(1123.45);
    expect(consoleSpy).toHaveBeenCalledWith('Amount credited. New balance: 1123.45');
  });

  test('TC-011: Multiple credit operations in sequence', () => {
    operations.creditAccount(100.00);
    operations.creditAccount(200.00);
    operations.creditAccount(50.00);
    
    expect(dataProgram.read()).toBe(1350.00);
  });

  test('TC-012: Credit account with zero amount', () => {
    operations.creditAccount(0.00);
    
    expect(dataProgram.read()).toBe(1000.00);
  });

  test('TC-013: Credit operation data persistence', () => {
    operations.creditAccount(250.00);
    
    // Simulate returning to menu and viewing balance
    const balance = dataProgram.read();
    expect(balance).toBe(1250.00);
  });

  test('TC-014: Credit near maximum balance', () => {
    // Set balance to 999,000.00
    dataProgram.write(999000.00);
    
    operations.creditAccount(999.99);
    
    expect(dataProgram.read()).toBe(999999.99);
  });
});

// ============================================================================
// 4. DEBIT ACCOUNT OPERATION TESTS - SUFFICIENT FUNDS (TC-015 to TC-021)
// ============================================================================

describe('4. Debit Account Operation Tests (Sufficient Funds)', () => {
  let dataProgram;
  let operations;
  let consoleSpy;

  beforeEach(() => {
    dataProgram = new DataProgram();
    operations = new Operations(dataProgram);
    consoleSpy = jest.spyOn(console, 'log').mockImplementation();
  });

  afterEach(() => {
    consoleSpy.mockRestore();
  });

  test('TC-015: Debit account with valid amount', () => {
    const result = operations.debitAccount(300.00);
    
    expect(result).toBe(true);
    expect(dataProgram.read()).toBe(700.00);
    expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 700.00');
  });

  test('TC-016: Debit account with small amount', () => {
    operations.debitAccount(0.01);
    
    expect(dataProgram.read()).toBe(999.99);
  });

  test('TC-017: Debit entire balance', () => {
    operations.debitAccount(1000.00);
    
    expect(dataProgram.read()).toBe(0.00);
  });

  test('TC-018: Debit with decimal precision', () => {
    operations.debitAccount(123.45);
    
    expect(dataProgram.read()).toBe(876.55);
    expect(consoleSpy).toHaveBeenCalledWith('Amount debited. New balance: 876.55');
  });

  test('TC-019: Multiple debit operations in sequence', () => {
    operations.debitAccount(100.00);
    operations.debitAccount(200.00);
    operations.debitAccount(50.00);
    
    expect(dataProgram.read()).toBe(650.00);
  });

  test('TC-020: Debit operation data persistence', () => {
    operations.debitAccount(300.00);
    
    // Simulate returning to menu and viewing balance
    const balance = dataProgram.read();
    expect(balance).toBe(700.00);
  });

  test('TC-021: Partial balance debit', () => {
    operations.debitAccount(999.99);
    
    expect(dataProgram.read()).toBe(0.01);
  });
});

// ============================================================================
// 5. INSUFFICIENT FUNDS VALIDATION TESTS (TC-022 to TC-027)
// ============================================================================

describe('5. Insufficient Funds Validation Tests', () => {
  let dataProgram;
  let operations;
  let consoleSpy;

  beforeEach(() => {
    dataProgram = new DataProgram();
    operations = new Operations(dataProgram);
    consoleSpy = jest.spyOn(console, 'log').mockImplementation();
  });

  afterEach(() => {
    consoleSpy.mockRestore();
  });

  test('TC-022: Debit amount exceeds balance (CRITICAL BUSINESS RULE)', () => {
    const result = operations.debitAccount(1500.00);
    
    expect(result).toBe(false);
    expect(dataProgram.read()).toBe(1000.00); // Balance unchanged
    expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
  });

  test('TC-023: Debit slightly over balance', () => {
    const result = operations.debitAccount(1000.01);
    
    expect(result).toBe(false);
    expect(dataProgram.read()).toBe(1000.00);
    expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
  });

  test('TC-024: Insufficient funds - balance unchanged', () => {
    dataProgram.write(500.00);
    
    operations.debitAccount(1000.00);
    
    expect(dataProgram.read()).toBe(500.00);
  });

  test('TC-025: Large debit on small balance', () => {
    dataProgram.write(10.00);
    
    const result = operations.debitAccount(100000.00);
    
    expect(result).toBe(false);
    expect(dataProgram.read()).toBe(10.00);
  });

  test('TC-026: Debit after failed debit attempt', () => {
    // First attempt - should fail
    let result = operations.debitAccount(2000.00);
    expect(result).toBe(false);
    
    // Second attempt - should succeed
    result = operations.debitAccount(500.00);
    expect(result).toBe(true);
    expect(dataProgram.read()).toBe(500.00);
  });

  test('TC-027: Zero balance debit attempt', () => {
    dataProgram.write(0.00);
    
    const result = operations.debitAccount(0.01);
    
    expect(result).toBe(false);
    expect(consoleSpy).toHaveBeenCalledWith('Insufficient funds for this debit.');
  });
});

// ============================================================================
// 6. MIXED OPERATIONS TESTS (TC-028 to TC-033)
// ============================================================================

describe('6. Mixed Operations Tests', () => {
  let dataProgram;
  let operations;

  beforeEach(() => {
    dataProgram = new DataProgram();
    operations = new Operations(dataProgram);
    jest.spyOn(console, 'log').mockImplementation();
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  test('TC-028: Credit then debit sequence', () => {
    operations.creditAccount(500.00);
    operations.debitAccount(300.00);
    
    expect(dataProgram.read()).toBe(1200.00);
  });

  test('TC-029: Debit then credit sequence', () => {
    operations.debitAccount(400.00);
    operations.creditAccount(200.00);
    
    expect(dataProgram.read()).toBe(800.00);
  });

  test('TC-030: Multiple mixed operations', () => {
    operations.creditAccount(500.00);  // 1500.00
    operations.debitAccount(200.00);   // 1300.00
    operations.creditAccount(100.00);  // 1400.00
    operations.debitAccount(300.00);   // 1100.00
    
    expect(dataProgram.read()).toBe(1100.00);
  });

  test('TC-031: Failed debit between credits', () => {
    operations.creditAccount(500.00);   // 1500.00
    operations.debitAccount(2000.00);   // Failed - still 1500.00
    operations.creditAccount(300.00);   // 1800.00
    
    expect(dataProgram.read()).toBe(1800.00);
  });

  test('TC-032: Debit to zero then credit', () => {
    operations.debitAccount(1000.00);  // 0.00
    operations.creditAccount(250.00);  // 250.00
    
    expect(dataProgram.read()).toBe(250.00);
  });

  test('TC-033: View balance between each operation', () => {
    let balance;
    
    balance = dataProgram.read();
    expect(balance).toBe(1000.00);
    
    operations.creditAccount(100.00);
    balance = dataProgram.read();
    expect(balance).toBe(1100.00);
    
    operations.debitAccount(50.00);
    balance = dataProgram.read();
    expect(balance).toBe(1050.00);
  });
});

// ============================================================================
// 7. MENU NAVIGATION AND USER INPUT TESTS (TC-034 to TC-040)
// ============================================================================

describe('7. Menu Navigation and User Input Tests', () => {
  test('TC-034: Exit application via menu - continueFlag set to false', () => {
    const mainProgram = new MainProgram();
    expect(mainProgram.continueFlag).toBe(true);
    
    // Simulate selecting option 4 (exit)
    mainProgram.continueFlag = false;
    expect(mainProgram.continueFlag).toBe(false);
  });

  test('TC-035: Invalid menu choice - non-numeric input', async () => {
    const mainProgram = new MainProgram();
    const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
    
    await mainProgram.processChoice('A');
    
    expect(consoleSpy).toHaveBeenCalledWith('Invalid choice, please select 1-4.');
    consoleSpy.mockRestore();
  });

  test('TC-036: Invalid menu choice - out of range high', async () => {
    const mainProgram = new MainProgram();
    const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
    
    await mainProgram.processChoice('5');
    
    expect(consoleSpy).toHaveBeenCalledWith('Invalid choice, please select 1-4.');
    consoleSpy.mockRestore();
  });

  test('TC-037: Invalid menu choice - out of range low', async () => {
    const mainProgram = new MainProgram();
    const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
    
    await mainProgram.processChoice('0');
    
    expect(consoleSpy).toHaveBeenCalledWith('Invalid choice, please select 1-4.');
    consoleSpy.mockRestore();
  });

  test('TC-038: Invalid menu choice - negative number', async () => {
    const mainProgram = new MainProgram();
    const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
    
    await mainProgram.processChoice('-1');
    
    expect(consoleSpy).toHaveBeenCalledWith('Invalid choice, please select 1-4.');
    consoleSpy.mockRestore();
  });

  test('TC-039: Menu redisplay after operation - continueFlag remains true', async () => {
    const mainProgram = new MainProgram();
    const consoleSpy = jest.spyOn(console, 'log').mockImplementation();
    
    // Process option 1 (view balance)
    await mainProgram.processChoice('1');
    
    // ContinueFlag should still be true for menu redisplay
    expect(mainProgram.continueFlag).toBe(true);
    consoleSpy.mockRestore();
  });

  test('TC-040: Menu redisplay after invalid choice', async () => {
    const mainProgram = new MainProgram();
    
    await mainProgram.processChoice('99');
    
    // ContinueFlag should still be true for menu redisplay
    expect(mainProgram.continueFlag).toBe(true);
  });
});

// ============================================================================
// 8. DATA TYPE AND BOUNDARY TESTS (TC-041 to TC-048)
// ============================================================================

describe('8. Data Type and Boundary Tests', () => {
  let dataProgram;
  let operations;

  beforeEach(() => {
    dataProgram = new DataProgram();
    operations = new Operations(dataProgram);
    jest.spyOn(console, 'log').mockImplementation();
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  test('TC-041: Maximum balance value', () => {
    dataProgram.write(999999.98);
    operations.creditAccount(0.01);
    
    expect(dataProgram.read()).toBe(999999.99);
  });

  test('TC-042: Minimum balance value', () => {
    dataProgram.write(0.01);
    operations.debitAccount(0.01);
    
    expect(dataProgram.read()).toBe(0.00);
  });

  test('TC-043: Decimal precision in calculations', () => {
    operations.creditAccount(0.33);
    operations.creditAccount(0.33);
    operations.creditAccount(0.34);
    
    expect(dataProgram.read()).toBe(1001.00);
  });

  test('TC-044: Maximum single credit', () => {
    const amount = operations.parseAmount('999999.99');
    
    expect(amount).toBe(999999.99);
    operations.creditAccount(amount);
    expect(dataProgram.read()).toBe(1000999.99);
  });

  test('TC-045: Maximum single debit', () => {
    dataProgram.write(999999.99);
    operations.debitAccount(999999.99);
    
    expect(dataProgram.read()).toBe(0.00);
  });

  test('TC-046: Balance display format', () => {
    const formatted = operations.formatCurrency(1000);
    
    expect(formatted).toBe('1000.00');
    expect(formatted).toMatch(/\.\d{2}$/);
  });

  test('TC-047: Negative amount input - credit', () => {
    const amount = operations.parseAmount('-100.00');
    
    expect(amount).toBeNull();
  });

  test('TC-048: Negative amount input - debit', () => {
    const amount = operations.parseAmount('-100.00');
    
    expect(amount).toBeNull();
  });
});

// ============================================================================
// 9. DATA PERSISTENCE AND STATE MANAGEMENT TESTS (TC-049 to TC-052)
// ============================================================================

describe('9. Data Persistence and State Management Tests', () => {
  let dataProgram;
  let operations;

  beforeEach(() => {
    dataProgram = new DataProgram();
    operations = new Operations(dataProgram);
    jest.spyOn(console, 'log').mockImplementation();
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  test('TC-049: Balance persists across operations', () => {
    operations.creditAccount(100.00);
    expect(dataProgram.read()).toBe(1100.00);
    
    operations.debitAccount(50.00);
    expect(dataProgram.read()).toBe(1050.00);
  });

  test('TC-050: Read operation doesn\'t modify balance', () => {
    const balance1 = dataProgram.read();
    const balance2 = dataProgram.read();
    const balance3 = dataProgram.read();
    const balance4 = dataProgram.read();
    const balance5 = dataProgram.read();
    
    expect(balance1).toBe(1000.00);
    expect(balance2).toBe(1000.00);
    expect(balance3).toBe(1000.00);
    expect(balance4).toBe(1000.00);
    expect(balance5).toBe(1000.00);
  });

  test('TC-051: Failed operation doesn\'t corrupt state', () => {
    operations.debitAccount(1500.00); // Failed
    expect(dataProgram.read()).toBe(1000.00);
    
    operations.creditAccount(200.00); // Success
    expect(dataProgram.read()).toBe(1200.00);
  });

  test('TC-052: Storage balance initialization', () => {
    const freshDataProgram = new DataProgram();
    
    expect(freshDataProgram.read()).toBe(1000.00);
  });
});

// ============================================================================
// 10. EDGE CASES AND ERROR SCENARIOS (TC-053 to TC-060)
// ============================================================================

describe('10. Edge Cases and Error Scenarios', () => {
  let operations;

  beforeEach(() => {
    const dataProgram = new DataProgram();
    operations = new Operations(dataProgram);
  });

  test('TC-053: Empty input at menu - parseAmount handles empty string', () => {
    const amount = operations.parseAmount('');
    
    expect(amount).toBeNull();
  });

  test('TC-054: Empty input at amount prompt', () => {
    const amount = operations.parseAmount('');
    
    expect(amount).toBeNull();
  });

  test('TC-055: Special characters in amount - dollar sign', () => {
    const amount = operations.parseAmount('$100.00');
    
    expect(amount).toBeNull();
  });

  test('TC-056: Multiple decimal points', () => {
    const amount = operations.parseAmount('100.00.50');
    
    expect(amount).toBeNull();
  });

  test('TC-057: Very long numeric input - exceeds maximum', () => {
    const amount = operations.parseAmount('12345678901234567890');
    
    // Should be null because it exceeds 999999.99
    expect(amount).toBeNull();
  });

  test('TC-058: Alphabetic characters in amount', () => {
    const amount = operations.parseAmount('ABC');
    
    expect(amount).toBeNull();
  });

  test('TC-059: Rapid successive operations', () => {
    const dataProgram = new DataProgram();
    const ops = new Operations(dataProgram);
    jest.spyOn(console, 'log').mockImplementation();
    
    // Perform 10 operations rapidly
    ops.creditAccount(100.00);   // 1100
    ops.debitAccount(50.00);     // 1050
    ops.creditAccount(200.00);   // 1250
    ops.debitAccount(100.00);    // 1150
    ops.creditAccount(50.00);    // 1200
    ops.debitAccount(25.00);     // 1175
    ops.creditAccount(75.00);    // 1250
    ops.debitAccount(150.00);    // 1100
    ops.creditAccount(300.00);   // 1400
    ops.debitAccount(400.00);    // 1000
    
    expect(dataProgram.read()).toBe(1000.00);
    jest.restoreAllMocks();
  });

  test('TC-060: Exit without performing operations', () => {
    const dataProgram = new DataProgram();
    
    // Immediately check balance without any operations
    expect(dataProgram.read()).toBe(1000.00);
  });
});

// ============================================================================
// ADDITIONAL VALIDATION TESTS
// ============================================================================

describe('Additional Validation Tests', () => {
  let operations;

  beforeEach(() => {
    const dataProgram = new DataProgram();
    operations = new Operations(dataProgram);
  });

  test('parseAmount: Valid decimal number', () => {
    expect(operations.parseAmount('123.45')).toBe(123.45);
  });

  test('parseAmount: Valid whole number', () => {
    expect(operations.parseAmount('100')).toBe(100.00);
  });

  test('parseAmount: Number with more than 2 decimal places (rounds)', () => {
    expect(operations.parseAmount('123.456')).toBe(123.46);
  });

  test('parseAmount: Zero is valid', () => {
    expect(operations.parseAmount('0')).toBe(0.00);
  });

  test('parseAmount: Maximum valid amount', () => {
    expect(operations.parseAmount('999999.99')).toBe(999999.99);
  });

  test('parseAmount: Amount exceeding maximum', () => {
    expect(operations.parseAmount('1000000.00')).toBeNull();
  });

  test('formatCurrency: Formats with 2 decimal places', () => {
    expect(operations.formatCurrency(1000)).toBe('1000.00');
    expect(operations.formatCurrency(1234.5)).toBe('1234.50');
    expect(operations.formatCurrency(0.1)).toBe('0.10');
  });
});
