# Account Management System - Node.js Implementation

## Overview

This is a modernized Node.js implementation of the legacy COBOL Account Management System. The application maintains all original business logic, data integrity, and menu options from the COBOL version.

## Migration Details

**Source:** Three COBOL files (main.cob, operations.cob, data.cob)  
**Target:** Single Node.js file (index.js)  
**Architecture:** Preserved three-layer architecture from COBOL

### Architecture Mapping

```
COBOL                    →  Node.js
────────────────────────────────────────────────
data.cob (DataProgram)   →  DataProgram class
operations.cob (Operations) →  Operations class
main.cob (MainProgram)   →  MainProgram class
```

## Business Logic Preserved

✅ **Initial Balance:** $1,000.00  
✅ **View Balance:** Display current account balance  
✅ **Credit Account:** Add funds to account  
✅ **Debit Account:** Remove funds with validation  
✅ **Insufficient Funds Protection:** Prevent overdrafts  
✅ **Decimal Precision:** 2 decimal places (cents)  
✅ **Data Persistence:** Balance maintained across operations  
✅ **Menu-Driven Interface:** Same 4 options as COBOL

## Prerequisites

- Node.js >= 18.0.0
- npm (comes with Node.js)

## Installation

```bash
cd src/accounting
npm install
```

## Running the Application

### Option 1: Command Line

```bash
cd src/accounting
npm start
```

### Option 2: Development Mode (with auto-reload)

```bash
cd src/accounting
npm run dev
```

### Option 3: VS Code Debugger

1. Open VS Code
2. Press `F5` or go to Run → Start Debugging
3. Select "Launch Account Management System" from the dropdown

## Usage

When you run the application, you'll see a menu:

```
--------------------------------
Account Management System
1. View Balance
2. Credit Account
3. Debit Account
4. Exit
--------------------------------
Enter your choice (1-4):
```

### Menu Options

1. **View Balance** - Display current account balance
2. **Credit Account** - Add money to the account (you'll be prompted for amount)
3. **Debit Account** - Withdraw money from the account (you'll be prompted for amount)
4. **Exit** - Close the application

### Example Session

```
Enter your choice (1-4): 1
Current balance: 1000.00

Enter your choice (1-4): 2
Enter credit amount: 250.50
Amount credited. New balance: 1250.50

Enter your choice (1-4): 3
Enter debit amount: 100.25
Amount debited. New balance: 1150.25

Enter your choice (1-4): 3
Enter debit amount: 2000.00
Insufficient funds for this debit.

Enter your choice (1-4): 4
Exiting the program. Goodbye!
```

## Code Structure

```javascript
// Data Layer - Manages persistence
class DataProgram {
  read()        // Get current balance
  write(balance) // Update balance
}

// Business Logic Layer - Implements operations
class Operations {
  viewBalance()              // Display balance
  creditAccount(amount)      // Add funds
  debitAccount(amount)       // Remove funds (with validation)
  formatCurrency(amount)     // Format as currency
  parseAmount(input)         // Validate and parse input
}

// UI Layer - User interface
class MainProgram {
  displayMenu()              // Show menu options
  processChoice(choice)      // Handle user selection
  run()                      // Main program loop
}
```

## Validation Rules

- **Amount Format:** Must be a valid positive number
- **Decimal Places:** Automatically rounded to 2 decimal places
- **Maximum Amount:** $999,999.99 (matching COBOL PIC 9(6)V99)
- **Minimum Amount:** $0.01
- **Insufficient Funds:** Debit rejected if amount > balance
- **Invalid Input:** User re-prompted for valid input

## Comparison with COBOL

| Feature | COBOL | Node.js |
|---------|-------|---------|
| Language | COBOL | JavaScript (ES6+) |
| Files | 3 separate files | 1 file with 3 classes |
| Module System | CALL/USING | Class methods |
| I/O | ACCEPT/DISPLAY | readline interface |
| Data Types | PIC 9(6)V99 | Number with validation |
| Control Flow | PERFORM UNTIL | while loop + async/await |
| Initial Balance | 1000.00 | 1000.00 ✓ |
| Business Logic | Preserved | Identical ✓ |

## Testing

Refer to [../../docs/TESTPLAN.md](../../docs/TESTPLAN.md) for comprehensive test cases covering all business logic.

## Future Enhancements

Potential improvements for the Node.js version:
- Add unit tests (Jest/Mocha)
- Add integration tests
- Implement REST API
- Add database persistence (PostgreSQL, MongoDB)
- Add transaction history/logging
- Support multiple accounts
- Add authentication
- Create web UI (React/Vue)

## License

ISC

---

*Migrated from COBOL legacy system - February 22, 2026*
