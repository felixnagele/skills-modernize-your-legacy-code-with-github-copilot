/**
 * Account Management System - Node.js Implementation
 * Migrated from COBOL legacy application
 * 
 * This application preserves the original business logic:
 * - Initial balance: $1,000.00
 * - View balance functionality
 * - Credit account operations
 * - Debit account operations with insufficient funds protection
 * - Menu-driven interface
 * - Data persistence across operations
 */

import readline from 'readline';

// ============================================================================
// DATA LAYER (Equivalent to data.cob - DataProgram)
// ============================================================================

/**
 * DataProgram - Manages persistent storage of account balance
 * Equivalent to COBOL data.cob
 */
class DataProgram {
  constructor() {
    // STORAGE-BALANCE PIC 9(6)V99 VALUE 1000.00
    this.storageBalance = 1000.00;
  }

  /**
   * READ operation - Retrieves current balance
   * @returns {number} Current balance
   */
  read() {
    return this.storageBalance;
  }

  /**
   * WRITE operation - Updates stored balance
   * @param {number} balance - New balance to store
   */
  write(balance) {
    this.storageBalance = balance;
  }
}

// ============================================================================
// BUSINESS LOGIC LAYER (Equivalent to operations.cob - Operations)
// ============================================================================

/**
 * Operations - Implements core business logic for account operations
 * Equivalent to COBOL operations.cob
 */
class Operations {
  constructor(dataProgram) {
    this.dataProgram = dataProgram;
  }

  /**
   * TOTAL operation - View current balance
   * Equivalent to OPERATION-TYPE = 'TOTAL '
   */
  viewBalance() {
    const balance = this.dataProgram.read();
    console.log(`Current balance: ${this.formatCurrency(balance)}`);
  }

  /**
   * CREDIT operation - Add funds to account
   * Equivalent to OPERATION-TYPE = 'CREDIT'
   * @param {number} amount - Amount to credit
   */
  creditAccount(amount) {
    // Read current balance
    let finalBalance = this.dataProgram.read();
    
    // Add amount to balance
    finalBalance += amount;
    
    // Write updated balance
    this.dataProgram.write(finalBalance);
    
    console.log(`Amount credited. New balance: ${this.formatCurrency(finalBalance)}`);
  }

  /**
   * DEBIT operation - Remove funds from account
   * Equivalent to OPERATION-TYPE = 'DEBIT '
   * @param {number} amount - Amount to debit
   * @returns {boolean} True if successful, false if insufficient funds
   */
  debitAccount(amount) {
    // Read current balance
    let finalBalance = this.dataProgram.read();
    
    // Check for sufficient funds (CRITICAL BUSINESS RULE)
    if (finalBalance >= amount) {
      // Subtract amount from balance
      finalBalance -= amount;
      
      // Write updated balance
      this.dataProgram.write(finalBalance);
      
      console.log(`Amount debited. New balance: ${this.formatCurrency(finalBalance)}`);
      return true;
    } else {
      console.log("Insufficient funds for this debit.");
      return false;
    }
  }

  /**
   * Format number as currency with 2 decimal places
   * Maintains COBOL PIC 9(6)V99 precision
   * @param {number} amount - Amount to format
   * @returns {string} Formatted currency string
   */
  formatCurrency(amount) {
    return amount.toFixed(2);
  }

  /**
   * Validate and parse amount input
   * @param {string} input - User input
   * @returns {number|null} Parsed amount or null if invalid
   */
  parseAmount(input) {
    const amount = parseFloat(input);
    
    // Validate numeric input
    if (isNaN(amount)) {
      return null;
    }
    
    // Validate non-negative
    if (amount < 0) {
      return null;
    }
    
    // Validate maximum (PIC 9(6)V99 = max 999999.99)
    if (amount > 999999.99) {
      return null;
    }
    
    // Round to 2 decimal places for precision
    return Math.round(amount * 100) / 100;
  }
}

// ============================================================================
// UI LAYER (Equivalent to main.cob - MainProgram)
// ============================================================================

/**
 * MainProgram - Entry point and user interface
 * Equivalent to COBOL main.cob
 */
class MainProgram {
  constructor() {
    this.dataProgram = new DataProgram();
    this.operations = new Operations(this.dataProgram);
    this.continueFlag = true;
    
    // Create readline interface for user input
    this.rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout
    });
  }

  /**
   * Display main menu
   * Equivalent to MAIN-LOGIC DISPLAY statements
   */
  displayMenu() {
    console.log("--------------------------------");
    console.log("Account Management System");
    console.log("1. View Balance");
    console.log("2. Credit Account");
    console.log("3. Debit Account");
    console.log("4. Exit");
    console.log("--------------------------------");
  }

  /**
   * Prompt for amount input
   * @param {string} message - Prompt message
   * @returns {Promise<number|null>} Parsed amount or null
   */
  async promptAmount(message) {
    return new Promise((resolve) => {
      this.rl.question(message, (input) => {
        const amount = this.operations.parseAmount(input);
        if (amount === null) {
          console.log("Invalid amount. Please enter a valid positive number.");
        }
        resolve(amount);
      });
    });
  }

  /**
   * Handle credit account operation
   */
  async handleCredit() {
    let amount = null;
    while (amount === null) {
      amount = await this.promptAmount("Enter credit amount: ");
    }
    this.operations.creditAccount(amount);
  }

  /**
   * Handle debit account operation
   */
  async handleDebit() {
    let amount = null;
    while (amount === null) {
      amount = await this.promptAmount("Enter debit amount: ");
    }
    this.operations.debitAccount(amount);
  }

  /**
   * Process user choice
   * Equivalent to EVALUATE USER-CHOICE
   * @param {string} choice - User's menu selection
   */
  async processChoice(choice) {
    const userChoice = parseInt(choice);

    switch (userChoice) {
      case 1:
        // WHEN 1 - View Balance
        this.operations.viewBalance();
        break;
        
      case 2:
        // WHEN 2 - Credit Account
        await this.handleCredit();
        break;
        
      case 3:
        // WHEN 3 - Debit Account
        await this.handleDebit();
        break;
        
      case 4:
        // WHEN 4 - Exit
        this.continueFlag = false;
        break;
        
      default:
        // WHEN OTHER - Invalid choice
        console.log("Invalid choice, please select 1-4.");
        break;
    }
  }

  /**
   * Main program loop
   * Equivalent to PERFORM UNTIL CONTINUE-FLAG = 'NO'
   */
  async run() {
    console.log("\n=== Account Management System Started ===\n");

    while (this.continueFlag) {
      this.displayMenu();
      
      await new Promise((resolve) => {
        this.rl.question("Enter your choice (1-4): ", async (choice) => {
          await this.processChoice(choice);
          resolve();
        });
      });
      
      // Add blank line for readability
      console.log();
    }

    // Exit message
    console.log("Exiting the program. Goodbye!");
    
    // Close readline interface
    this.rl.close();
  }
}

// ============================================================================
// APPLICATION ENTRY POINT
// ============================================================================

/**
 * Start the application
 * Equivalent to COBOL STOP RUN
 */
async function main() {
  const app = new MainProgram();
  await app.run();
  process.exit(0);
}

// Export classes for testing
export { DataProgram, Operations, MainProgram };

// Run the application only if this file is executed directly
// This prevents the app from running when imported for testing
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch((error) => {
    console.error("Application error:", error);
    process.exit(1);
  });
}
