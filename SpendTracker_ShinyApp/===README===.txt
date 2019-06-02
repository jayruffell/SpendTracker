INSTRUCTIONS
- based on three data sources: TSB bank statements, Countdown online order receipts, and manually-entered grocery receipts.
- all three data sources should be saved in the 'pastTransactionsData' folder inside the app's working directory.
- TSB bank statements should have four files, 2 loan statements, 1 revolving credit and 1 credit card. The former 3 should be saved using the 'csv with balance' option on the TSB website and the latter should be the 'csv' option. They should be saved with the word "TSB" in the filename (which is the default from TSB).
- Date ranges for TSB statements should overlap so that no transactions are missed. The app will de-dupe any duplicated transactions.
- Countdown online order receipts should be in the default pdf format provided by countdown, and should be saved with the word "orderinvoice" in the filename (which is the default from Countdown). Again, better to have the same receipt twice that to miss one because the app will de-dupe.
- Manually entered grocery receipt data should be added to the existing excel file 'manuallyEnteredGroceryReceipts'.
 

