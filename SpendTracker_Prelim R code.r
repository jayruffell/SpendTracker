
# Improvements
# - by default don't show mortgage, but add this in as a checkbox in shiny (include principal and loan)
# - show first and last date of all transactions?
# - show projected spend for current month based on days left
# - For any category, drill down into all transactions for a given month (default 'current month')
# - spends by day over time for given month (default current month)

#__________________________________________________________________________________________________________________________________

# Once we have CSVs of all spend from TSB, use this code to track spends vs deposits over time, and hoopefully classify into spend categories. Basically replicate westpac cashnav. Then put in a dashboard if I can be bothered.
#__________________________________________________________________________________________________________________________________

#+++++++++++++++
# Set params & load packages
#+++++++++++++++

rm(list=ls())

downloadsPath <- 'C:/Users/user/Downloads/' # where statements get saved
projectPath <- 'C:/Users/user/Documents/JAMES/SpendTracker/' # where code lives
dataPath <- paste0(projectPath, 'pastTransactionsData/') # where transactions will get saved - in case TSB can't give us e.g. more than 3mo

suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
library(lubridate)

#__________________________________________________________________________________________________________________________________

# Extract/read in data ----
#__________________________________________________________________________________________________________________________________

# Need to save four files to C:\Users\user\Downloads (TSB should automatically save to this locn):
# - 1 Visa
# - 1 liberty revolving
# - 2 x liberty table (i.e. mortgage)
# - Save them for as big a date range as you like, just make sure it overlaps with last time. Code below will remove duplicate entries
# - Save them in 'CSV including balance' format on website, but note can't do this for VISA.
# - DELETE TSB FILES FROM DOWNLOADS ONCE CODE HAS RUN (actually not necessary, R removes de-dupes)

#+++++++++++++++
# Read data in from downloads folder if it exists
#+++++++++++++++

tsbfiles <- list.files(downloadsPath)[grepl('[Tt][Ss][Bb]', list.files(downloadsPath))]

if(length(tsbfiles)>0){
  
  # Read in new downloaded data, inc. automatically saving to dataPath
  newList <- lapply(tsbfiles, function(x) {
    trans <- read.csv(paste0(downloadsPath, x), stringsAsFactors = FALSE)
    write.csv(trans, paste0(dataPath, x), row.names=FALSE)
    if(grepl('4548670363823106', x)) trans <- mutate(trans, acc='visa')
    if(grepl('70015100640', x)) trans <- mutate(trans, acc='mortgage1')
    if(grepl('70015100641', x)) trans <- mutate(trans, acc='mortgage2')
    if(grepl('70015100647', x)) trans <- mutate(trans, acc='revolving')
    
    # Give warning if there are any 'true' duplicates (i.e. actual transactions with same account, date, amount etc), as these will get removed by distinct() below.
    trueDupes <- trans[duplicated(trans),]
    if(nrow(trueDupes)>0){
      cat('\nThe following rows are true duplicates (i.e. actual transactions with same account, date, amount etc within each raw data file). \nThese will get removed by distinct() below! \nIn future add them back in if numbers are significant:\n')
      print(trueDupes)
    } 
    return(trans)
  })
  newDF <- bind_rows(newList)
  newDF <- as.tbl(newDF)
}

#+++++++++++++++
# Read in historic data, bind to new data, and dedupe
#+++++++++++++++

oldtsbfiles <- list.files(dataPath)[grepl('[Tt][Ss][Bb]', list.files(dataPath))]

oldList <- lapply(oldtsbfiles, function(x) {
  trans <- read.csv(paste0(dataPath, x), stringsAsFactors = FALSE)
  if(grepl('4548670363823106', x)) trans <- mutate(trans, acc='visa')
  if(grepl('70015100640', x)) trans <- mutate(trans, acc='mortgage1')
  if(grepl('70015100641', x)) trans <- mutate(trans, acc='mortgage2')
  if(grepl('70015100647', x)) trans <- mutate(trans, acc='revolving')
  
  # Give warning if there are any 'true' duplicates (i.e. actual transactions with same account, date, amount etc), as these will get removed by distinct() below.
  trueDupes <- trans[duplicated(trans),]
  if(nrow(trueDupes)>0){
    cat('\nThe following rows are true duplicates (i.e. actual transactions with same account, date, amount etc within each raw data file). \nThese will get removed by distinct() below! \nIn future add them back in if numbers are significant:\n')
    print(trueDupes)
  }
  return(trans)
})
oldDF <- bind_rows(oldList)
oldDF <- as.tbl(oldDF)

# Bind to new
if(exists("newDF")){ 
  dd <- bind_rows(newDF, oldDF)
} else {
  dd <- oldDF
}

# Dedupe
dd <- distinct(dd)

#+++++++++++++++
# Data formatting/cleaning
#+++++++++++++++

# Set date properly
dd$Date <- as.Date(dd$Date, format='%d/%m/%y')

# Exclude initial massive deposits/transfers from Westpac following loan & revolving credit restructuring - will massively skew earnings etc
dd <- dd %>%
  filter(round(Amount,0)!=-315000) %>%
  filter(round(Amount,0)!=-42000) %>% # initial loan drawdowns (2x 315K)
  filter(round(Amount,0)!=24536) %>%
  filter(round(Amount,0)!=-24536) %>%
  filter(round(Amount,0)!=14541) %>%
  filter(round(Amount,0)!=-14541) %>% # transfers from revolving into loan accounts (show up in both) during loan restructure 
  filter(round(Amount,0)!=5300) %>% # reward for switching to TSB
  filter(round(Amount,0)!=10000) %>% 
  filter(round(Amount,0)!=27339) %>% # transfers of savings from Westpac
  filter(round(Amount,0)!=28737) %>% # not sure what this one is! Extra $ from loan restructure, deposited from lawyer I believe.
  filter(round(Amount,0)!=28737) # not sure what this one is! Extra $ from loan restructure, deposited from lawyer I believe.

# Exclude payments to Visa account - cos these will show up as both a -ve out of revolving and a +ve into visa. Just want anything bought out of visa to show up once, in visa acc.
dd <- dd %>%
  filter(acc!='visa' | !grepl('PAYMENT RECEIVED', Description)) %>%
  filter(acc!='revolving' | !grepl('credit card', Particulars))

# Also exclude mortgage repayments that show up as a +ve into mortgage accounts, for same reason as above
dd <- dd %>%
  filter(!grepl('mortgage', acc) | !grepl('Repayment', Particulars))

#__________________________________________________________________________________________________________________________________

# Classify transactions ----
#__________________________________________________________________________________________________________________________________

dd$Description <- toupper(dd$Description)
dd$Particulars <- toupper(dd$Particulars)
dd <- dd %>%
  mutate(
    spendCategory=
      # Pay
      ifelse((grepl('ANNALECT|50119ACC|MINISTRY OF|TAX FAM31', Description) & Amount > 0), 'Pay&GovnContributions', 
             
             # Mortgage and bank fees
             ifelse(grepl('SERVICE FEE|CARD REISSUE FE|ACCOUNT FEE', Description), 'BankFees',
                    ifelse(grepl('LOAN/EQUITY', Particulars), 'MortgagePrincipal',
                           ifelse(grepl('LOAN INTEREST', Description), 'MortgageInterest',
                                  
                                  # Everything else
                                  ifelse(grepl('COUNT ?DOWN|NEW ?WORLD|SAFFRON|EAT ?ME', Description), 'Groceries', 
                                         ifelse(grepl('BURGER|DOMINOS|WENDY|MCDONALDS|KEBAB', Description), 'FastFood',
                                                ifelse(grepl('CAFE|DEAR JERVOIS|SUSHI|BAKERY|BISTRO|RESTAURANT|SUGARGRILL|STARK|1929', Description), 'Cafes&EatingOut',
                                                       ifelse(grepl('Z TE AT|CAR ?PARK|VTNZ|BP |NZ ?TRANSPORT', Description), 'Car',
                                                              ifelse(grepl('BABY|MOCKA', Description), 'Baby',
                                                                     ifelse(grepl('MITRE|HAMMER|KINGS|CITTA|FREEDOM FURNITURE|HOMESTEAD PICTURE|SPOTLIGHT|STORAGE ?BOX', Description), 'Home&Garden',
                                                                            ifelse(grepl('WATERCARE|SLINGSHOT|SKINNY|AKL COUNCIL', Description), 'Utilities',
                                                                                   ifelse(grepl('PHARMACY|HEALTH NEW LYNN|PROACTIVE|ASTERON', Description), 'Health',
                                                                                          ifelse(grepl('NETFLIX|MOVIES|CINEMA', Description), 'Entertainment',
                                                                                                 'Other'))))))))))))))
dd <- dd %>%
  mutate(
    spendCategory=
      ifelse(grepl('RODNEY ?WAYNE|HUE|ZARA|MOOCHI|SISTERS AND CO|KATHRYN ?WILSON|STITCHES|HAIRDRESS|KSUBI|THE ?SLEEP ?STORE', Description) |
               (grepl('SUPERETTE', Description) & Amount < -50), # separates superette store from dairies.
             'EmilyClothes&Beauty',
             ifelse(grepl('S A F E', Description), 'Charity',
                    ifelse(grepl('K? -?MART|FARMERS|WAREHOUSE|TWL 187 ST LUKES', Description), 'KmartFarmersWarehouse',
                           ifelse(grepl('^MO ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ?$', Description), 'MoPayments',
                    spendCategory)))))

#+++++++++++++++
# Record information on unclassified transactions
#+++++++++++++++

# Look for other big transactions to exclude
unclassifiedBigSpends <- dd %>%
  filter(spendCategory=='Other' & Amount < -50) %>%
  select(-Balance, -spendCategory, -Reference) %>%
  arrange(Amount)

cat('Unclassified spends over $50:\n')
print(unclassifiedBigSpends)

unclassifiedBigDeposits <- dd %>%
  filter(spendCategory=='Other'  & Amount > 50) %>%
  select(-Balance, -spendCategory, -Reference) %>%
  arrange(desc(Amount))

cat('Unclassified deposits over $50:\n')
print(unclassifiedBigDeposits)

# Record all unclassified deposits and spends and print info on total value of unclassified transactions
unclassifiedSpends <- dd %>%
  filter(spendCategory=='Other' & Amount < 0) %>%
  arrange(desc(Amount)) %>%
  as.data.frame()
unclassifiedDeposits <- dd %>%
  filter(spendCategory=='Other' & Amount > 0) %>%
  arrange(desc(Amount))
cat(nrow(unclassifiedSpends), 'withdrawals constituting', round(sum(unclassifiedSpends$Amount), 0), 'dollars unaccounted for\n')
cat(nrow(unclassifiedDeposits), 'deposits constituting', round(sum(unclassifiedDeposits$Amount), 0), 'dollars unaccounted for\n')

#__________________________________________________________________________________________________________________________________

# Create outputs ----
#__________________________________________________________________________________________________________________________________

#+++++++++++++++
# First need to create ordered factor for month, so ggplot will plot correctly
#+++++++++++++++

dd <- dd %>%
  mutate(month=format(Date, '%b %y'))
monthOrder <- dd %>%
  arrange(Date) %>%
  distinct(month)
monthOrder <- monthOrder %>%
  mutate(myorder=1:nrow(.))
dd <- dd %>%
  left_join(monthOrder, by='month') %>%
  arrange(Date)
dd$month <- factor(dd$month, levels=unique(dd$month[order(dd$myorder)]), ordered=TRUE)

#+++++++++++++++
# Total balance (deposits minus withdrawals)
#+++++++++++++++

dd %>%
  group_by(month) %>%
  summarise(Amount=round(sum(Amount), 0)) %>%
  mutate(posneg=ifelse(Amount>0, 'pos', 'neg')) %>%
  ggplot(aes(month, Amount, fill=posneg)) + geom_bar(stat='identity') + 
  geom_text(aes(label=Amount)) + 
  xlab("") + ylab("Monthly balance ($)") + 
  theme(legend.position="none") + ggtitle('Overall balance per month')

#+++++++++++++++
# Spends by category for selected month, spends over time, and transaction list, with options to:
# - include or exclude mortgage
# - show only a specific category
#+++++++++++++++

# Filter down based on dashboard inputs - NB filtering for chosenCategory happens further down cos always want to show everything in below plot
chosenMonth <- 'Feb 19' # 'All Months'
if(chosenMonth!='All Months') plotdf <- filter(dd, month==chosenMonth) else(plotdf <- dd)
excludeMortgage <- 'Yes' # 'Yes' or 'No'
if(excludeMortgage=='Yes') plotdf <- filter(plotdf, !grepl('Mortgage', spendCategory)) else(plotdf <- dd)

# For remaining plots exclude pay and any unclassified transactions that show up as 'Other', then make withdrawals +ve
plotdf <- plotdf %>%
  filter(Amount<0) %>%
  mutate(Amount=-1*round(Amount, 0))

# Spends by category
plotdf %>%
  group_by(spendCategory) %>%
  summarise(Amount=sum(Amount)) %>%
  # Order spend category so shows better order in plots
  mutate(spendCategory=factor(spendCategory, levels=unique(spendCategory[order(Amount)]))) %>%
  ggplot(aes(spendCategory, Amount, fill=spendCategory)) + geom_bar(stat='identity') + 
  geom_text(aes(label=Amount)) + 
  xlab("") + ylab("Category spend ($)") + 
  theme(legend.position="none") + ggtitle(paste('Spend by category:', chosenMonth)) +
  coord_flip()

# Filter category for next two plots based on dash inputs
chosenCategory <- 'Home&Garden' # 'AllCategories'
if(chosenCategory!='AllCategories') plotdf <- filter(plotdf, spendCategory==chosenCategory)

# Spends over time
plotdf2 <- plotdf %>% 
  group_by(Date) %>%
  summarise(Amount=sum(Amount))

# plot line plot if <1 obs, otherwise scatterplot 
if(nrow(plotdf2)<2){ 
  ggplot(plotdf2, aes(Date, Amount)) + geom_point(colour="#00BFC4") + ylab('Amount ($)')
} else {
  ggplot(plotdf2, aes(Date, Amount)) + geom_line(colour="#00BFC4")  + ylab('Amount ($)')
}
  
# Show transaction list
plotdf %>%
  select(Date, Amount, Description, acc, spendCategory) %>%
  as.data.frame()

#+++++++++++++++
# Look at final balances (ex visa)
#+++++++++++++++

dd %>%
  filter(acc!='visa') %>%
  group_by(acc) %>%
  arrange(Date) %>%
  filter(row_number()==n()) %>%
  select(acc, Balance, Date)

cat('IN FUTURE EXCLUDE EVERYTHING BEFORE FEB 2019 (?) - OR WHENEVER PAY AND ALL SPENDS WERE COMING OUT OF TSB\n')
gc()
