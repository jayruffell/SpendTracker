
#__________________________________________________________________________________________________________________________________

# Use this file to load any data and scripts before calling ui or server - any objects available here should be available to ui and server
#__________________________________________________________________________________________________________________________________

#__________________________________________________________________________________________________________________________________

# Set params & load packages ----
#__________________________________________________________________________________________________________________________________

rm(list=ls())
downloadsPath <- 'C:/Users/user/Downloads/' # where statements get saved
dataPath <- 'C:/Users/user/Documents/JAMES/SpendTracker/pastTransactionsData/' # where downloaded trans data gets saved, and prev data gets read in from

suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
library(lubridate)
library(shiny)

#__________________________________________________________________________________________________________________________________

# Extract/read in data and classify transactions ----
#__________________________________________________________________________________________________________________________________

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

# Dedupe, after first finding any 'true' duplicates (i.e. actual transactions with same account, date, amount etc), as these will get removed by distinct() below. Vs 'False duplicates', which are just rows that are getting removed cos of overlapping dates between new and old data. True dupes will get printed to dash as a warning
trueDupes <- oldDF[duplicated(oldDF),]
if(exists("newDF")) trueDupes <- bind_rows(trueDupes, newDF[duplicated(newDF),])
trueDupes <- trueDupes %>%
  transmute(Date=Date, Amount=Amount, Description=Amount, Account=acc) %>% # prettifying for shiny, and adding month for reactive filtering
  mutate(month=format(as.Date(Date, format='%d/%m/%y'), '%b %Y'))
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
             
             # Mortgage & bank fees
             ifelse(grepl('SERVICE FEE|CARD REISSUE FE|ACCOUNT FEE', Description), 'BankFees',
                    ifelse(grepl('LOAN/EQUITY', Particulars), 'MortgagePrincipal',
                           ifelse(grepl('LOAN INTEREST', Description), 'MortgageInterest',
                                  
                                  # Everything else
                                  ifelse(grepl('COUNT ?DOWN|NEW ?WORLD|SAFFRON|EAT ?ME', Description), 'Groceries', 
                                         ifelse(grepl('BURGER|DOMINOS|WENDY|MCDONALDS|KEBAB', Description), 'FastFood',
                                                ifelse(grepl('CAFE|DEAR JERVOIS|SUSHI|BAKERY|BISTRO|RESTAURANT|SUGARGRILL|STARK|1929', Description), 'Cafes&EatingOut',
                                                       ifelse(grepl('Z TE AT|CAR ?PARK|VTNZ|BP|TRANSPORT', Description), 'Car',
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
                    ifelse(grepl('K ?-?MART|FARMERS|WAREHOUSE|TWL 187 ST LUKES', Description), 'KmartFarmersWarehouse',
                           ifelse(grepl('^MO ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ?$', Description), 'MoPayments',
                                  spendCategory)))))

#__________________________________________________________________________________________________________________________________

# Create ordered factor for month, so ggplot will plot correctly ----
#__________________________________________________________________________________________________________________________________

dd <- dd %>%
  mutate(month=format(Date, '%b %Y'))
monthOrder <- dd %>%
  arrange(Date) %>%
  distinct(month)
monthOrder <- monthOrder %>%
  mutate(myorder=1:nrow(.))
dd <- dd %>%
  left_join(monthOrder, by='month') %>%
  arrange(Date)
dd$month <- factor(dd$month, levels=unique(dd$month[order(dd$myorder)]), ordered=TRUE)

