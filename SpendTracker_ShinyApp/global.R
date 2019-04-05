
#__________________________________________________________________________________________________________________________________

# Use this file to load any data and scripts before calling ui or server - any objects available here should be available to ui and server
#__________________________________________________________________________________________________________________________________

#__________________________________________________________________________________________________________________________________

# Set params & load packages ----
#__________________________________________________________________________________________________________________________________

# Specify data path - where downloaded trans data gets saved, and prev data gets read in from. NOTE SHINY NEEDS RELATIVE PATHS FROM THE APP WORKING DIR IF GETTING PUBLISHED TO SERVER.  
# dataPath <- 'C:/Users/user/Documents/JAMES/SpendTracker/SpendTracker_ShinyApp/pastTransactionsData/' # local version
dataPath <- 'pastTransactionsData/' # server version

suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
library(lubridate)
library(shiny)

#__________________________________________________________________________________________________________________________________

# Extract/read in data and classify transactions ----
#__________________________________________________________________________________________________________________________________

#+++++++++++++++
# Read in all transactions and dedupe
#+++++++++++++++

oldtsbfiles <- list.files(dataPath)[grepl('[Tt][Ss][Bb]', list.files(dataPath))] # calling 'old' cos I used to read new files in from different locn, but Shiny didn't like this.
oldList <- lapply(oldtsbfiles, function(x) {
  trans <- read.csv(paste0(dataPath, x), stringsAsFactors = FALSE)
  if(grepl('4548670363823106', x)) trans <- mutate(trans, acc='visa')
  if(grepl('70015100640', x)) trans <- mutate(trans, acc='mortgage1')
  if(grepl('70015100641', x)) trans <- mutate(trans, acc='mortgage2')
  if(grepl('70015100647', x)) trans <- mutate(trans, acc='revolving')
  trans$filename <- gsub(".csv", "", x)
  return(trans)
})
oldDF <- bind_rows(oldList)
oldDF <- as.tbl(oldDF)

# Set date properly
oldDF$Date <- as.Date(oldDF$Date, format='%d/%m/%y')

# Dedupe, after first finding any 'true' duplicates (i.e. actual transactions with same account, date, amount etc), as these will get removed by distinct() below. Vs 'False duplicates', which are just rows that are getting removed cos of overlapping dates between new and old data. True dupes will get printed to dash as a warning
trueDupes <- oldDF[duplicated(oldDF),]
trueDupes <- trueDupes %>%
  select(Date, Amount, Description, acc) %>%
  rename(Account=acc) %>% # prettifying for shiny, and adding month for reactive filtering
  mutate(month=format(Date, '%b %Y'),
         Date=as.character(Date))
oldDF <- select(oldDF, -filename) # filename was used to find 'true dupes' rather than just overlapping dateranges for different downloads from the same account.
dd <- distinct(oldDF)

#+++++++++++++++
# Data formatting/cleaning
#+++++++++++++++

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

# Remove all dates prior to Jan 2019, cos we weren't getting full data (pay and/or spend going into Westpac still)
dd <- dd %>%
  filter(Date>'2018-12-31')

#__________________________________________________________________________________________________________________________________

# Classify transactions ----
#__________________________________________________________________________________________________________________________________

dd$Description <- toupper(dd$Description)
dd$Particulars <- toupper(dd$Particulars)

dd <- dd %>%
  mutate(
    spendCategory=
      # Pay
      ifelse((grepl('ANNALECT|50119ACC|MINISTRY OF|TAX FAM31', Description) & Amount > 0) |
               (grepl('RUFFELL', Description) & grepl('ACC', Description) & Amount > 0), 'Pay&GovnContributions',

             # Mortgage & bank fees
             ifelse(grepl('SERVICE FEE|CARD REISSUE FE|ACCOUNT FEE|INTEREST CHARGED', Description), 'BankFees',
                    ifelse(grepl('LOAN/EQUITY', Particulars), 'MortgagePrincipal',
                           ifelse(grepl('LOAN INTEREST', Description), 'MortgageInterest',

                                  # Everything else
                                  ifelse(grepl('FARRO|COUNT ?DOWN|PAK ?N ?SAVE|NEW ?WORLD|SAFFRON|EAT ?ME', Description), 'Groceries',
                                                ifelse(grepl('SAAN|CAFE|DEAR JERVOIS|SUSHI|BAKERY|BISTRO|RESTAURANT|SUGARGRILL|STARK|1929|GOOD ?HOME|BEER ?BREW|DELI|MR ?ILLINGSWORTH|LIQUOR|KREEM', Description), 'CafesAlcohol&EatingOut',
                                                       ifelse(grepl('Z TE AT|HEEM|UBER|CAR ?PARK|VTNZ|BP|TRANSPORT|CYCLES|TOURNAMENT|AT HOP', Description), 'Transport',
                                                              ifelse(grepl('BABY|MOCKA|H ?& ?M|BAND ?OF ?BOYS|KID ?REPUBLIC|THE ?SLEEP ?STORE|ALYCE|G4U ?DOLLAR ?STORE|COTTON ?ON|WHITCOULLS', Description), 'Baby',
                                                                     ifelse(grepl('MITRE|HAMMER|KINGS|CITTA|FREEDOM FURNITURE|HOMESTEAD PICTURE|SPOTLIGHT|STORAGE ?BOX|CARPET ?CLEAN|KODAK|REFUSE ?STATION', Description), 'Home&Garden',
                                                                            ifelse(grepl('WATERCARE|SLINGSHOT|SKINNY|AKL COUNCIL|MERIDIAN', Description), 'Utilities',
                                                                                   ifelse(grepl('PHARMACY|HEALTH NEW LYNN|PROACTIVE|ASTERON|PHYSIO', Description), 'Health',
                                                                                          ifelse(grepl('POP-UP ?GLOBE|NETFLIX|MOVIES|CINEMA', Description), 'Entertainment',
                                                                                                 'Other')))))))))))))
dd <- dd %>%
  mutate(
    spendCategory=
      ifelse(grepl('RODNEY ?WAYNE|CACI|HUE|ZARA|MOOCHI|SISTERS AND CO|KATHRYN ?WILSON|STITCHES|HAIRDRESS|KSUBI|KATIE ?AND ?LINA ?NAILS|MECCA|BRAS ?N ?THINGS|SASS & BIDE', Description) |
               (grepl('SUPERETTE', Description) & Amount < -50), # separates superette store from dairies.
             'EmilyClothes&Beauty',
             ifelse(grepl('BURGER|DOMINOS|WENDY|MCDONALDS|KEBAB|SUPERETTE', Description), 'FastFood',
                    ifelse(grepl('K ?-?MART|FARMERS|WAREHOUSE|TWL 187 ST LUKES', Description), 'KmartFarmersWarehouse',
                           ifelse(grepl('^MO ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ?$', Description), 'MoPayments',
                                  ifelse(grepl('CHARITY|FLOWERS|ELTON|S A F E', Description), 'Gifts&Charity',
                                         spendCategory))))))
    
# For classfying new transactions
dd %>%
  # Only look at latest month, if prev months done already
  mutate(month=format(Date, '%b %Y')) %>%
  filter(month=='Mar 2019') %>%
  filter(spendCategory=='Other') %>%
  # Split 'other' into known and unknown, so Im only classifying the latter
  mutate(
    spendCategory=
      ifelse(grepl('APPLE NZ|GOOGLE ?STORAGE|SURF2SURF|NZEI', Description), 'Other_known', 'Other')) %>%
  filter(spendCategory!='Other_known') %>%
  arrange(desc(month), Amount) %>%
  select(-Balance, -spendCategory) %>%
  as.data.frame()

names(dd)
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
