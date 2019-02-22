#__________________________________________________________________________________________________________________________________

# Once we have CSVs of all spend from TSB, use this code to track spends vs deposits over time, and hoopefully classify into spend categories. Basically replicate westpac cashnav. Then put in a dashboard if I can be bothered.
#__________________________________________________________________________________________________________________________________

#+++++++++++++++
# Set params & load packages
#+++++++++++++++

rm(list=ls())

downloadsPath <- 'C:/Users/user/Downloads/' # where statements get saved
projectPath <- 'C:/Users/user/Documents/JAMES/Spend tracker/' # where code lives
dataPath <- paste0(projectPath, '/pastTransactions/') # where transactions will get saved - in case TSB can't give us e.g. more than 3mo

suppressMessages(library(dplyr))
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
# - DELETE TSB FILES FROM DOWNLOADS ONCE CODE HAS RUN

answer <- winDialog('yesno', 'Is new data getting updated from downloads folder?') # can turn to a checkbox in shiny

if(answer=='YES'){

  #+++++++++++++++
  # Read data in from downloads folder
  #+++++++++++++++
  
  # First check all files are in downloads
  tsbfiles <- list.files(downloadsPath)[grepl('[Tt][Ss][Bb]', list.files(downloadsPath))]
  if((any(grepl('4548670363823106', tsbfiles)) & any(grepl('70015100640',  tsbfiles))  & any(grepl('70015100641',  tsbfiles))  & any(grepl('70015100647',  tsbfiles)))!=TRUE){
    stop('Havent downloaded all TSB files to downloads folder.')
  }
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
if(answer=='YES'){ 
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

#__________________________________________________________________________________________________________________________________

# Classify transactions ----
#__________________________________________________________________________________________________________________________________

dd$Description <- toupper(dd$Description)
dd$Particulars <- toupper(dd$Particulars)
dd <- dd %>%
  mutate(
    spendCategory=
             ifelse(grepl('ANNALECT|50119ACC', Description), 'Pay', 
             ifelse(grepl('LOAN INTEREST|SERVICE FEE|CARD REISSUE FE|ACCOUNT FEE', Description) |
                      grepl('LOAN/EQUITY', Particulars), 'MortgageAndBankFees', 
                    ifelse(grepl('COUNT ?DOWN|NEW ?WORLD|SAFFRON|EAT ?ME', Description), 'Groceries', 
                           ifelse(grepl('BURGER|DOMINOS|WENDY|MCDONALDS|KEBAB', Description), 'FastFood',
                                  ifelse(grepl('CAFE|DEAR JERVOIS|SUSHI|BAKERY|BISTRO|RESTAURANT|SUGARGRILL|STARK|1929', Description), 'CafesAndEatingOut',
                                         ifelse(grepl('Z TE AT|CAR ?PARK|VTNZ|BP ', Description), 'Car',
                                                ifelse(grepl('BABY|MOCKA', Description), 'Baby',
                                                       ifelse(grepl('MITRE|HAMMER|KINGS|CITTA|FREEDOM FURNITURE|HOMESTEAD PICTURE|SPOTLIGHT|STORAGE ?BOX', Description), 'Home&Garden',
                                                              ifelse(grepl('WATERCARE|SLINGSHOT|SKINNY|AKL COUNCIL', Description), 'Utilities',
                                                                     ifelse(grepl('PHARMACY|HEALTH NEW LYNN|PROACTIVE|ASTERON', Description), 'Health',
                                                                            ifelse(grepl('NETFLIX|MOVIES|CINEMA', Description), 'Entertainment',
                                                                            'Other'))))))))))))
dd <- dd %>%
  mutate(
    spendCategory=
      ifelse(grepl('RODNEY ?WAYNE|HUE|ZARA|MOOCHI|SUPERETTE|SISTERS AND CO|KATHRYN ?WILSON|STITCHES|HAIRDRESS', Description), 'EmilyClothesAndBeauty',
             ifelse(grepl('S A F E', Description), 'Charity',
                    ifelse(grepl('K? -?MART|FARMERS|WAREHOUSE|TWL 187 ST LUKES', Description), 'KmartFarmersWarehouse',
                    spendCategory))))

# Look for other big transactions to exclude
unclassifiedBigSpends <- dd %>%
  filter(spendCategory=='Other') %>%
  select(-Balance, -spendCategory, -Reference) %>%
  arrange(Amount) %>%
  head()
cat('Biggest unclassified spends:\n')
print(unclassifiedBigSpends)
unclassifiedBigDeposits <- dd %>%
  filter(spendCategory=='Other') %>%
  select(-Balance, -spendCategory, -Reference) %>%
  arrange(desc(Amount)) %>%
  head()
cat('Biggest unclassified deposits:\n')
print(unclassifiedBigDeposits)

cat(nrow(filter(dd, spendCategory=='Other')), 'transactions constituting', round(-1*sum(dd$Amount[dd$Amount<0]), 0), 'dollars unaccounted for\n')

#__________________________________________________________________________________________________________________________________

# Calc money in and money out per month, total and by spend category ----
#__________________________________________________________________________________________________________________________________

#+++++++++++++++
# Look at final balances (ex visa)
#+++++++++++++++

dd %>%
  filter(acc!='visa') %>%
  group_by(acc) %>%
  arrange(Date) %>%
  filter(row_number()==n()) %>%
  select(acc, Balance, Date)

#+++++++++++++++
# calc spend and deposits by category
#+++++++++++++++
# 
# dd %>%
#   mutate(month=month(date))




# #__________________________________________________________________________________________________________________________________
# 
# # TEMP testing web scraping to get statements ----
# #__________________________________________________________________________________________________________________________________
# 
# # From https://stackoverflow.com/questions/52191599/how-do-i-scrape-my-own-data-from-my-banks-website-using-r
# 
# # OBSOLETE - TOO MUCH OF A SECURITY RISK I THINK, PROB CANT DO IT DUE TO BANKS SECURITY SETTINGS, AND EASY ENOUGH TO DOWNLOAD STATEMENTS AS-AND-WHEN  
# 
# library(rvest)
# url <- "https://homebank.tsbbank.co.nz/online/"
# session <- html_session(url)              
# form <- html_form(session)[[1]]
# filled_form <- set_values(form, card = "5000530012581547",password = "xxx")
# session <- submit_form(session,filled_form)

cat('Code complete - delete all TSB files from', downloadsPath, '\n')

gc()



