
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
library(stringr)
library(pdftools)
library(readxl)

#__________________________________________________________________________________________________________________________________

# Extract/read in TSB data and classify transactions ----
#__________________________________________________________________________________________________________________________________

#+++++++++++++++
# Read in all transactions and dedupe
#+++++++++++++++

oldtsbfiles <- list.files(dataPath)[grepl('[Tt][Ss][Bb]', list.files(dataPath))] # calling 'old' cos I used to read new files in from different locn, but Shiny didn't like this.
oldList <- lapply(oldtsbfiles, function(x) {
  trans <- read.csv(paste0(dataPath, x), stringsAsFactors = FALSE)
  if(grepl('454867036', x)) trans <- mutate(trans, acc='visa')
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
  filter(acc!='visa' | !grepl('PAYMENT - THANK YOU', Description)) %>%
  filter(acc!='revolving' | !grepl('TSB Visa Centre', Description)) %>%
  filter(acc!='revolving' | !grepl('credit card', Particulars)) %>%
  filter(acc!='revolving' | !grepl('T/f To 4548', Description))

# Exclude paying for mum's insurance - and depositing cash back that she paid back
dd <- dd %>%
  filter(Amount!=-626.45 | Date!='2019-06-09')

# Also exclude mortgage repayments that show up as a +ve into mortgage accounts, for same reason as above
dd <- dd %>%
  filter(!grepl('mortgage', acc) | !grepl('Repayment', Particulars))

# Exclude interest payments from within mortgage accounts - these are already accounted for within the mortgage payments from the revolving account. (note would need to add these back in if I wanted to see interest/principal split in future, but then subtract interest amount from revolving acc). Way it works is that interest gets subtracted out of balance within mortgage accounts, then full amount gets paid out of revolving into mortgage accounts.
dd <- filter(dd, !grepl('mortgage', acc) | !grepl('[Ll][Oo][Aa][Nn] ?[Ii][Nn][Tt]', Description))

# Exclude paying down loan out of revolving credit acc
dd <- dd %>%
  filter(!grepl('Lump sum', Description)  | Date!='2019-11-22')

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
      ifelse((grepl('ANNALECT|50119ACC|MINISTRY OF|FAM31|OMD', Description) & Amount > 0) |
               (grepl('RUFFELL', Description) & grepl('ACC', Description) & Amount > 0) |
               Date=='2019-08-27' & Amount==1724.82, 'Pay&GovnContributions',

             # Mortgage & bank fees
             ifelse(grepl('SERVICE FEE|CARD REISSUE FE|ACCOUNT FEE|INTEREST CHARGED|OFFSHORE SERVICE', Description) |
                      (grepl('LOAN INTEREST', Description) & acc=='revolving'), 'BankFees&RevolvingAccInterest',
                    ifelse(grepl('LOAN/EQUITY', Particulars), 'Mortgage',
                           # ifelse(grepl('LOAN INTEREST', Description), 'MortgageInterest', # see note at top of script - this is double-counting interest repayments

                                  # Everything else
                                  ifelse(grepl('FARRO|COUNT ?DOWN|PAK ?N ?SAVE|NEW ?WORLD|SAFFRON|REFILL ?NATION|EAT ?ME|SPORTS ?FUEL|HELLOFRESH|WOOP', Description), 'Groceries',
                                         ifelse(grepl('SAAN|PETAL|BODRAM|CAFE|FARIDAS|AMANO|GOODE|ORTOLANA|ROSEBANK COFFEE|DEAR JERVOIS|SUSHI|KOKORO|OZONE|MONTANA CATERING|BAKERY|BISTRO|RESTAURANT|SUGARGRILL|STARK|1929|GOOD ?HOME|BEER ?BREW|TEED STREET LARDER|INDIAN SUMMER|CHAPATI INDIAN|BREWERY|GENTA|DELI|MR ?ILLINGSWORTH|LIQUOR|KREEM|GARRISON PUBLIC|THAI|CENTRAL KITCHEN|CHINOISERIE|SANTHIYAS|HIGH TEA|KAI|KISS ?KISS|ET TU|LEIGH EATS|PANDORO|LITTLE DISTRICT|BREWED AWAKENING|WINTERGARDENS|ACE KITCHEN|MEXICO|THE GREAT SPICE', Description), 'CafesAlcohol&EatingOut',
                                                ifelse(grepl('^Z |HEEM|UBER|GULL|TYRES|CAR ?PARK|VTNZ|EBIKEFIX|BP|REPCO|TRANSPORT|NEURON|SERVICE STATION|^GAS|CYCLES|TOURNAMENT|WILSON PARKING|AT HOP|MOBIL |PEDALS|NZAA', Description) | (grepl('AA INSURANCE', Description) & Amount >= -300), 'Transport',
                                                       ifelse(grepl('STITCHFOX|TOYS|BABY|MOCKA|H ?& ?M|BAND ?OF ?BOYS|KID ?REPUBLIC|THE ?SLEEP ?STORE|ALYCE|G4U ?DOLLAR ?STORE|COTTON ?ON|WHITCOULLS|EDWARD|TOY LIBRARY', Description) |
                                                                (grepl('TWL', Description) & Amount==-179.00), 
                                                              'Baby',
                                                              ifelse(grepl('MITRE|HAMMER|KINGS|CITTA|GARDENPOST|FREEDOM FURNITURE|ECOCITYELECTRICIANS|HOMESTEAD PICTURE|SPOTLIGHT|STORAGE ?BOX|CARPET ?CLEAN|KODAK|REFUSE ?STATION|GARRISONS|NURSERY|RATES|A CLEANER|AAKLCOUNCIL|CURTAINS|SHUT THE FRONT DOOR', Description) |
                                                                       (grepl('AA INSURANCE', Description) & Amount < -300), 'Home&Garden',
                                                                     ifelse(grepl('WATERCARE|SLINGSHOT|SKINNY|AKL COUNCIL|MERIDIAN', Description), 'Utilities',
                                                                            ifelse(grepl('PHARMACY|ACCIDENT COMP CLAIMS|PHARM|IHERB|MEDICAL|UNICHEM|HEALTH NEW LYNN|PROACTIVE|ASTERON|PHYSIO|MEDSTRENGTH|WHITE CROSS|WAITEMATA ?DHB|KATHERINE RITCHIE|DEBIT RP|WAITAKERE FOOT|DR DEVASHANA', Description) | grepl('YOGA', Particulars), 'Health',
                                                                                   ifelse(grepl('POP-UP ?GLOBE|NETFLIX|MOVIES|READING LYNN|CINEMA|BANFF|YOUTUBE VIDEOS|AUCKLAND ZOO|MOTAT|POP UP GLOBE|PARNELL BATHS|KELLY ?TARLTON', Description), 'Entertainment',
                                                                                          'Other'))))))))))))
dd <- dd %>%
  mutate(
    spendCategory=
      ifelse(grepl('SMITH & CAUGHEY|RODNEY ?WAYNE|CACI|HUE|ZARA|MOOCHI|SISTERS AND CO|KATHRYN ?WILSON|STITCHES|HAIRDRESS|KSUBI|KATIE ?AND ?LINA ?NAILS|MECCA|BRAS ?N ?THINGS|SASS & BIDE|LULU|HUFFER|SHAMPOO N THINGS|AS COLOUR|MI PIACI|KAREN WALKER|BENDON|QUEEN NAIL|LONELY PONSONBY|ONCEIT|AREA 51|SHINE ON|VIVO|LONELY HEARTS|HAIR|BASSIKE|DESIGNER WARDROBE|LUXURY NAILS|PEDICURE|P184180837|R&R NAILS|JULIETTE ?HOGAN|CLINIC ?42|DOSE AND CO|MAGGIE ?MARILYN', Description) |
               (grepl('SUPERETTE', Description) & Amount < -50), # separates superette store from dairies.
             'EmilyClothes&Beauty',
             ifelse(grepl('BURGER|DOMINOS|WENDY|MCDONALD|KEBAB|SUPERETTE|SUBWAY|PITA ?PIT|PIZZA|MEXICALI', Description), 'FastFood',
                    ifelse(grepl('K ?-?MART|FARMERS|WAREHOUSE|TWL 187 ST LUKES', Description), 'KmartFarmersWarehouse',
                           ifelse(grepl('^MO ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ? ?$', Description), 'MoPayments',
                                  ifelse(grepl('TOYCO|EVERYDAY ?HERO|CHARITY|ANIMALS ?AUSTRALIA|PENINSULA BLOOM|FLOWERS|ELTON|S A F E|SAVE ANIMALS|FORME SPA|TICKETMASTER|AUCKLAND AQUARIUM LTD', Description), 'Gifts&Charity',
                                         ifelse(grepl('FIBREGLASS|SURF2SURF|SURFSTITCH|AUDIBLE|BARBER|RED\'S|HALLENSTEINS|TORPEDO 7|AMZN MKTP|DRCOMFORTS|SMARTBUYGLASSES|JUSTCUTS|WEST AUCKLAND HOSPICE|SURF SHOP|SURF SCHOOL|ASICS ONEHUNGA', Description) |
                                                  (grepl('WESTPAC', Description) & grepl(-250, Amount)), 'Jay',
                                                spendCategory)))))))

# For classfying new transactions
tmp <- dd %>%
  # Only look at latest month, if prev months done already
  mutate(month=format(Date, '%b %Y')) %>%
  # filter(month=='Jul 2020') %>%
  filter(Date>'2020-04-01') %>%
  filter(spendCategory=='Other') %>%
  # Split 'other' into known and unknown, so Im only classifying the latter
  mutate(
    spendCategory=
      ifelse(grepl('APPLE NZ|GOOGLE STORAGE|NZEI|POST SHOP|WIGGLE|MICROWAVE|WSL NEW LYNN|APPLE.COM|AMAZON WEB SERVICES', Description), 'Other_known', 'Other')) %>%
  filter(spendCategory!='Other_known') %>%
  arrange(Amount, desc(month)) %>%
  select(-Balance, -spendCategory) %>%
  as.data.frame()
head(tmp, 100)

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

# --------------------------------------------------------------------------------------------------------------



#__________________________________________________________________________________________________________________________________

# Repeat all of above for groceries transactions ----
#__________________________________________________________________________________________________________________________________

#__________________________________________________________________________________________________________________________________

# First, online countdown order statements ----
#__________________________________________________________________________________________________________________________________

#+++++++++++++++
# Read in all transactions and convert from pdf to dataframe
#+++++++++++++++

grocfiles <- list.files(dataPath)[grepl('[Oo]rder', list.files(dataPath)) &
                                    grepl('\\.pdf', list.files(dataPath))] 
grocList <- list()
for(f in 1:length(grocfiles)){

  # Read in pdf and format to dataframe
  g <- pdf_text(paste0(dataPath, grocfiles[f]))
  g <- str_split(g, '\n') # pdf_text uses \r\n to denote new line. 
  g <- unlist(g) # 1 list element per page
  g <- gsub("\r", "", g) # pdf_text gives new lines an '\r\n' symbol, but shiny doesn't like this - have to split lines with '\n' then separately remove the \r
  g <- data.frame(item=g, stringsAsFactors = F)
  
  # Extract metadata: date
  date <- g %>%
    filter(grepl('Date\\s.*20[0-9][0-9]', item)) %>% # word date, then whitespace, ending in year
    pull(item) 
  date <- unique(str_extract(date, '[0-9]?[0-9].*20[0-9][0-9]'))
  if(length(date)>1) stop('date is not uniquely defined')
  
  date <- paste(str_extract(date, '^[0-9]?[0-9]'), # day
                str_extract(date, '[A-Z][a-z][a-z]'), # mo
                str_extract(date, '20[0-9][0-9]$'), sep='-') # year
  date <- as.Date(date, format='%d-%b-%Y')
  
  # Extract metadata: total price
  total <- g %>%
    filter(grepl('[Ii]nvoice ?[Tt]otal', item)) %>% # word date, then whitespace, ending in year
    pull(item) 
  total <- str_extract(total, '\\$[0-9][0-9][0-9]?[0-9]?\\.[0-9][0-9]$') # price only
  if(length(total)>1) stop('total is not uniquely defined')
  total <- as.numeric(gsub('\\$', '', total))
  
  # Extract metadata: delivery price
  delivery <- g %>%
    filter(grepl('[DD]elivery ?[Ff]ee|[Pp]ick ?[Uu]p ?[Ff]ee', item)) %>% # word date, then whitespace, ending in year
    pull(item) 
  delivery <- str_extract(delivery, '\\$[0-9][0-9]?[0-9]?[0-9]?\\.[0-9][0-9]$') # price only
  if(length(delivery)>1) stop('delivery price is not uniquely defined')
  if(is.na(delivery)) stop('delivery price is undefined')
  delivery <- as.numeric(gsub('\\$', '', delivery))
  
  # Filter down to purchased items - row starts with a number (reference), possibly preceded by some whitespace and ends with "$XXX.00" (price). Currently there are a bunch of other rows with e.g. our address, date, etc.
  g <- g %>%
    filter(grepl('^\\s.*[1-9].*\\$[0-9]?[0-9]?[0-9]\\.[0-9][0-9]$', item))
  
  # Add in price var & convert name to lower case
  g <- g %>%
    mutate(amount=str_extract(item, '\\$[0-9]?[0-9]?[0-9]\\.[0-9][0-9]$')) %>%
    mutate(amount=as.numeric(gsub('\\$', '', amount))) %>%
    mutate(item=tolower(item))
  
  # Pull out all remaining numbers and spaces from item, leaving only the item name (currently 'item' includes reference, quantity of item, unit price etc, separated by spaces)
  g <- g %>%
    mutate(item=str_extract(item, '[a-z].*[a-z]'))
  
  # Add in previous metadata - date and delivery price
  g <- bind_rows(g, data.frame(item='delivery', amount=delivery, stringsAsFactors = F))
  g <- mutate(g, date=date)
  
  # Check that order total matches sum of costs - ensures no rows were lost during data cleaning
  if(is.na(total)) stop('Total is NA')
  if(nrow(g[!complete.cases(g),])>0) stop('not all rows of g are complete cases')
  if(is.na(total)) stop('Total is NA')
  if(abs(sum(g$amount)-total)>5 &
     g$date[1]!='2020-06-12' # think this one is off cos of a countdown gift card
     ) stop('Price of individual items stripped out of pdf do not sum to total price') # error if off by >$5
  grocList[[f]] <- g
}
gg <- bind_rows(grocList)

# Add datasource for subsequent data checks
gg <- mutate(gg, dataSource='receipts_OnlineBuy')

#__________________________________________________________________________________________________________________________________

# Now add manually entered grocery statements ----
#__________________________________________________________________________________________________________________________________

#+++++++++++++++
# Read in transactions
#+++++++++++++++

mm <- read_excel(paste0(dataPath, 'manuallyEnteredGroceryReceipts.xlsx'))
mm$date <- as.Date(mm$date)

# Add datasource for subsequent data checks
mm <- mutate(mm, dataSource='receipts_OfflineBuy')

#__________________________________________________________________________________________________________________________________

# Continue with groceries - manually entered and Countdown online orders combined ----
#__________________________________________________________________________________________________________________________________

#++++++++++++++
# Bind both grocery transaction types together
#++++++++++++++

gg <- bind_rows(gg, mm)
rm(mm)

#+++++++++++++++
# Dedupe 
#+++++++++++++++

gg <- distinct(gg)

#+++++++++++++++
# Create ordered factor for month, so ggplot will plot correctly ----
#+++++++++++++++

gg <- gg %>%
  mutate(month=format(date, '%b %Y'))
monthOrder <- gg %>%
  arrange(date) %>%
  distinct(month)
monthOrder <- monthOrder %>%
  mutate(myorder=1:nrow(.))
gg <- gg %>%
  left_join(monthOrder, by='month') %>%
  arrange(date)
gg$month <- factor(gg$month, levels=unique(gg$month[order(gg$myorder)]), ordered=TRUE)

#+++++++++++++++
# rename cols to be identical to dd, so i can repurpose other dd/shiny code, plus make spend -ve for same reason
#+++++++++++++++

gg <- gg %>%
  rename(Date=date, Amount=amount, Description=item) %>%
  mutate(Amount=Amount*-1)

#+++++++++++++++
# Classify transactions ----
#+++++++++++++++

gg$Description <- toupper(gg$Description)
unique(gg$spendCategory)
gg <- gg %>%
  mutate(
    spendCategory=
      ifelse(grepl('FRESH PRODUCE|BEANS GREEN|SPINACH|TOMATOES|BLUEBERRIES|APPLES|CARROTS|POTATOES|KUMARA|MIXED VEGETABLES|FROZEN PEAS|GARLIC|ONIONS|SWEET POTATO|KIWI ?FRUIT|MANDARINS|COURGETTE|GINGER|BANANAS|FROZEN MIXED BERRIES', Description), 'Fruit&Veg',
             ifelse(grepl('MILK|GOPALA|YOGHURT|CHEESE|ANCHOR|CREAM', Description), 'Dairy',
                    ifelse(grepl('PEANUT|ALMOND|SUNFLOWER|APRICOTS|RAISINS|WALNUTS|TASTI|MIXED NUTS|CRANBERR', Description), 'NutsSeeds&DriedFruit',
                           ifelse(grepl('BEPANTHEN|TODDLER|FISH FINGERS|DOROTHY ?BUTLER|FATHER RABBIT|BABY ?SNACKS|ONLY ORGANIC|RAFFERTY|LITTLE BELLIES|BABY FOOD|BABY WIPES|NAPPY|BANANA PORRIDGE|NAPPIES|WEETBIX|SPIRALS', Description), 'Baby',
                                  ifelse(grepl('EGGS|CHICKEN|BEEF', Description), 'Meat&Eggs',
                                         ifelse(grepl('OIL|MASTERFOODS|MRS ?ROGERS|PEPPERCORNS|PAPRIKA', Description), 'OilsHerbs&Spices',
                                                ifelse(grepl('TOFFEE|WHITTAKERS|CADBURY|CHOC|WINE|SAUVIGNON|MALTESERS|LIQUORICE|LICORICE|JUICE', Description), 'Treats',
                                                       ifelse(grepl('PASTA|RICE|CORN ?CHIPS|OATS|BREAD|VOGELS|TORTILLAS|BAGELS|LENTILS|TURTLE BEANS|KIDNEY', Description), 'Carbs&Pulses',
                                                              ifelse(grepl('COFFEE|AVALANCHE|PLUNGER', Description), 'Tea&Coffee',
                                                                     ifelse(grepl('PADS|LAUNDRY|WASH|TOILET|BATHROOM|TAMPON|REXONA|SCHICK|DOVE|BIN LINER|CLEANER|RUBBISH|PAPER|SCOTCH BRITE|NASAL|NUROFEN|PARACETAMOL|SNAPLOCK|SUNBLOCK|TISSUES|IBUPROFEN|DISH BRUSH|MOISTURISER|HAIRSPRAY|PANADOL|FLOSS|TOOTHPASTE|SANITISER|BAND-?AID|PLASTERS', Description), 'Kitchen&Bathroom',                               
                                                                            ifelse(grepl('DELIVERY', Description), 'Delivery',
                                                                                   'Other'))))))))))))

# For classfying new transactions
gg %>%
  # Only look at latest month, if prev months done already
  mutate(month=format(Date, '%b %Y')) %>%
  filter(month=='Nov 2019') %>%
  filter(spendCategory=='Other') %>%
  # Split 'other' into known and unknown, so Im only classifying the latter
  mutate(
    spendCategory=
      ifelse(grepl('TOMATO PASTE|HOT ?CROSS|BATTERY|HONEY|SALT|MARMALADE|SOUP|REUSABLE BAG|POPSICLES|VEGEMITE|FLOWERS', Description), 'Other_known', 'Other')) %>%
  filter(spendCategory!='Other_known') %>%
  arrange(Amount) %>%
  select(Description, Amount, month) %>%
  as.data.frame()
