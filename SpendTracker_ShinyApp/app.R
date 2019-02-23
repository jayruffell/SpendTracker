
# Improvements
# - instructions that only show when data updates, then 'update data' button?
# - instructions, i.e. must have 4 files in downloads folder for new d/ls
# - by default don't show mortgage, but add this in as a checkbox in shiny (include principal and loan)
# - show first and last date of all transactions?
# - show projected spend for current month based on days left
# - For any category, drill down into all transactions for a given month (default 'current month')
# - spends by day over time for given month (default current month)
# - print warnings - Ive collected a couple myself

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
    
    # Give warning if there are any 'true' duplicates (i.e. actual transactions with same account, date, amount etc), as these will get removed by distinct() below.
    trueDupes <- trans[duplicated(trans),]
    if(nrow(trueDupes)>0){
      warning(c('\nThe following rows are true duplicates (i.e. actual transactions with same account, date, amount etc within each raw data file). \nThese will get removed by distinct() below! \nIn future add them back in if numbers are significant:\n', paste0(capture.output(trueDupes), collapse='\n')))
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
    warning(c('\nThe following rows are true duplicates (i.e. actual transactions with same account, date, amount etc within each raw data file). \nThese will get removed by distinct() below! \nIn future add them back in if numbers are significant:\n', paste0(capture.output(trueDupes), collapse='\n')))
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

#__________________________________________________________________________________________________________________________________

# User interface ----
#__________________________________________________________________________________________________________________________________

ui <- fluidPage(
  # titlePanel("How much will Jemily save?"),
  
  sidebarLayout(
    
    #+++++++++++++++
    # UI inputs ----
    #+++++++++++++++
    
    sidebarPanel(
      # numericInput(inputId="numEm", 
      #              label=h4("E monthly salary after tax:"), 
      #              value=10), 
      
      # Select spend category to plot
      radioButtons(inputId="chosenCategory", 
                   label=h4("Spend Category"), 
                   choiceNames=as.list(c('AllCategories', unique(dd$spendCategory))),
                   choiceValues=as.list(c('AllCategories', unique(dd$spendCategory))),
                   selected='AllCategories'),
      
      # If 'AllCategories' is chosen above, decide whether to exclude mortgage
      conditionalPanel(
        condition = "chosenCategory==AllCategories",
        radioButtons(inputId="excludeMortgage", 
                     label=h4("Exclude mortgage payments from plots?"), 
                     choiceNames=list('Yes', 'No'),
                     choiceValues=list('Yes', 'No'),
                     selected='Yes')),

      # Select month to plot
      radioButtons(inputId="chosenMonth", 
                   label=h4("Month"), 
                   choiceNames=as.list(c('All Months', as.character(unique(dd$month)))),
                   choiceValues=as.list(c('All Months', as.character(unique(dd$month)))),
                   selected='All Months')
    ),
      
    #+++++++++++++++
    # UI plot and table positions ----
    #+++++++++++++++
    
    mainPanel(
      br(),
      h5(helpText('INSTRUCTIONS IF ADDING NEW TRANSACTIONS:')),
      h5(helpText('Need to save four files to C:/Users/user/Downloads (TSB should automatically save to this locn); 1 x Visa, 1 x Liberty Revolving, and 2 x Liberty Table (i.e. mortgage)')),
      h5(helpText('Save them for as big a date range as you like, just make sure it overlaps with last time. Code will remove duplicate entries')),
      h5(helpText('Save them in "CSV including balance" format on website (Liberty accounts) or CSV (Visa).')),
      br(),
      # Outputs on unclassified transactions 
      tableOutput("finalBalances"),
      br(),
      tableOutput("unclassifiedSpendsAbove50"),
      br(),
      tableOutput("unclassifiedDepositsAbove50"),
      br(),
      tableOutput("totValueOfUnclassifiedTransns"),
      br(),
      plotOutput("totalBalance"),
      br(),
      plotOutput("totalOutgoings"),
      br(),
      plotOutput("spendByCategory")
      # )
    )
  )
)

#__________________________________________________________________________________________________________________________________

# Define outputs ----
#__________________________________________________________________________________________________________________________________

server <- function(input, output) {
  
  #+++++++++++++++
  # Final balances table
  #+++++++++++++++
  
  output$finalBalances <- renderTable({
    dd %>%
      filter(acc!='visa') %>%
      rename(Account=acc) %>%
      group_by(Account) %>%
      arrange(Date) %>%
      filter(row_number()==n()) %>%
      select(Account, Balance)
  },
  caption=paste('Final balances as at', as.character(max(format(dd$Date, '%d %b')))),
  striped=TRUE,
  caption.placement="top") # NB see xtable options for things like caption
  
  #+++++++++++++++
  # Tables of unclassified transactions valuing >$50
  #+++++++++++++++
  
  output$unclassifiedSpendsAbove50 <- renderTable({
    dd %>%
      filter(spendCategory=='Other' & Amount < -50) %>%
      rename(Account=acc) %>%
      mutate(Date=as.character(Date)) %>%
      select(Date, Amount, Description, Account) %>%
      arrange(Amount)
  },
  caption='Unclassified spends over $50',
  striped=TRUE,
  caption.placement="top") # NB see xtable options for things like caption
  
  output$unclassifiedDepositsAbove50 <- renderTable({
    dd %>%
      filter(spendCategory=='Other' & Amount >50) %>%
      rename(Account=acc) %>%
      mutate(Date=as.character(Date)) %>%
      select(Date, Amount, Description, Account) %>%
      arrange(Amount)
  },
  caption='Unclassified deposits over $50',
  striped=TRUE,
  caption.placement="top") # NB see xtable options for things like caption
  
  #+++++++++++++++
  # Table with info on total value of unclassified transactions
  #+++++++++++++++
  
  output$totValueOfUnclassifiedTransns <- renderTable({
    bind_rows(dd %>%
                filter(spendCategory=='Other' & Amount < 0) %>%
                summarise(Type='Withdrawals', 
                          NumOfTransactions=nrow(.),
                          Amount=sum(Amount)),
              dd %>%
                filter(spendCategory=='Other' & Amount > 0) %>%
                summarise(Type='Deposits', 
                          NumOfTransactions=nrow(.),
                          Amount=sum(Amount))
    )
    # unclassifiedDeposits <- dd %>%
    #   filter(spendCategory=='Other' & Amount > 0)
    # paste0(nrow(unclassifiedSpends), 'withdrawals constituting', round(sum(unclassifiedSpends$Amount), 0), 'dollars unaccounted for\n', 
    #        nrow(unclassifiedDeposits), 'deposits constituting', round(sum(unclassifiedDeposits$Amount), 0), 'dollars unaccounted for\n')
  },
  caption='Total value of unclassified ("Other") transactions',
  striped=TRUE,
  caption.placement="top") # NB see xtable options for things like caption
  
  
  #+++++++++++++++
  # Plot total balance (deposits minus withdrawals)
  #+++++++++++++++
  
  output$totalBalance <- renderPlot({
    dd %>%
      group_by(month) %>%
      summarise(Amount=round(sum(Amount), 0)) %>%
      mutate(posneg=ifelse(Amount>0, 'pos', 'neg')) %>%
      ggplot(aes(month, Amount, fill=posneg)) + geom_bar(stat='identity') + 
      geom_text(aes(label=Amount)) + 
      xlab("") + ylab("Monthly balance ($)") + 
      theme(legend.position="none") + ggtitle('Overall balance (deposits minus withdrawals) per month')
  })
  
  #+++++++++++++++
  # Plot total outgoings
  #+++++++++++++++
  
  output$totalOutgoings <- renderPlot({
    dd %>%
      group_by(month) %>%
      filter(Amount<0) %>%
      summarise(Amount=round(sum(Amount), 0)*-1) %>%
      ggplot(aes(month, Amount)) + geom_bar(stat='identity', fill='#F8766D') + 
      geom_text(aes(label=Amount)) + 
      xlab("") + ylab("Monthly balance ($)") + ggtitle('Total spend per month')
  })
  
  #+++++++++++++++
  # Plot of spend by category
  #+++++++++++++++
  
  output$spendByCategory <- renderPlot({

    # Filter down based on dashboard inputs - could make reactive for all remaining plots?
    if(input$chosenMonth=='All Months' & input$excludeMortgage=='No') {
      plotdf <- dd
    } else if(input$chosenMonth=='All Months' & input$excludeMortgage=='Yes') {
      plotdf <- filter(dd, !grepl('Mortgage', spendCategory))
    } else if(input$chosenMonth!='All Months' & input$excludeMortgage=='No') {
      plotdf <- filter(dd, month==input$chosenMonth)
    } else if(input$chosenMonth!='All Months' & input$excludeMortgage=='Yes') {
      plotdf <- filter(dd, month==input$chosenMonth & !grepl('Mortgage', spendCategory))
    }
  
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
      theme(legend.position="none") + ggtitle(paste('Spend by category:', input$chosenMonth)) +
      coord_flip()
  })
  
  # # Report savings based on current params
  # output$savings <- renderText({
  #   mortgagePayments <- ifelse(input$radio==1, mortgageCurrent,
  #                              ifelse(input$radio==2, mortgageInterestOnly,
  #                                     input$mortgageOther))
  #   paste0("Savings per month under current inputs: $", input$numEm + input$numJay - input$numOut - mortgagePayments)
  # })
  # 
  # # Report Emily earnings required to break even
  # output$earnings <- renderText({
  #   mortgagePayments <- ifelse(input$radio==1, mortgageCurrent,
  #                              ifelse(input$radio==2, mortgageInterestOnly,
  #                                     input$mortgageOther))
  #   paste0("Emily salary required to break even: $",
  #          input$numEm + -1*(input$numEm + input$numJay - input$numOut - mortgagePayments))
  # })
  # 
  # # Report outgoings required to break even
  # output$outgoings <- renderText({
  #   mortgagePayments <- ifelse(input$radio==1, mortgageCurrent,
  #                              ifelse(input$radio==2, mortgageInterestOnly,
  #                                     input$mortgageOther))
  #   paste0("Maximum outgoings required to break even: $",
  #          input$numOut + input$numEm + input$numJay - input$numOut - mortgagePayments)
  # })
  # 
  # # Graph of savings against wide range of param values
  # output$plot1 <- renderPlot({
  #   
  #   mortgagePayments <- ifelse(input$radio==1, mortgageCurrent,
  #                              ifelse(input$radio==2, mortgageInterestOnly,
  #                                     input$mortgageOther))
  # 
  #   # Plot changing e salary and j salary for constant mortgage repayment
  #   currentSavings <- input$numEm + input$numJay - mortgagePayments - input$numOut # to plot current inputs on plot
  #   jaySalVec <- c(input$numJay_4d, input$numJay)
  #   if(input$numEm==0) emSalVec <- seq(from=0, to=1.5, by=0.05) # need ifelse to avoid error in seq() when numEm==0
  #   if(input$numEm!=0) emSalVec <- c(seq(from=0, to=1.5*input$numEm, by=50), input$numEm) # adding input$numEm allows me to highlight current parameter values
  # 
  #   plotdf <- expand.grid(jaySalVec=jaySalVec, emSalVec=emSalVec)
  #   plotdf <- plotdf %>%
  #     mutate(mortgagePayments=mortgagePayments) %>%
  #     mutate(savings=jaySalVec + emSalVec - input$numOut - mortgagePayments) %>%
  #     mutate(`Jay's Salary`=ifelse(jaySalVec==input$numJay_4d, 'Jay salary 4d pw', 'Jay full salary'))
  # 
  #            ggplot(plotdf, aes(emSalVec, savings, colour=`Jay's Salary`)) +
  #              # show breaking even as a horiz line
  #              geom_abline(slope=0, intercept=0, colour='grey') +
  #              geom_line() +
  #              xlab("Emily's salary") + ylab("Savings") + ggtitle("Savings per month under changes to Emily's salary") +
  #              # add a point giving currwent param values
  #              geom_point(x=input$numEm, y=currentSavings, size=10, alpha=0.1, shape=1, colour='black') +
  #              annotate("text", x=input$numEm, y=currentSavings, label="Current inputs", vjust=3, hjust=-0.25) +
  #              theme(text=element_text(size=15))
  #            
  # })
}

#__________________________________________________________________________________________________________________________________

# Create app ----
#__________________________________________________________________________________________________________________________________

shinyApp(ui, server)
# runApp('JemilySavingsWithBaby', display.mode='showcase') # can't have this as an 'active' line in script (if detected it will throw an error). So just save any changes to the script then copy this line into console to run. showcase model helps to debug.
