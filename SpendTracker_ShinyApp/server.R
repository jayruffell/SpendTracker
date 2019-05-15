#__________________________________________________________________________________________________________________________________

# Define outputs ----
#__________________________________________________________________________________________________________________________________

server <- function(input, output) {

  #__________________________________________________________________________________________________________________________________
  
  # First set base font size for all plots ----
  #__________________________________________________________________________________________________________________________________
  
  theme_set(theme_grey(base_size = 14)) 
    
  #__________________________________________________________________________________________________________________________________
  
  # Summary tab outputs ----
  #__________________________________________________________________________________________________________________________________
  
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
      xlab("") + ylab("") + 
      theme(legend.position="none") + ggtitle('Overall balance (deposits minus withdrawals) per month')
  })
  
  #+++++++++++++++
  # Plot total outgoings
  #+++++++++++++++
  
  output$totalOutgoings_noFiltering <- renderPlot({
    dd %>%
      group_by(month) %>%
      filter(Amount<0) %>%
      summarise(Amount=round(sum(Amount), 0)*-1) %>%
      ggplot(aes(month, Amount)) + geom_bar(stat='identity', fill='#F8766D') + 
      geom_text(aes(label=Amount)) + 
      xlab("") + ylab("") + ggtitle('Total spend per month')
  })
  
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
  width=300,
  caption=paste('Final balances as at', as.character(max(dd$Date, '%d %b'))),
  striped=TRUE,
  caption.placement="top") # NB see xtable options for things like caption
  
  #__________________________________________________________________________________________________________________________________
  
  # Spend by category tab ----
  #__________________________________________________________________________________________________________________________________
  
  #+++++++++++++++
  # For all plots in this section filter to exclude deposits, then make spends positive
  #+++++++++++++++
  
  spendsdf <- dd %>%
    filter(Amount<0) %>%
    mutate(Amount=-1*round(Amount, 0))
  
  #+++++++++++++++
  # Plot total outgoings, filterable by category and mortgage y/n
  #+++++++++++++++
  
  output$totalOutgoings_categoryFiltering <- renderPlot({
    
    
    # Filter down based on dashboard inputs
    if(input$chosenCategory=='AllCategories' & input$excludeMortgage=='No') {
      plotdf <- spendsdf
    } else if(input$chosenCategory=='AllCategories' & input$excludeMortgage=='Yes') {
      plotdf <- filter(spendsdf, !grepl('Mortgage', spendCategory))
    } else if(input$chosenCategory!='AllCategories' & input$excludeMortgage=='No') {
      plotdf <- filter(spendsdf, spendCategory==input$chosenCategory)
    } else if(input$chosenCategory!='AllCategories' & input$excludeMortgage=='Yes') {
      plotdf <- filter(spendsdf, spendCategory==input$chosenCategory & !grepl('Mortgage', spendCategory))
    }
    
    # Plot
    plotdf %>%
      group_by(month) %>%
      summarise(Amount=round(sum(Amount), 0)) %>%
      ggplot(aes(month, Amount)) + geom_bar(stat='identity', fill='#F8766D') +
      geom_text(aes(label=Amount)) +
      xlab("") + ylab("") + ggtitle(paste('Total spend per month:', input$chosenCategory))
  })
  
  #+++++++++++++++
  # Plot of spend by category, filterable by month and mortgage y/n
  #+++++++++++++++
  
  output$spendByCategory <- renderPlot({
    
    # Filter down based on dashboard inputs
    if(input$chosenMonth=='All Months' & input$excludeMortgage=='No') {
      plotdf <- spendsdf
    } else if(input$chosenMonth=='All Months' & input$excludeMortgage=='Yes') {
      plotdf <- filter(spendsdf, !grepl('Mortgage', spendCategory))
    } else if(input$chosenMonth!='All Months' & input$excludeMortgage=='No') {
      plotdf <- filter(spendsdf, month==input$chosenMonth)
    } else if(input$chosenMonth!='All Months' & input$excludeMortgage=='Yes') {
      plotdf <- filter(spendsdf, month==input$chosenMonth & !grepl('Mortgage', spendCategory))
    }
    
    # Plot
    plotdf %>%
      group_by(spendCategory) %>%
      summarise(Amount=sum(Amount)) %>%
      # Order spend category so shows better order in plots
      mutate(spendCategory=factor(spendCategory, levels=unique(spendCategory[order(Amount)]))) %>%
      ggplot(aes(spendCategory, Amount, fill=spendCategory)) + geom_bar(stat='identity') + 
      geom_text(aes(label=Amount)) + 
      xlab("") + ylab("") + 
      theme(legend.position="none") + ggtitle(paste('Spend by category:', input$chosenMonth)) +
      coord_flip()
  })
  
  #+++++++++++++++
  # Plot of spend over time, filterable by month, category, and mortgage y/n, plus clickable output from above
  #+++++++++++++++
  
  # Believe needs to be in reactive expression so I can pass to near_points() i.e. clickable output
  plotdf_spends <- reactive({
    # Filter down based on dashboard inputs
    if(input$chosenMonth=='All Months') plotdf1 <- spendsdf else plotdf1 <- filter(spendsdf, month==input$chosenMonth)
    if(input$excludeMortgage=='No') plotdf2 <- plotdf1 else plotdf2 <- filter(plotdf1, !grepl('Mortgage', spendCategory))
    if(input$chosenCategory=='AllCategories') plotdf3 <- plotdf2 else plotdf3 <- filter(plotdf2, spendCategory==input$chosenCategory)
    plotdf3
  })
  
  # Plot
  output$spendOverTime <- renderPlot({
    plotdf_spends() %>%
      ggplot(aes(Date, Amount)) + geom_point(colour='#00BFC4', alpha=0.9, size=2) + 
      ylab("") + xlab("") +
      ggtitle(paste('Spend over time:', input$chosenMonth, input$chosenCategory, '. Click on plot to see transactions below.'))
  })
  
  #+++++++++++++++
  # Clickable output from above plot
  #+++++++++++++++
  
  output$spendOverTimeInfo <- renderTable({
    nearPoints(plotdf_spends(), 
               input$plot_click, threshold = 10, maxpoints = 100) %>%
      transmute(Date=as.character(Date), Amount, Description)
  })
  
  #+++++++++++++++
  # Table of transactions, filterable by month, category, and mortgage y/n
  #+++++++++++++++
  
  output$filteredTransactions <- renderTable({
    
    # Filter down based on dashboard inputs
    if(input$chosenMonth=='All Months') tabledf1 <- spendsdf else tabledf1 <- filter(spendsdf, month==input$chosenMonth)
    if(input$excludeMortgage=='No') tabledf2 <- tabledf1 else tabledf2 <- filter(tabledf1, !grepl('Mortgage', spendCategory))
    if(input$chosenCategory=='AllCategories') tabledf3 <- tabledf2 else tabledf3 <- filter(tabledf2, spendCategory==input$chosenCategory)
    
    # Plot
    tabledf3 %>%
      arrange(Date) %>%
      rename(Account=acc) %>%
      mutate(Date=as.character(Date)) %>%
      select(Date, Amount, Description, Account)
  },
  caption='All transactions for filtered options',
  striped=TRUE,
  caption.placement="top") # NB see xtable options for things like caption)

  #__________________________________________________________________________________________________________________________________
  
  # Groceries tab ----
  #__________________________________________________________________________________________________________________________________
  
  output$grocerySpendsCheck <- renderPlot({
    
    #+++++++++++++++
    # TEMP check groceries bill from 'gg' (groc receipts) matches up with groceries bill from 'dd' (bank statements)
    #+++++++++++++++
    
    checkdf <- bind_rows(gg %>%
                           group_by(Date) %>%
                           summarise(Amount=sum(Amount)*-1) %>%
                           mutate(dataSource='receipts'),
                         dd %>%
                           filter(Amount<0) %>% # there's an outlier where we got +$20 from countdown online. Rebate??
                           filter(spendCategory=='Groceries') %>%
                           mutate(dataSource=ifelse(grepl('ONLINE', Description), 
                                                    'bankStatements_OnlineBuy', 'bankStatements_OfflineBuy')) %>%
                           group_by(Date, dataSource) %>%
                           summarise(Amount=sum(Amount)*-1))
    ggplot(checkdf, aes(Date, Amount, colour=dataSource)) + geom_line() + ylab('') + xlab ("") +
      ggtitle('Data check: does data from receipts match data from bank statements?')
  })
  
  #+++++++++++++++
  # For all plots in this section filter to exclude deposits, then make spends positive
  #+++++++++++++++
  
  spendsdf_g <- gg %>%
    mutate(Amount=-1*round(Amount, 0))
  
  #+++++++++++++++
  # Plot total outgoings, filterable by category
  #+++++++++++++++
  
  output$totalOutgoings_categoryFiltering_g <- renderPlot({
    
    
    # Filter down based on dashboard inputs
    if(input$chosenCategory_g=='AllCategories') {
      plotdf <- spendsdf_g
    } else if(input$chosenCategory_g!='AllCategories') {
      plotdf <- filter(spendsdf_g, spendCategory==input$chosenCategory_g)
    }
    
    # Plot
    plotdf %>%
      group_by(month) %>%
      summarise(Amount=round(sum(Amount), 0)) %>%
      ggplot(aes(month, Amount)) + geom_bar(stat='identity', fill='#F8766D') +
      geom_text(aes(label=Amount)) +
      xlab("") + ylab("") + ggtitle(paste('Total spend per month:', input$chosenCategory_g))
  })
  
  #+++++++++++++++
  # Plot of spend by category, filterable by month
  #+++++++++++++++
  
  output$spendByCategory_g <- renderPlot({
    
    # Filter down based on dashboard inputs
    if(input$chosenMonth_g=='All Months') {
      plotdf <- spendsdf_g
    } else if(input$chosenMonth_g!='All Months') {
      plotdf <- filter(spendsdf_g, month==input$chosenMonth_g)
    }
    
    # Plot
    plotdf %>%
      group_by(spendCategory) %>%
      summarise(Amount=sum(Amount)) %>%
      # Order spend category so shows better order in plots
      mutate(spendCategory=factor(spendCategory, levels=unique(spendCategory[order(Amount)]))) %>%
      ggplot(aes(spendCategory, Amount, fill=spendCategory)) + geom_bar(stat='identity') + 
      geom_text(aes(label=Amount)) + 
      xlab("") + ylab("") + 
      theme(legend.position="none") + ggtitle(paste('Spend by category:', input$chosenMonth_g)) +
      coord_flip()
  })
  
  #+++++++++++++++
  # Plot of spend over time, filterable by month & category, plus clickable output from above
  #+++++++++++++++
  
  # Believe needs to be in reactive expression so I can pass to near_points() i.e. clickable output
  plotdf_spends_g <- reactive({
    # Filter down based on dashboard inputs
    if(input$chosenMonth_g=='All Months') plotdf2 <- spendsdf_g else plotdf2 <- filter(spendsdf_g, month==input$chosenMonth_g)
    if(input$chosenCategory_g=='AllCategories') plotdf3 <- plotdf2 else plotdf3 <- filter(plotdf2, spendCategory==input$chosenCategory_g)
    plotdf3
  })
  
  # Plot
  output$spendOverTime_g <- renderPlot({
    plotdf_spends_g() %>%
      ggplot(aes(Date, Amount)) + geom_point(colour='#00BFC4', alpha=0.9, size=2) + 
      ylab("") + xlab("") +
      ggtitle(paste('Spend over time:', input$chosenMonth_g, input$chosenCategory_g, '. Click on plot to see transactions below.'))
  })
  
  #+++++++++++++++
  # Clickable output from above plot
  #+++++++++++++++
  
  output$spendOverTimeInfo_g <- renderTable({
    nearPoints(plotdf_spends_g(), 
               input$plot_click_g, threshold = 10, maxpoints = 100) %>%
      transmute(Date=as.character(Date), Amount, Description)
  })
  
  #+++++++++++++++
  # Table of transactions, filterable by month, category, and mortgage y/n
  #+++++++++++++++
  
  output$filteredTransactions_g <- renderTable({
    
    # Filter down based on dashboard inputs
    if(input$chosenMonth_g=='All Months') tabledf2 <- spendsdf_g else tabledf2 <- filter(spendsdf_g, month==input$chosenMonth_g)
    if(input$chosenCategory_g=='AllCategories') tabledf3 <- tabledf2 else tabledf3 <- filter(tabledf2, spendCategory==input$chosenCategory_g)
    
    # Plot
    tabledf3 %>%
      arrange(Date) %>%
      # rename(Account=acc) %>%
      mutate(Date=as.character(Date)) %>%
      select(Date, Amount, Description) # , Account)
  },
  caption='All transactions for filtered options',
  striped=TRUE,
  caption.placement="top") # NB see xtable options for things like caption)
  
  #__________________________________________________________________________________________________________________________________

  # Pay outputs ----
  #__________________________________________________________________________________________________________________________________

  #+++++++++++++++
  # For all plots in this section filter to exclude withdrawals and any deposits<200
  #+++++++++++++++

  paydf <- dd %>%
    filter(Amount>200)

  #+++++++++++++++
  # Plot pay by month
  #+++++++++++++++

  output$payByMonth <- renderPlot({

    # Plot
    paydf %>%
      group_by(month) %>%
      summarise(Amount=round(sum(Amount), 0)) %>%
      ggplot(aes(month, Amount)) + geom_bar(stat='identity', fill='#F8766D') +
      geom_text(aes(label=Amount)) +
      xlab("") + ylab("") + ggtitle('Total pay per month')
  })
  
  #+++++++++++++++
  # Plot of pay over time, filterable by month
  #+++++++++++++++
  
  # Believe needs to be in reactive expression so I can pass to near_points() i.e. clickable output
  plotdf_pay <- reactive({
    # Filter down based on dashboard inputs
    if(input$chosenMonth3=='All Months') plotdf <- paydf else plotdf <- filter(paydf, month==input$chosenMonth3)
  })

  # Plot
  output$payOverTime <- renderPlot({
    plotdf_pay() %>%
      ggplot(aes(Date, Amount)) + geom_point(colour='#00BFC4', alpha=0.9, size=2) +
      ylab("") + xlab("") +
      ggtitle(paste('Pay over time:', input$chosenMonth, '. Click on plot to see transactions below.'))
  })

  # Clickable output from above plot
  output$payOverTimeInfo <- renderTable({
    nearPoints(plotdf_pay(), 
               input$plot_click2, threshold = 10, maxpoints = 100) %>%
      transmute(Date=as.character(Date), Amount, Description)
  })
  
  #+++++++++++++++
  # Table of transactions, filterable by month
  #+++++++++++++++

  output$filteredTransactions_pay <- renderTable({

    # Filter down based on dashboard inputs
    if(input$chosenMonth3=='All Months') tabledf <- paydf else tabledf <- filter(paydf, month==input$chosenMonth3)
    # Plot
    tabledf %>%
      arrange(Date) %>%
      rename(Account=acc) %>%
      mutate(Date=as.character(Date)) %>%
      select(Date, Amount, Description, Account)
  },
  caption='All transactions for filtered options',
  striped=TRUE,
  caption.placement="top") # NB see xtable options for things like caption)
  
  
  #__________________________________________________________________________________________________________________________________
  
  # Unclassified transactions tab ----
  #__________________________________________________________________________________________________________________________________
  
  #+++++++++++++++
  # Warning about duplicated transactions
  #+++++++++++++++
  
  # true duplicates (i.e. actual transactions with same account, date, amount etc within each raw data file), which were removed when data from overlapping dateranges was combined. In future add them back in if numbers are significant
  output$duplicatedTransns <- renderTable({
    if(input$chosenMonth2=='All Months') trueDupes else filter(trueDupes, month==input$chosenMonth2)
  },
  caption='Duplicated transactions that have been incorrectly removed from analysis',
  striped=TRUE,
  caption.placement="top") # NB see xtable options for things like caption
  
  #+++++++++++++++
  # Tables of unclassified transactions valuing >$50
  #+++++++++++++++
  
  # Filter down based on inputs for next few tables
  tabledfFun <- reactive({
    if(input$chosenMonth2=='All Months') {
      dd 
    } else {
      filter(dd, month==input$chosenMonth2)
    }
  })
  
  output$unclassifiedSpendsAbove50 <- renderTable({
    tabledfFun() %>%
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
    tabledfFun() %>%
      filter(spendCategory=='Other' & Amount >50) %>%
      rename(Account=acc) %>%
      mutate(Date=as.character(Date)) %>%
      select(Date, Amount, Description, Account) %>%
      arrange(Amount)
  },
  caption='Unclassified deposits over $50',
  striped=TRUE,
  caption.placement="top") # NB see xtable options for things like caption
  
  output$allUnclassifiedTransactions <- renderTable({
    tabledfFun() %>%
      filter(spendCategory=='Other') %>%
      rename(Account=acc) %>%
      mutate(Date=as.character(Date)) %>%
      select(Date, Amount, Description, Account) %>%
      arrange(Amount)
  },
  caption='All unclassified transactions',
  striped=TRUE,
  caption.placement="top") # NB see xtable options for things like caption
  
  #+++++++++++++++
  # Table with info on total value of unclassified transactions
  #+++++++++++++++
  
  output$totValueOfUnclassifiedTransns <- renderTable({
    bind_rows(tabledfFun() %>%
                filter(spendCategory=='Other' & Amount < 0) %>%
                summarise(Type='Withdrawals', 
                          NumOfTransactions=nrow(.),
                          Amount=sum(Amount)),
              tabledfFun() %>%
                filter(spendCategory=='Other' & Amount > 0) %>%
                summarise(Type='Deposits', 
                          NumOfTransactions=nrow(.),
                          Amount=sum(Amount))
    )
  },
  caption='Total value of unclassified ("Other") transactions',
  striped=TRUE,
  caption.placement="top") # NB see xtable options for things like caption
}
