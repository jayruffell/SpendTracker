#__________________________________________________________________________________________________________________________________

# User interface ----
#__________________________________________________________________________________________________________________________________

ui <- fluidPage(
  tabsetPanel(
    
    # SUMMARY STATS
    tabPanel('Summary', fluid=TRUE,
             # br(),
             # h5(helpText('INSTRUCTIONS IF ADDING NEW TRANSACTIONS:')),
             # h5(helpText('- Need to save four files to Documents/JAMES/SpendTracker/SpendTracker_ShinyApp/pastTransactionsData; 1 x Visa, 1 x Liberty Revolving, and 2 x Liberty Table (i.e. mortgage)')),
             # h5(helpText('- Save them for as big a date range as you like, just make sure it overlaps with last time. Dashboard will remove duplicate entries')),
             # h5(helpText('- Save them in "CSV including balance" format on website (Liberty accounts) or CSV (Visa).')),
             # h5(helpText('- Dashboard will automatically upload new files if they exist in the above folder.')),
             # br(),
             # br(),
             # # Outputs on balances 
             # tableOutput("finalBalances"),
             # br(),
             plotOutput("totalBalance"),
             br(),
             plotOutput("totalOutgoings_noFiltering")),
    
    # SPEND BY CATEGORY
    tabPanel('Spend by category', fluid=TRUE,
             sidebarLayout(

               #+++++++++++++++
               # UI inputs ----
               #+++++++++++++++

               sidebarPanel(
                 # Select month to plot
                 radioButtons(inputId="chosenMonth",
                              label=h4("Month"),
                              choiceNames=as.list(c('All Months', rev(as.character(unique(dd$month))))),
                              choiceValues=as.list(c('All Months', rev(as.character(unique(dd$month))))),
                              selected='All Months'),

                 # If 'AllCategories' is chosen above, decide whether to exclude mortgage
                 radioButtons(inputId="excludeMortgage",
                              label=h4("Exclude mortgage payments from plots?"),
                              choiceNames=list('Yes', 'No'),
                              choiceValues=list('Yes', 'No'),
                              selected='Yes'),

                 # Select spend category to plot
                 radioButtons(inputId="chosenCategory",
                              label=h4("Spend Category"),
                              choiceNames=as.list(c('AllCategories', unique(dd$spendCategory))),
                                choiceValues=as.list(c('AllCategories', unique(dd$spendCategory))),
                                selected='AllCategories')
               ),

               #+++++++++++++++
               # UI plot and table positions ----
               #+++++++++++++++

               mainPanel(
                 plotOutput("spendByCategory"),
                 br(),
                 plotOutput("totalOutgoings_categoryFiltering"),
                 br(),
                 plotOutput("spendOverTime", click='plot_click'),
                 tableOutput("spendOverTimeInfo"), # for clicking on plot and returning output
                 br(),
                 tableOutput("filteredTransactions")
               )
             )
    ),

    # SPEND BY CATEGORY - GROCERIES
    tabPanel('Groceries', fluid=TRUE,
             sidebarLayout(
               
               #+++++++++++++++
               # UI inputs ----
               #+++++++++++++++
               
               sidebarPanel(
                 # Select month to plot
                 radioButtons(inputId="chosenMonth_g",
                              label=h4("Month"),
                              choiceNames=as.list(c('All Months', rev(as.character(unique(gg$month))))),
                              choiceValues=as.list(c('All Months', rev(as.character(unique(gg$month))))),
                              selected='All Months'),

                 # Select spend category to plot
                 radioButtons(inputId="chosenCategory_g",
                              label=h4("Spend Category"),
                              choiceNames=as.list(c('AllCategories', unique(gg$spendCategory))),
                              choiceValues=as.list(c('AllCategories', unique(gg$spendCategory))),
                              selected='AllCategories')
               ),
               
               #+++++++++++++++
               # UI plot and table positions ----
               #+++++++++++++++
               
               mainPanel(
                 plotOutput("grocerySpendsCheck"),
                 br(),
                 plotOutput("spendByCategory_g"),
                 br(),
                 plotOutput("totalOutgoings_categoryFiltering_g"),
                 br(),
                 plotOutput("spendOverTime_g", click='plot_click_g'),
                 tableOutput("spendOverTimeInfo_g"), # for clicking on plot and returning output
                 br(),
                 tableOutput("filteredTransactions_g")
               )
             )
    ),
    
    # PAY AND DEPOSITS
    tabPanel('Pay', fluid=TRUE,
             sidebarLayout(

               #+++++++++++++++
               # UI inputs ----
               #+++++++++++++++

               sidebarPanel(

                 # Select month to plot
                 radioButtons(inputId="chosenMonth3",
                              label=h4("Month"),
                              choiceNames=as.list(c('All Months', rev(as.character(unique(dd$month))))),
                              choiceValues=as.list(c('All Months', rev(as.character(unique(dd$month))))),
                              selected='All Months')
               ),

               #+++++++++++++++
               # UI plot and table positions ----
               #+++++++++++++++

               mainPanel(
                 h5(helpText('"Pay" is defined as any deposits  over $300.')),
                 br(),
                 plotOutput("payByMonth"),
                 br(),
                 plotOutput("payOverTime", click='plot_click2'),
                 tableOutput("payOverTimeInfo"), # for clicking on plot and returning output
                 br(),
                 tableOutput("filteredTransactions_pay")
               )
             )
    ),

    # SUMMARY OF UNCLASSIFIED TRANSACTIONS
    tabPanel('Unclassified & duplicated transactions', fluid=TRUE,
             sidebarLayout(
               sidebarPanel(
                 # Select month to plot
                 radioButtons(inputId="chosenMonth2", 
                              label=h4("Month"), 
                              choiceNames=as.list(c('All Months', rev(as.character(unique(dd$month))))),
                              choiceValues=as.list(c('All Months', rev(as.character(unique(dd$month))))),
                              selected='All Months')
               ),
               mainPanel(tableOutput("duplicatedTransns"),
                         br(),
                         tableOutput("totValueOfUnclassifiedTransns"),
                         br(),
                         tableOutput("unclassifiedSpendsAbove50"),
                         br(),
                         tableOutput("unclassifiedDepositsAbove50"),
                         br(),
                         tableOutput("allUnclassifiedTransactions"),
                         br()
               )
             )
    ),
    
    # INSTRUCTIONS
    tabPanel('Instructions', fluid=TRUE, verbatimTextOutput("instructions") 
             # sidebarLayout(
             #   sidebarPanel(),
               # mainPanel(textOutput("instructions")
               # )
             # )
    )
  )
)
