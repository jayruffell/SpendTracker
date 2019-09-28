
# calc combined ACC + annalect pay each month, and is it too high? i.e how much should I owe ACC and/or IRD?

# Check out IRD analysis below first - gives good summary of earnings vs tax paid for Annalect and ACC.

# Summary from IRD analysis: assuming that the amount they think went into my bank account from ACC and omd is correct (which I can validate) then only issue is why ACC levy doesnt count towards tax paid. 

#__________________________________________________________________________________________________________________________________

# ACC numbers ----
#__________________________________________________________________________________________________________________________________

# What pay should have been
minMonthlyPay_hrsPerWeek <- 20 # how many hrs am I assuming I worked at Annalect each month under min scenario?
maxMonthlyPay <- 7541 # based on 130K salary
minMonthlyPay <- maxMonthlyPay/(40/minMonthlyPay_hrsPerWeek) + (maxMonthlyPay/(40/(40-minMonthlyPay_hrsPerWeek))*0.8) # based on 25hrs pw

paypmdf <- dd %>%
  filter(grepl('ACC|ANNALECT|OMD', Description) & Amount>0) %>%
  mutate(source=ifelse(grepl('ACC', Description), 'ACC', 'Annalect')) %>%
  mutate(month=as.character(month)) %>%
  group_by(month, source) %>%
  summarise(amount=sum(Amount))  %>%
  bind_rows(data.frame(month=c('min', 'max'), amount=c(minMonthlyPay, maxMonthlyPay), source=c('min', 'max'),
            stringsAsFactors=F))

# total pay 
ggplot(paypmdf, aes(month, amount, fill=source)) + geom_bar(stat='identity') + 
  geom_abline(slope=0, intercept=maxMonthlyPay) + geom_abline(slope=0, intercept=minMonthlyPay) + 
  scale_y_continuous(breaks=seq(0, 10000, by=1000)) + 
  ggtitle('Note Jan pay may include 5wks of pay due to Xmas so actual pay may be ok?')

# Descrepancy - how much should I owe ACC?
library('tidyr')
discrepdf <- paypmdf %>%
  filter(!month %in% c('min', 'max', 'Apr 2019')) %>%
  group_by(month) %>%
  summarise(amount=sum(amount)) %>%
  mutate(maxOwed=amount-minMonthlyPay, minOwed=amount-maxMonthlyPay) %>%
  select(-amount) %>%
  gather(key, value, -month)
ggplot(discrepdf, aes(month, value, fill=key)) + geom_bar(stat='identity', position='dodge')

# Print min and max possible amount owed
discrepdf %>%
  mutate(totmax=ifelse(key=='maxOwed', value, 0),
         totmin=ifelse(key=='minOwed', value, 0)) %>%
  summarise(minPossibleOwed=sum(totmin),
            maxPossibleOwed=sum(totmax)) %>%
  gather(key, value) %>%
  print()

# #++++++++++++++
# # Comparing ACC record of pre-tax earnings and pay from their letter with what actually showed up in bank account:
# #++++++++++++++
# 
# # Jan
# dd %>%
#   filter(grepl('ACC|ANNALECT|OMD', Description) & Amount>0) %>%
#   filter(month=='Jan 2019')
# (1195*3 + 1456)*.8 # estimated Annalect earnings for Jan from ACC letter
# 5140 # actual Annalect amount paid into account in Jan
# (1523*3 + 1262)*.8 # estimated Acc pay for Jan from ACC letter
# 4422 # actual acc amount paid into account in Jan
# # ==> Actual Annalect earnings are ~1K higher than estimated by ACC. Is this cos of funny pay in Jan (December pay has to 'last 5 weeks'?)  
# 
# # Feb
# dd %>%
#   filter(grepl('ACC|ANNALECT|OMD', Description) & Amount>0) %>%
#   filter(month=='Feb 2019')
# (1456*3 + 1165)*.8 # estimated Annalect earnings for Feb from ACC letter
# 4709 # actual Annalect amount paid into account in Feb
# (1262*3 + 1553)*.8 # estimated Acc pay for Feb from ACC letter
# 2290+1000+1194 # actual acc amount paid into account in Feb
# # ==> paid too much but not by a huge amount  
# 
# # Mar
# dd %>%
#   filter(grepl('ACC|ANNALECT|OMD', Description) & Amount>0) %>%
#   filter(month=='Mar 2019')
# (1571 + 2031 + 2437)*.8 # estimated Annalect earnings for Feb from ACC letter
# 6933 # actual Annalect amount paid into account in Feb ***BUT*** ACC letter only covers til 17th Mar.
# (1147 + 687 + 282)*.8 # estimated Acc pay for Feb from ACC letter
# 1471+242 # actual acc amount paid into account in Feb
# # ==> Cant tell anything here cos ACC only worked out Annalect pay up to mid March, but bank acc pay is based on full month. Can say that ACC paid what they said they'd pay according to letter.  

#__________________________________________________________________________________________________________________________________

# IRD numbers - from tax assessment letter on myIRD ----
#__________________________________________________________________________________________________________________________________

# IRD summary numbers
amountOwed <- 3950.57
Earnings <- 140836.12 # ACC and OMD
KSEarnings <- 587.01 # Kiwisaver
taxableIncome <- Earnings + KSEarnings
taxOwed <- 37589.59
PAYE <- 35332.76
ACClevy <- 1755.37
taxPaid <- PAYE - ACClevy; taxPaid # WHY IS LEVY DEDUCTED FROM TAX PAYED????
taxCredits <- 61.63 # believe this is tax on KSEarnings??
taxPaidPlusCredits <- taxPaid + taxCredits; taxPaidPlusCredits
discrepancy <- taxOwed - taxPaidPlusCredits; discrepancy

# And numbers from second page of letter - how Earnings are calculated
jobEarnings <- 105893.00
accEarnings <- 34943.12
Earnings - jobEarnings - accEarnings # correct

jobTax <- 27312.50
accTax <- 8020.26
PAYE - jobTax - accTax # correct

# Based on above tax vs income numbers, check if I'm paying the expected tax rates or getting slammed for having ACC as a second job
expectedTaxOwed <- 37589.89 # from online calculator based on taxableIncome
expectedTaxOwed - taxOwed # ==> v close. So IRD tax calculation doesn't assume higher tax rate for ACC payments.

#++++++++++++++
# What I can do from here
#++++++++++++++

#1. use bank statements to confirm ACC and job amounts going into bank account for year ending Mar 31st:
jobEarningsAfterTax <- jobEarnings - jobTax; jobEarningsAfterTax
accEarningsAfterTax <- accEarnings - accTax; accEarningsAfterTax
jobEarningsAfterTax + accEarningsAfterTax
jobTax + accTax
# NB PAYE calculator says based on jobEarnings + AccEarnings take homne pay should be $97,429 - doesnt account for KS tho.

#2. If these numbers are correct then between ACC and IRD I should be paying back the specified amount, *except for issue with levy getting removed from tax owed*. Also this amount will need to be recalculated based on amount paid back to ACC (presumably tax amount paid will stay the same but earnings will drop by amount paid back).
  
# ==> see below for this analysis

#++++++++++++++
# Compare IRD numbers to bank statement numbers - which I have now going back to Mar 2018
#++++++++++++++

# BUT NOTE IRD TAX YEAR IS PRESUMABLY FOR DATE *EARNED* NOT DATE *PAID INTO BANK ACCOUNT*

# Numbrs from westpac statements
bb <- read.csv(paste0(dataPath, 'TEMP_statementsForCheckingIRDOverpayment.csv'), stringsAsFactors = F)

# numbers from tsb
tt <- dd %>%
  filter((grepl('ANNALECT|50119ACC|OMD', Description) & Amount > 0) |
           (grepl('RUFFELL', Description) & grepl('ACC', Description) & Amount > 0))
str(bb)
all <- bb %>%
  mutate(Date=as.Date(Date, format='%d-%b-%y'), source='westpac') %>%
  bind_rows(
    transmute(tt, Date, Amount, From=ifelse(grepl('ACC', Description), 'ACC', 'OMD'), source='tsb')) %>%
  filter(Date>as.Date('2018-03-31')) %>%
           filter(Date<=as.Date('2019-03-31')) %>%
  as.tbl()
ggplot(all, aes(Date, Amount, colour=source)) +geom_point() # check no doubleups

all %>%
  group_by(From) %>%
  summarise(sum(Amount))

actualTakeHomePay <- sum(all$Amount); actualTakeHomePay
onlineCalcTakeHomePay <- 97429 # based on what IRD says OMD + ACC earnings were; excludes KS
actualTakeHomePay - onlineCalcTakeHomePay # ==> assuming I was paid what ACC says, I have an extra $5324 in my bank account over and above what my take home pay should have been.

# # TEMP plot revolving credit levels
# dd %>%
#   filter(acc=='revolving') %>%
#   ggplot(aes(Date, Balance)) + geom_line()

#++++++++++++++
# SUMMARY
#++++++++++++++

# IRD is based on date earned not date paid into bank acc, but based on the latter at least then yes, I earned ~$103K *after tax* for the year ending 31 Mar 2019. This corresponds to more than $140K before tax, so it does look like I've been overpaid by ACC by quite a bit. 

# Still need to figure out why ACC levy is subtracted from tax paid tho, per this line above: taxPaid <- "PAYE - ACClevy; taxPaid".
