library(arules)

data(Groceries)
str(Groceries)

#The Groceries data set contains 1 month (30 days) of real-world point-of-sale transaction data 
#from a typical local grocery outlet. 
#The data set contains 9835 transactions and the items are aggregated to 169 categories.


inspect(head(Groceries))

# calculates support for frequent items
frequentItems <- eclat (Groceries, parameter = list(supp = 0.07, maxlen = 15)) 

inspect(frequentItems)

itemFrequencyPlot(Groceries, topN=10, type="absolute", main="Item Frequency") 

rules <- apriori (Groceries, parameter = list(supp = 0.001, conf = 0.5)) # Min Support as 0.001, confidence as 0.8.

rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf)) # show the support, lift and confidence for all rules

rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift)) # show the support, lift and confidence for all rules


#To get stronger rules, increase the value of confidence parameter.
#To get longer or shorter rules, increase or decrease maxlength.

rules <- apriori(Groceries, parameter = list (supp = 0.001, conf = 0.7, maxlen=3)) # maxlen = 3 limits the elements in a rule to 3
rules

rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift)) # show the support, lift and confidence for all rules

#to find rules relating to certain items  -- e.g., bottled vs. canned beer?
rules <- apriori (data=Groceries, parameter=list (supp=0.001,conf = 0.08), 
                  appearance = list (default="lhs",rhs="bottled beer"), 
                  control = list (verbose=F)) # get rules that lead to buying 'beer'
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))


rules <- apriori (data=Groceries, parameter=list (supp=0.001,conf = 0.08, minlen=2), 
                  appearance = list(default="rhs",lhs="bottled beer"), 
                  control = list (verbose=F)) # those who bought 'beer' also bought..
rules_conf <- sort (rules, by="lift", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))

#visualize rules
library(arulesViz)

rules <- apriori(Groceries, parameter = list (supp = 0.002, conf = 0.6, maxlen=3)) # maxlen = 3 limits the elements in a rule to 3
rules

plot(rules)
plot(rules, method = "two-key plot")
plot(rules, engine = "plotly")

#graph visualization
subrules <- head(rules, n = 10, by = "confidence")

plot(subrules, method = "graph",  engine = "htmlwidget")

