library(arules)
trans = read.transactions(file="Transactions.csv", 
                          format="single",
                          sep=",", cols=c(1,2))

trans

#Explore the transaction data using inspect function
inspect(trans)

#Plotting the transaction data using image function.
image(trans) 

#Find the item frequency using itemFrequency function (Support)
itemFrequency(trans)

#Plot item frequency
itemFrequencyPlot(trans)

#Applying Apriori algorithm on the data
#Interested only in those rules that have min sup of 0.2 and conf of 0.6

rules = apriori(trans, parameter=list(sup=0.2, conf=0.6))
inspect(rules)

#Visualizng the rules

library(arulesViz)
plot(rules, method="graph", control=list(itemLabels=T))
