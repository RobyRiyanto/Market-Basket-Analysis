# Load library
library(arules)
library(arulesViz)
library(dplyr)


# Read transactions from file data_transaksi.txt
data_transaction <- read.transactions(file="https://academy.dqlab.id/dataset/data_transaksi.txt", format="single", sep="\t", cols=c(1,2), skip=1)

# Displays the number of combinations of products contained in the list of existing transactions
inspect(apriori(data_transaction, parameter = list(support=.1, minlen=2, target='frequent itemsets')))

# Displays a List of Transaction Items
data_transaction@itemInfo

# Displays a List of Transaction Codes
data_transaction@itemsetInfo

# Item Frequency
itemFrequency(data_transaction)
# or we can use this 
itemFrequency(data_transaction, type="absolute")

# Top 3 Statistics
data_item <- itemFrequency(data_transaction, type="absolute")

# Doing sorting on data_item
data_item <- sort(data_item, decreasing = TRUE)

# Take the first 3 items
data_item <- data_item[1:3]

# Convert data_item into  dataframe with Product_Name and Amount columns
data_item <- data.frame("Product_Name" = names(data_item), "Amount" = data_item, row.names=NULL)

# Save as file
write.csv(data_item, file="output/top3_item_retail.txt", eol = "\r\n")

# Displays the frequency plot item
itemFrequencyPlot(data_transaction)

# View Itemset per Transaction with Inspect
inspect(data_transaction)

# Generates association rules and saves as variable mba
mba <- apriori(data_transaction)

# View Rules with the inspect function
inspect(mba)

# Filter the RHS
inspect(subset(mba, rhs %in% "Sirup"))

# Filter LHS
inspect(subset(mba, lhs %in% "Gula"))

# FIlter LHS And RHS
inspect(subset(mba, lhs %in% "Pet Food" & rhs %in% "Sirup"))

# Generating Rules with Support and Confidence Parameters
mba <- apriori(data_transaction, parameter = list(supp = 0.1, confidence = 0.5))

# Notice that at the end of the output line, there are 16 rules. More than the previous apriori command execution which only resulted in 3 rules.

# Resulting Inspection Rules
inspect(mba)

# Filter LHS and RHS
inspect(subset(mba, lhs %in% "Teh Celup" | rhs %in% "Teh Celup"))

# Filter based lift
inspect(subset(mba, (lhs %in% "Teh Celup" | rhs %in% "Teh Celup") & lift>1))

# Recommendations - Filter by %ain%
inspect(subset(mba, (lhs %ain% c("Pet Food", "Gula"))))

# Visualizing Rules with Graph
plot(subset(mba, lift>1.1), method="graph")