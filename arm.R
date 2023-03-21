# install.packages("arules")
# install.packages("arulesViz")
# install.packages("rgl")


# Loading package
library(arules)
library(arulesViz)
library(ggplot2)
options(rgl.useNULL = TRUE)
library(rgl)
library(rgl)
library(tidyverse)
library(viridis)
library(TSP)
library(data.table)
#library(ggplot2)
#library(Matrix)
library(tcltk)
library(dplyr)
library(devtools)
library(purrr)
library(tidyr)

library(arulesViz)
library(htmlwidgets)

dataset = read.transactions('also_viewed_basket.csv', 
                            sep = ',', rm.duplicates = TRUE)
inspect(dataset[1:4])

itemFrequencyPlot(dataset, topN = 10)
# Fitting model
# Training Apriori on the dataset
set.seed = 220 # Setting seed
associa_rules = apriori(data = dataset, 
                        parameter = list(support = 0.01, 
                                         confidence = 0.6,
                                         maxlen = 5))

# Plot


# Visualising the results
inspect(sort(associa_rules, by = 'support')[1:15])
inspect(sort(associa_rules, by = 'confidence')[1:15])
inspect(sort(associa_rules, by = 'lift')[1:15])

left_sp_rules <- apriori(data = dataset, 
                         parameter = list(supp=.001, conf=.01, minlen=2, maxlen = 5),
        appearance = list(default="rhs", lhs="B01GCKO9IK"),
        control=list(verbose=FALSE))
left_sp_rules <- sort(left_sp_rules, decreasing=TRUE, by="lift")
inspect(left_sp_rules[1:15])

associa_rules

subrules_lift <- sort(associa_rules, by = 'lift')[1:50]
subrules_conf <- sort(associa_rules, by = 'confidence')[1:50]
subrules_sup <- sort(associa_rules, by = 'support')[1:50]

subrules <- union(subrules_lift, subrules_conf)
subrules <- union(subrules, subrules_sup)
subrules

subrules_sup_40 <- sort(associa_rules, by = 'support')[1:40]

plot(subrules, method = "graph", 
     measure = "confidence", shading = "lift")

summary(associa_rules)
plot(associa_rules)
plot(associa_rules, method = "two-key plot", measure=c("support", "lift"))
plot(associa_rules, measure=c("support", "lift"), shading = "confidence")
plot(associa_rules, measure=c("support", "confidence"), shading = "lift")

plot(subrules)
plot(subrules, method = "two-key plot", measure=c("support", "lift"))
plot(subrules, measure=c("support", "lift"), shading = "confidence")
plot(subrules, measure=c("support", "confidence"), shading = "lift")

rule_df <- DATAFRAME(associa_rules)
# rule_df$sup_bin <- cut(rule_df$support, breaks = quantile(rule_df$support, probs = seq(0, 1, 0.25)), include.lowest = TRUE)
rule_df$sup_str <- toString(rule_df$support)

ggplot(rule_df, aes(x=support, y=confidence)) +
  geom_bin2d() + theme_bw()

ggplot(rule_df, aes(x=lift, y=confidence)) +
  geom_bin2d() + theme_bw()

ggplot(rule_df, aes(x=support, y=lift)) +
  geom_bin2d() + theme_bw()

# rule_df %>% pull(LHS) %>% .[1]

# item_counts <- sapply(associa_rules, function(x) length(lhs(x)) + length(rhs(x)))

# rules = inspect(associa_rules)

plot(subrules_lift, method = "graph")
plot(subrules_lift, method="graph", engine="interactive", measure = "lift")
plot(subrules_lift, method = "paracoord", control = list(reorder = TRUE))

plot(subrules, method="graph", engine="htmlwidget")

w = plot(subrules, method="graph", engine="htmlwidget", measure = "lift")
saveWidget(
  w,
  'arm_graph_widget_lift.html',
  selfcontained = TRUE,
  libdir = NULL,
  background = "white",
  title = class(w)[[1]],
  knitrOptions = list()
)

plot(subrules_sup_40, method = "graph")

# sel <- plot(subrules_lift, measure=c("support", "lift"), shading = "confidence",interactive = TRUE)
