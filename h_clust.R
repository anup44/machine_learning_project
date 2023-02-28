# install.packages("philentropy")
# install.packages("dendextend")
install.packages("ape")

library(philentropy)
library(readr)
library(ape)
library(ggplot2)
suppressPackageStartupMessages(library(dendextend))
suppressPackageStartupMessages(library(dplyr))


reviews <- read_csv('tokens_vectorized.csv')
reviews = reviews[-1]
head(reviews)
# summary(reviews)
scaled = scale(t(reviews))
scaled[is.na(scaled)] = 0
scaled = as.matrix(scaled)
# dist_reviews <- distance(as.matrix(scaled), method="cosine",use.row.names = TRUE)
sim1 <- scaled / sqrt(colSums(scaled * scaled))
sim1[is.na(sim1)] = 0
sim1[is.infinite(sim1)] = 0
sim <- t(sim1) %*% sim1
dim(sim) 
cos_dist = as.dist(1 - sim)

hclust_avg <- hclust(cos_dist, method = 'average')
hcd <- as.dendrogram(hclust_avg)
cut_avg <- cutree(hclust_avg, k = 10)

avg_col_dend <- color_labels(color_branches(hcd, k = 10), k = 10)
avg_col_dend <- assign_values_to_leaves_nodePar(avg_col_dend, 0.2, "lab.cex")

nodePar <- list(lab.cex = 0.1,
                cex = 0.07)
plot(avg_col_dend, 
     main="Dendogram for hclust using 'average' method for 10 colored clusters", 
     nodePar=nodePar)

reviews_cl <- mutate(reviews, cluster = cut_avg)
count(reviews_cl, cluster)

ggplot(reviews_cl, aes(x = cluster)) + 
  geom_histogram(binwidth=1, color='black') +
  stat_bin(binwidth=1, geom='text', color='white', size=4,
           aes(label=..count..), position=position_stack(vjust=0.5)) +
  labs(title="Histogram of cluster indices for 'average' method") + 
  theme(plot.title = element_text(size=14, face="bold"))

plot(hcd, type = "rectangle", ylab = "Height")

nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), 
                cex = 0.07, col = "blue")
# Customized plot; remove labels
plot(hcd, ylab = "Height", nodePar = nodePar, leaflab = "none")


colors = c("black", "red", "blue", "green", "orange", "purple", "lightblue", "pink", "gray", "brown")
clus4 = cutree(hclust_avg, 10)
plot(as.phylo(hclust_avg), type = "fan", tip.color = colors[clus4],
     label.offset = 1, cex = 0.2)



rect.hclust(hclust_avg , k = 10, border = 2:6)
abline(h = 10, col = 'red')

points = read_csv("trans_points.csv")[-1]
rat_cat = read_csv('rating_category.csv')[-1]
dim(points)
dim(rat_cat)
colnames(rat_cat)[1] = 'review_rating'

reviews_cl1 = bind_cols(reviews_cl, points)
reviews_cl1 = bind_cols(reviews_cl1, rat_cat)

ggplot(reviews_cl1, aes(x = x, y = y, shape = factor(review_rating), color = factor(cluster))) + 
  geom_point() +
  labs(title="Scatter plot of cluster centers for 'average' method") + 
  theme(plot.title = element_text(size=14, face="bold"))

ggplot(reviews_cl1, aes(x = factor(review_rating), y = factor(cluster))) + geom_bin_2d()

ggplot(reviews_cl1, aes(x = factor(category_1), y = factor(cluster))) + geom_bin_2d() + 
  scale_x_discrete(guide = guide_axis(angle = 90))


hclust_ward <- hclust(cos_dist, method = 'ward.D')
hcd_ward <- as.dendrogram(hclust_ward)
cut_ward <- cutree(hclust_ward, k = 10)

ward_col_dend <- color_labels(color_branches(hcd_ward, k = 10), k = 10)
ward_col_dend <- assign_values_to_leaves_nodePar(ward_col_dend, 0.2, "lab.cex")

nodePar <- list(lab.cex = 0.1,
                cex = 0.07)
plot(ward_col_dend, 
     main="Dendogram for hclust using 'ward.D' method for 10 colored clusters", 
     nodePar=nodePar)

reviews_cl <- mutate(reviews, cluster = cut_ward)
count(reviews_cl, cluster)

ggplot(reviews_cl, aes(x = cluster)) + 
  geom_histogram(binwidth=1, color='black') +
  stat_bin(binwidth=1, geom='text', color='white', size=4,
           aes(label=..count..), position=position_stack(vjust=0.5)) +
  labs(title="Histogram of cluster indices for 'ward.D' method") + 
  theme(plot.title = element_text(size=14, face="bold"))


reviews_cl1 <- mutate(reviews_cl1, cluster = cut_ward)

ggplot(reviews_cl1, aes(x = x, y = y, shape = factor(review_rating), color = factor(cluster))) + 
  geom_point() +
  labs(title="Scatter plot of clusters for 'ward.D' method") + 
  theme(plot.title = element_text(size=14, face="bold"))

ggplot(reviews_cl1, aes(x = factor(review_rating), y = factor(cluster))) + 
  geom_bin_2d() +
  stat_bin2d(geom='text', color='white', size=4, aes(label=..count..)) +
  labs(title="2D histogram plot of clusters index vs review rating for 'ward.D' method") + 
  theme(plot.title = element_text(size=14, face="bold"))

ggplot(reviews_cl1, aes(x = factor(category_1), y = factor(cluster))) + 
  geom_bin_2d() +
  stat_bin2d(geom='text', color='white', size=4, aes(label=..count..)) +
  labs(title="2D histogram plot of clusters index vs category for 'ward.D' method") + 
  theme(plot.title = element_text(size=14, face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


hclust_comp <- hclust(cos_dist, method = 'complete')
hcd_comp <- as.dendrogram(hclust_comp)
cut_comp <- cutree(hclust_comp, k = 10)

comp_col_dend <- color_labels(color_branches(hcd_comp, k = 10), k = 10)
comp_col_dend <- assign_values_to_leaves_nodePar(comp_col_dend, 0.2, "lab.cex")

nodePar <- list(lab.cex = 0.1,
                cex = 0.07)
plot(comp_col_dend, 
     main="Dendogram for hclust using 'complete' method for 10 colored clusters", 
     nodePar=nodePar)

reviews_cl <- mutate(reviews, cluster = cut_comp)
count(reviews_cl, cluster)

ggplot(reviews_cl, aes(x = cluster)) + 
  geom_histogram(binwidth=1, color='black') +
  stat_bin(binwidth=1, geom='text', color='white', size=4,
           aes(label=..count..), position=position_stack(vjust=0.5)) +
  labs(title="Histogram of cluster indices for 'complete' method") + 
  theme(plot.title = element_text(size=14, face="bold"))


reviews_cl1 <- mutate(reviews_cl1, cluster = cut_comp)

ggplot(reviews_cl1, aes(x = x, y = y, shape = factor(review_rating), color = factor(cluster))) + 
  geom_point() +
  labs(title="Scatter plot of clusters for 'complete' method") + 
  theme(plot.title = element_text(size=14, face="bold"))

ggplot(reviews_cl1, aes(x = factor(review_rating), y = factor(cluster))) + 
  geom_bin_2d() +
  stat_bin2d(geom='text', color='white', size=4, aes(label=..count..)) +
  labs(title="2D histogram plot of clusters index vs review rating for 'complete' method") + 
  theme(plot.title = element_text(size=14, face="bold"))

ggplot(reviews_cl1, aes(x = factor(category_1), y = factor(cluster))) + 
  geom_bin_2d() +
  stat_bin2d(geom='text', color='white', size=4, aes(label=..count..)) +
  labs(title="2D histogram plot of clusters index vs category for 'complete' method") + 
  theme(plot.title = element_text(size=14, face="bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))






reviews_cl1 <- mutate(reviews_cl1, cluster = cut_avg)
count(reviews_cl1, cluster)

ggplot(reviews_cl1, aes(x = x, y = y, shape = factor(review_rating), color = factor(cluster))) + geom_point()

ggplot(reviews_cl1, aes(x = factor(review_rating), y = factor(cluster))) + geom_bin_2d() +
  stat_bin2d(geom='text', color='white', size=4,
             aes(label=..count..))
  

ggplot(reviews_cl1, aes(x = factor(category_1), y = factor(cluster))) + geom_bin_2d() + 
  scale_x_discrete(guide = guide_axis(angle = 90))
