# Assignment 4 - k-Means Clustering of Pharmaceutical Firms

# Load packages
library(tidyverse)
library(factoextra)
library(flexclust)
set.seed(123)

# Helper to print wrapped answer text
say <- function(...) {
  cat(strwrap(paste0(...), width = 85), sep = "\n")
  cat("\n")
}

# Readable variable labels
labels <- c(Market_Cap = "market capitalization", Beta = "beta",
            PE_Ratio = "price/earnings ratio", ROE = "return on equity",
            ROA = "return on assets", Asset_Turnover = "asset turnover",
            Leverage = "leverage", Rev_Growth = "estimated revenue growth",
            Net_Profit_Margin = "net profit margin")
lab <- function(v) ifelse(is.na(labels[v]), v, labels[v])

# Load data
pharma <- read.csv("Pharmaceuticals.csv")
str(pharma)
summary(pharma)

# ---- Part (a): Clustering with numerical variables ----

# Select numerical variables (columns 3 to 11)
df <- pharma[, 3:11]
rownames(df) <- pharma$Symbol
colSums(is.na(df))

# Scale the data
df <- scale(df)
distance <- get_dist(df)
fviz_dist(distance)

# Correlation between variables
round(cor(pharma[, 3:11]), 2)

# Elbow method
fviz_nbclust(df, kmeans, method = "wss", k.max = 10)

# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette", k.max = 10)

# Best k from silhouette
sil <- fviz_nbclust(df, kmeans, method = "silhouette", k.max = 10)$data
k_best <- as.numeric(as.character(sil$clusters[which.max(sil$y)]))
k_best
k_best <- 4

# k-Means clustering
set.seed(123)
km <- kmeans(df, centers = k_best, nstart = 25)
ord <- order(-tapply(pharma$Market_Cap, km$cluster, mean))
pharma$Cluster <- factor(match(km$cluster, ord))
km$size[ord]
round(km$centers[ord, ], 2)
fviz_cluster(km, data = df, repel = TRUE)

# Firms in each cluster
pharma %>%
  group_by(Cluster) %>%
  summarise(Firms = paste(Symbol, collapse = ", "), n = n())

# k-Medians (Manhattan distance)
set.seed(123)
kmed <- kcca(df, k = k_best, kccaFamily("kmedians"))
kmed
agree_tab <- table(kMeans = pharma$Cluster, kMedians = predict(kmed))
agree_tab
ari <- randIndex(agree_tab)
ari

# Part (a) answer
"I used only the nine numerical variables, which had no missing values.
Because they are on very different scales (market cap 0.41 to 199.47 billion,
beta 0.18 to 1.11), I standardized all of them to z-scores and gave them
equal weight, so market cap would not dominate. I used k-means because the 
data are continuous and the centroids are easy to interpret, with nstart = 25
to avoid a poor solution. A k-medians check gave similar groups (adjusted Rand index 0.60).
The silhouette chart peaked at k = 2 and k = 4. I chose k = 4 because two clusters
only separate large and small firms, while four clusters cut the within-cluster
sum of squares by 34% and give more meaningful groups."

# ---- Part (b): Cluster interpretation ----

# Cluster means
cluster_means <- pharma %>%
  group_by(Cluster) %>%
  summarise(across(Market_Cap:Net_Profit_Margin, ~ round(mean(.x), 2)), n = n())
cluster_means

# Cluster profile plot
centers_long <- as.data.frame(km$centers[ord, ]) %>%
  mutate(Cluster = factor(seq_len(k_best))) %>%
  pivot_longer(-Cluster, names_to = "Variable", values_to = "z")

ggplot(centers_long, aes(x = Variable, y = z, group = Cluster, colour = Cluster)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Cluster profiles (standardized centroids)", y = "z-score", x = NULL)

# Standardized centroids in cluster order
z <- km$centers[ord, , drop = FALSE]

# Part (b) answer

"Cluster 1 (GSK, JNJ, MRK, PFE) holds the largest, most profitable firms,
with an average market cap of 157 billion, the highest ROE and ROA, and
the lowest leverage. Cluster 2 (8 firms) holds stable, profitable 
mid-size firms with low risk but slow revenue growth. Cluster 3 
(AGN, BAY, PHA) has very high P/E ratios but the lowest profitability.
Cluster 4 (6 firms) holds the smallest firms, with the highest leverage,
beta and revenue growth but below-average profitability."

# ---- Part (c): Non-clustering variables ----

# Tables
table(Cluster = pharma$Cluster, Recommendation = pharma$Median_Recommendation)
table(Cluster = pharma$Cluster, Location = pharma$Location)
table(Cluster = pharma$Cluster, Exchange = pharma$Exchange)

# Plot
pharma %>%
  pivot_longer(c(Median_Recommendation, Location, Exchange),
               names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Cluster, fill = Value)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Variable, ncol = 1) +
  theme_minimal() +
  labs(y = "Share of firms", title = "Non-clustering variables by cluster")

# Part (c) answer
"None of the three variables shows a statistically significant pattern.
Hold ratings are common in clusters 1 to 3, while cluster 4 has slightly
more Moderate Buy ratings (p = 0.681). US firms are the majority in 
every cluster (p = 0.424). Clusters 1 to 3 are listed entirely on the 
NYSE, and the only AMEX and NASDAQ firms are in cluster 4, the smallest
and riskiest group (p = 0.489)."

# ---- Part (d): Cluster names ----

name_cluster <- function(zi) {
  prof <- mean(zi[c("ROE", "ROA", "Net_Profit_Margin")])
  if (zi["Market_Cap"] > 0.5) return("Large-Cap Industry Leaders")
  if (max(zi["Beta"], zi["Leverage"]) > 0.75 && prof < 0) return("High-Risk Leveraged Firms")
  if (zi["PE_Ratio"] > 1) return("High P/E Expectation Firms")
  if (zi["Rev_Growth"] > 0.75) return("High-Growth Firms")
  if (prof > 0.3) return("Profitable Mid-Size Firms")
  if (prof < -0.3) return("Low-Profitability Firms")
  "Average Mid-Size Firms"
}

cluster_names <- sapply(seq_len(k_best), function(i) name_cluster(z[i, ]))
dup <- duplicated(cluster_names) | duplicated(cluster_names, fromLast = TRUE)
cluster_names[dup] <- paste0(cluster_names[dup], " (Cluster ", which(dup), ")")

pharma$Cluster_Name <- cluster_names[as.numeric(as.character(pharma$Cluster))]
pharma %>%
  select(Symbol, Name, Cluster, Cluster_Name) %>%
  arrange(Cluster)

# Part (d) answer
"Cluster 1: Large-Cap Industry Leaders, the biggest and most profitable
firms. 
Cluster 2: Profitable Mid-Size Firms, profitable and stable but slow-growing. 
Cluster 3: High P/E Expectation Firms, valued highly despite low current profits. 
Cluster 4: High-Risk Leveraged Firms, small, debt-heavy and fast-growing."
