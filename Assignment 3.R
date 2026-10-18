# ============================================================
# 5 - Naive Bayes: Universal Bank
# ============================================================

# install.packages("caret")     # install first if needed
install.packages("e1071")     # install first if needed
install.packages("reshape2")  # install first if needed
library(caret)
library(e1071)
library(reshape2)

setwd("C:/Users/kasen/OneDrive/Machine Learning")
MyData <- read.csv("UniversalBank.csv")
summary(MyData)


# keep only Online, CreditCard, and Personal Loan
# (read.csv turns "Personal Loan" into "Personal.Loan")
MyData <- MyData[, c("Online", "CreditCard", "Personal.Loan")]
names(MyData) <- c("Online", "CC", "Loan")

# convert all three variables to factors (they are 0/1 categorical)
MyData$Online <- factor(MyData$Online)
MyData$CC     <- factor(MyData$CC)
MyData$Loan   <- factor(MyData$Loan)

set.seed(123)
# training (60%) and validation (40%)
Index_Train <- createDataPartition(MyData$Loan, p = 0.6, list = FALSE)
Train <- MyData[Index_Train, ]
Valid <- MyData[-Index_Train, ]
nrow(Train)
nrow(Valid)

# ============================================================
# A. Pivot table: Online as columns, CC and Loan as rows (counts)
# ============================================================
Melted  <- melt(Train, id.vars = c("CC", "Loan"), measure.vars = "Online")
Pivot_A <- dcast(Melted, CC + Loan ~ value, fun.aggregate = length)
names(Pivot_A)[3:4] <- c("Online_0", "Online_1")
Pivot_A

# ============================================================
# B. P(Loan = 1 | CC = 1, Online = 1) from the pivot table
# ============================================================
Accepted <- Pivot_A$Online_1[Pivot_A$CC == 1 & Pivot_A$Loan == 1]
Rejected <- Pivot_A$Online_1[Pivot_A$CC == 1 & Pivot_A$Loan == 0]
P_B <- Accepted / (Accepted + Rejected)
cat("B. P(Loan=1 | CC=1, Online=1) from pivot table =", round(P_B, 4),
    "(", Accepted, "of", Accepted + Rejected, "customers )\n")

# ============================================================
# C. Two separate pivot tables
# ============================================================
Loan_Online <- table(Loan = Train$Loan, Online = Train$Online)
Loan_Online

Loan_CC <- table(Loan = Train$Loan, CC = Train$CC)
Loan_CC

# ============================================================
# D. Compute the probabilities
# ============================================================
N_Loan1 <- sum(Train$Loan == 1)
N_Loan0 <- sum(Train$Loan == 0)

P1 <- Loan_CC["1", "1"] / N_Loan1        # i.   P(CC = 1 | Loan = 1)
P2 <- Loan_Online["1", "1"] / N_Loan1    # ii.  P(Online = 1 | Loan = 1)
P3 <- N_Loan1 / nrow(Train)              # iii. P(Loan = 1)
P4 <- Loan_CC["0", "1"] / N_Loan0        # iv.  P(CC = 1 | Loan = 0)
P5 <- Loan_Online["0", "1"] / N_Loan0    # v.   P(Online = 1 | Loan = 0)
P6 <- N_Loan0 / nrow(Train)              # vi.  P(Loan = 0)

data.frame(Quantity = c("i. P(CC=1 | Loan=1)", "ii. P(Online=1 | Loan=1)",
                        "iii. P(Loan=1)", "iv. P(CC=1 | Loan=0)",
                        "v. P(Online=1 | Loan=0)", "vi. P(Loan=0)"),
           Value = round(c(P1, P2, P3, P4, P5, P6), 4))

# ============================================================
# E. Naive Bayes P(Loan = 1 | CC = 1, Online = 1)
# ============================================================
P_E <- (P1 * P2 * P3) / ((P1 * P2 * P3) + (P4 * P5 * P6))
cat("E. Naive Bayes P(Loan=1 | CC=1, Online=1) =", round(P_E, 4), "\n")

# ============================================================
# F. Compare with (B)
# ============================================================
c(Pivot_Table_B = P_B, Naive_Bayes_E = P_E)
# Answer: The two values are close, but (B) is the more accurate estimate.
# It is the exact conditional probability, counted directly from customers
# who have both CC = 1 and Online = 1. Naive Bayes assumes CC and Online are
# independent given Loan, which is only approximately true.


# ============================================================
# G. Run Naive Bayes on the training data
# ============================================================
# Entries needed for P(Loan = 1 | CC = 1, Online = 1):
#  - the Loan = 1 and Loan = 0 counts in the CC = 1 column of Loan_CC
#  - the Loan = 1 and Loan = 0 counts in the Online = 1 column of Loan_Online
#  - the row totals for Loan = 1 and Loan = 0 (priors and denominators)

nb_model <- naiveBayes(Loan ~ CC + Online, data = Train)
nb_model
# A-priori probabilities = P(Loan = 0) and P(Loan = 1).
# In the CC table, row 1 / column 1 = P(CC = 1 | Loan = 1) and
# row 0 / column 1 = P(CC = 1 | Loan = 0). The Online table reads the same way.

# predictions on the training data, as probabilities
Predicted_Train <- predict(nb_model, Train, type = "raw")
head(Predicted_Train)

# the entry that corresponds to P(Loan = 1 | CC = 1, Online = 1)
P_G <- Predicted_Train[Train$CC == 1 & Train$Online == 1, 2][1]
cat("G. Model P(Loan=1 | CC=1, Online=1) =", round(P_G, 4),
    "   By hand (E) =", round(P_E, 4), "\n")
# Answer: The model's value matches (E), because naiveBayes() uses the same formula with the same training-set proportions.