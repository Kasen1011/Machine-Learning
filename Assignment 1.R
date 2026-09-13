install.packages("ggplot2")
library(ggplot2)
data <- StudentsPerformance
quant_vars <- c("math.score", "reading.score", "writing.score")
cat_vars <- c("gender", "race.ethnicity", "parental.level.of.education",
"lunch", "test.preparation.course")
summary(data[, quant_vars])#Summaries
sapply(data[cat_vars], table)#Summaries
data$log_math_score <- log1p(data$math.score) #transformation
summary(data$log_math_score)
ggplot(data, aes(x = math.score)) +
  geom_histogram(binwidth = 5) #Plotted quantitative var
ggplot(data, aes(x = reading.score, y = writing.score)) +
  geom_point() #scatterplot