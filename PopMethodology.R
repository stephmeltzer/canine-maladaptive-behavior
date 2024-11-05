# Load necessary libraries
# Install any missing packages using install.packages("package_name") if needed
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(dplyr)
library(lubridate)
library(psych)
library(treemap)

# Read in the dataset and view its structure
pop1 <- read_csv("/Users/StephanieMeltzer/Desktop/pop.csv")
glimpse(pop1)
summary(pop1)

# Convert relevant columns to factors
pop1 <- pop1 %>%
  mutate(
    breedid = as.factor(breedid),
    newgroup = as.factor(newgroup),
    sex = as.factor(sex),
    isneutered = as.factor(isneutered),
    whereacquired = as.factor(whereacquired),
    country = as.factor(country),
    aschild = as.factor(aschild),
    otherdogs = as.factor(otherdogs),
    healthproblems = as.factor(healthproblems),
    top5 = as.factor(top5)
  )
summary(pop1)

# Visualization 1: Bar plot of where pets were acquired
qplot(factor(whereacquired), data = pop1, geom = "bar",
      fill = I("skyblue"), xlab = "Where pet was acquired")

# Visualization 2: Boxplot for trainability by breed type
ggplot(pop1, aes(y = train, x = breedid, fill = breedid)) +
  geom_boxplot() +
  theme(
    plot.title = element_text(color = "black", size = 14, face = "bold.italic"),
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold")
  ) +
  labs(title = "Level of trainability by breed type", x = "Breed", y = "Trainability")

# Visualization 3: Boxplot for energy levels by breed type
ggplot(pop1, aes(y = energy, x = breedid, fill = breedid)) +
  geom_boxplot() +
  theme(
    plot.title = element_text(color = "black", size = 14, face = "bold.italic"),
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold")
  ) +
  labs(title = "Energy levels by breed type", x = "Breed", y = "Energy")

# Calculate mean energy by breed
by(pop1$energy, pop1$breedid, mean, na.rm = TRUE)

# Correlation analysis between numeric columns
cor(pop1 %>% select(where(is.numeric)), use = "complete.obs")

# Custom function to check correlations above a threshold
corr_check <- function(df, threshold) {
  matriz_cor <- cor(df %>% select(where(is.numeric)), use = "complete.obs")
  for (i in 1:nrow(matriz_cor)) {
    correlations <- which(abs(matriz_cor[i,]) > threshold & matriz_cor[i,] != 1)
    if (length(correlations) > 0) {
      lapply(correlations, FUN = function(x) cat(paste(colnames(df)[i], "with", colnames(df)[x]), "\n"))
    }
  }
}

# Run correlation check function with pop1 and threshold of 0.85
corr_check(pop1, 0.85)

# Conduct correlation test using psych package
ct <- corr.test(pop1 %>% select(where(is.numeric)), method = "pearson", adjust = "holm")

# ANOVA to test differences in trainability by breed
model <- lm(train ~ breedid, data = pop1)
ANOVA <- aov(model)
summary(ANOVA)

# Post hoc Tukey test for breed trainability differences
tukey_train <- TukeyHSD(aov(train ~ breedid, data = pop1))
print(tukey_train)

# Export Tukey test results to CSV
TK_data <- as.data.frame(tukey_train$breedid)
write.csv(TK_data, "TK_data.csv")

# Visualization: Tukey test results
psig <- as.numeric(apply(tukey_train$breedid[,2:3], 1, prod) >= 0) + 1
op <- par(mar = c(4.2, 15, 3.8, 2))
plot(tukey_train, col = psig, yaxt = "n")
for (j in 1:length(psig)) {
  axis(2, at = j, labels = rownames(tukey_train$breedid)[length(psig) - j + 1],
       las = 1, cex.axis = 0.8, col.axis = psig[length(psig) - j + 1])
}
par(op)

# Summary of trainability by breed
pop1 %>%
  group_by(breedid) %>%
  summarize(mean_train = mean(train, na.rm = TRUE)) %>%
  arrange(desc(mean_train))

# Linear model visualization of trainability by dog age across breeds
ggplot(pop1, aes(y = train, x = dogage, color = breedid)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black", formula = y ~ x) +
  facet_wrap(~breedid) +
  labs(title = "Trainability over time of Top 5 breeds", x = "Dog age", y = "Level of trainability") +
  theme(
    plot.title = element_text(color = "black", size = 14, face = "bold.italic"),
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold")
  )

# Boxplot for trainability by acquisition source and sex
ggplot(pop1, aes(y = train, x = whereacquired, fill = sex)) +
  geom_boxplot() +
  labs(title = "Trainability levels by acquisition source and sex", x = "Where Acquired", y = "Trainability") +
  theme(
    plot.title = element_text(color = "black", size = 14, face = "bold.italic"),
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold")
  )

# Treemap for average weight by breed and group (ensure "groups5" is defined or replace with pop1)
treepop1 <- pop1 %>%
  group_by(breedid) %>%
  summarize(weight = mean(weight, na.rm = TRUE)) %>%
  arrange(desc(weight))

treemap(treepop1,
        index = c("breedid"),
        vSize = "weight",
        type = "index",
        palette = "Set2",
        bg.labels = c("white"),
        align.labels = list(
          c("center", "center"),
          c("right", "bottom")
        )
)
