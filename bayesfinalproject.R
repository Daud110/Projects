# Load the haven package
library(haven)


if(!require(bayesplot)){
  install.packages("bayesplot")
  library(bayesplot)
}


if(!require(cmdstanr)){
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  library(cmdstanr)
  install_cmdstan(cores = 2)
}

if(!require(corrplot)){
  install.packages("corrplot")
  library(corrplot)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(ggdist)){
  install.packages("ggdist")
  library(ggdist) # for stat_dotsinterval
}

if(!require(posterior)){
  install.packages("posterior")
  library(posterior)
}
library(brms)
library(plyr)
install.packages("car")

library(car)

library(gridExtra)

# Load the dataset
vlbw <- read.csv("vlbw.csv")

# Inspect the first few rows of the dataset
head(vlbw)

# Summary statistics
summary(vlbw)




data<- vlbw



cath <- data
cath <- na.omit(cath)


cath$pvh <- mapvalues(cath$pvh, from = c("absent", "possible", "definite"), to = c(0, 1, 2))
cath$ivh <- mapvalues(cath$ivh, from = c("absent", "possible", "definite"), to = c(0, 1, 2))
cath$ipe <- mapvalues(cath$ipe, from = c("absent", "possible", "definite"), to = c(0, 1, 2))

cath$sex <- mapvalues(cath$sex, from = c("male", "female"), to = c(0, 1))


head(cath)

str(cath[, c("birth", "exit", "hospstay", "lowph", "pltct", "bwt", "lol", "magsulf", "meth", "toc", "apg1", "vent", "pneumo", "pda", "cld", "pvh", "ivh", "ipe", "sex", "dead")])

# Convert 'sex', 'pvh', 'ivh', 'ipe' to numeric
cath$sex <- as.numeric(cath$sex)
cath$pvh <- as.numeric(cath$pvh)
cath$ivh <- as.numeric(cath$ivh)
cath$ipe <- as.numeric(cath$ipe)

#feature selection


numeric_data <- cath[, c("birth", "exit" ,"hospstay", "lowph", "pltct" , "bwt", "lol", "magsulf" , "meth", "toc", "apg1" ,"vent" ,"pneumo" ,"pda" ,"cld" ,"pvh", "ivh", "ipe","sex" ,"dead")]

# Calculate correlation matrix and plot it
corr_matrix <- cor(numeric_data, use = "complete.obs")
corrplot(corr_matrix, method = "circle")

# Calculate VIF for the numeric data
vif(lm(dead ~ ., data = numeric_data))


# Remove problematic variables and calculate VIF again
numeric_data_cleaned <- numeric_data %>%
  select(-c(birth, exit, hospstay))

vif(lm(dead ~ ., data = numeric_data_cleaned))

cor_matrix <- cor(numeric_data_cleaned, use = "complete.obs")
cor_dead <- cor_matrix["dead", ]
cor_dead_sorted <- sort(cor_dead, decreasing = TRUE)
print(cor_dead_sorted)




# Plot distribution of death by sex
ggplot(cath, aes(x = factor(sex), fill = factor(dead))) +
  geom_bar(position = "dodge") +
  labs(title = "Counts of Death by Sex", x = "Sex", y = "Frequency") +
  theme_minimal()

# Plot distribution of pneumonia
p2 <- ggplot(cath, aes(x = factor(pneumo))) +
  geom_bar(fill = "gray") +
  labs(title = "Counts of Pneumonia", x = "Pneumonia", y = "Frequency") +
  theme_minimal()

# Plot distribution of IVH
p3 <- ggplot(cath, aes(x = factor(ivh))) +
  geom_bar(fill = "gray") +
  labs(title = "Counts of IVH", x = "IVH", y = "Frequency") +
  theme_minimal()

# Plot distribution of pltct (Platelet Count)
p4 <- ggplot(cath, aes(x = pltct)) +
  geom_histogram(binwidth = 50, fill = "gray", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Platelet Count", x = "Platelet Count", y = "Frequency") +
  theme_minimal()

# Plot distribution of bwt (Birth Weight)
p5 <- ggplot(cath, aes(x = bwt)) +
  geom_histogram(binwidth = 100, fill = "gray", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Birth Weight", x = "Birth Weight (g)", y = "Frequency") +
  theme_minimal()

# Combine all plots into one plot
grid.arrange(p2, p3, p4, p5, ncol = 2)
# pneumo, ivh, pltct, bwt, 

priors = c(
  prior(normal(-0.3, 0.15), coef = "pltct"),
  prior(normal(-0.5, 0.25), coef = "bwt"),
  prior(beta(2, 8), coef = "pneumo"),
  prior(normal(0, 1), coef = "ivh")
)



# Fit the model
fit_sig <- brm(
  dead ~ pltct + bwt + pneumo + ivh,
  data = cath,
  family = bernoulli(),
  prior = priors,
  chains = 4,
  iter = 2000
)


# Summarize the model
summary(fit_sig)

priors_hierarchical <- c(
  prior(normal(0, 0.01), coef = "pltct"),
  prior(normal(-0.1, 0.05), coef = "bwt"),
  prior(beta(2, 8), coef = "pneumo"),
  prior(normal(0, 1), coef = "ivh"),
  prior(exponential(2), class = "sd", group = "sex")  # Weakly informative prior for random intercept SD
)

fit_sig_hier <- brm(
  dead ~ pltct + bwt + pneumo + ivh + (1 | sex),  # Random intercept grouped by 'sex'
  data = cath,
  family = bernoulli(),
  prior = priors_hierarchical,
  chains = 4,
  iter = 2000,
  control = list(adapt_delta = 0.95)  # Adjust to improve convergence if needed
)


# Check the hierarchical model fit summary
summary(fit_sig_hier)

# Posterior predictive checks and diagnostics
pp_check(fit_sig, type =  "bars")
pp_check(fit_sig_hier, type =  "bars")

pp_check(fit_sig_hier, type = "bars_grouped", group = "sex")

predicted_probs_pool <- posterior_predict(fit_sig)
mean_predicted_probs_pool <- rowMeans(predicted_probs_pool)
predicted_labels_pool <- ifelse(mean_predicted_probs_pool > 0.5, 1, 0)

accuracy_pool <- mean(predicted_labels_pool[1:length(cath$dead)] == cath$dead)

predicted_probs_hier <- posterior_predict(fit_sig_hier)
mean_predicted_probs_hier <- rowMeans(predicted_probs_hier)
predicted_labels_hier <- ifelse(mean_predicted_probs_hier > 0.5, 1, 0)

accuracy_hier <- mean(predicted_labels_hier[1:length(cath$dead)] == cath$dead)


loo_pool <- loo(fit_sig)
loo_pool
s = plot(loo_pool, label_points = TRUE)



loo_hier <- loo(fit_sig_hier)
loo_hier
s2 = plot(loo_hier, label_points = TRUE)

grid.arrange(s, s2)


loo_compare(loo_pool,loo_hier)

