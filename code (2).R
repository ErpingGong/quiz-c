set.seed(42) 

n_samples <- 1000

age_groups <- c('18-24', '25-34', '35-44', '45-54', '55-64', '65+')
genders <- c('Male', 'Female', 'Other')
income_groups <- c('Low', 'Medium', 'High')
education_levels <- c('High School', 'Bachelor', 'Master', 'PhD')

support <- sample(c(0, 1), size = n_samples, replace = TRUE)
age_group <- sample(age_groups, size = n_samples, replace = TRUE)
gender <- sample(genders, size = n_samples, replace = TRUE)
income_group <- sample(income_groups, size = n_samples, replace = TRUE)
highest_education <- sample(education_levels, size = n_samples, replace = TRUE)

simulated_data <- data.frame(Support = support,
                             AgeGroup = age_group,
                             Gender = gender,
                             IncomeGroup = income_group,
                             HighestEducation = highest_education)

library(ggplot2)

ggplot(simulated_data, aes(x = AgeGroup, fill = as.factor(Support))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "red", "1" = "green"), name = "Support") +
  labs(title = "Support for Political Party by Age Group",
       x = "Age Group",
       y = "Proportion") +
  theme_minimal()

library(rstanarm)


simulated_data$Gender <- as.factor(simulated_data$Gender)
simulated_data$IncomeGroup <- as.factor(simulated_data$IncomeGroup)
simulated_data$HighestEducation <- as.factor(simulated_data$HighestEducation)


model <- stan_glm(Support ~ AgeGroup + Gender + IncomeGroup + HighestEducation, 
                  data = simulated_data, 
                  family = binomial(link = "logit"))

print(summary(model))
