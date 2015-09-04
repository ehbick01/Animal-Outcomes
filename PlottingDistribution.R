# Create binomial distribution based on the predicted odds for euthanasia
x <- round(predLinear*100, digits = 0)
y <- dbinom(x, 500, 0.2)
plotit <- data.frame('x' = x, 'y' = y)

# Plot distribution
ggplot(plotit, aes(x = x)) +
  geom_histogram(aes(y = ..density..),
                 bindwidth = 0.5,
                 colour = 'Grey', fill = 'white') +
  geom_density(alpha=.2, fill="Dark Grey") +
  ggtitle('Odds of Being Euthanized for Dogs') +
  labs(x="Probability", y="Percent Euthanized") +
  theme_tufte(base_size = 10, base_family = 'MonoSans')

# Build quantile table for odds
probTable <- data.frame('First Quartile' = quantile(x, 0.25),
                        'Second Quartile' = quantile(x, 0.5),
                        'Third Quartile' = quantile(x, 0.75),
                        'Fourth Quartile' = quantile(x, 1))

# Plot distribution and shade area under specific probabilities
ggplot(plotit, aes(x = x)) +
  geom_histogram(aes(y = ..density..),
                 colour = 'Grey', fill = 'white') +
  geom_histogram(data = subset(plotit, x < quantile(x, 0.5)),
                 aes(x = x, y = ..density..),
                 colour = 'Grey', fill = 'Black') +
  ggtitle('Odds of Being Euthanized for Dogs') +
  labs(x="Probability", y="Percent Euthanized") +
  theme_tufte(base_size = 10, base_family = 'MonoSans') # Second histogram determines what probability to shade

# Wrap into function
shadeIt <- function(prob) {
  
  ggplot(plotit, aes(x = x)) +
    geom_histogram(aes(y = ..count..),
                   colour = 'Grey', fill = 'white') +
    geom_histogram(data = subset(plotit, x < prob),
                   aes(x = x, y = ..count..),
                   colour = 'Grey', fill = 'Black') +
    scale_y_continuous(labels = comma) +
    ggtitle('Distribution of Predicted Probability \nfor Dogs Being Euthanized') +
    labs(x="Predicted Probability of Being Euthanized", y="# of Times Each Probability Was Predicted") +
    theme_tufte(base_size = 10, base_family = 'MonoSans') # Second histogram determines what probability to shade

}