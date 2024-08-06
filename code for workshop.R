# Harman Jaggi 2023-2024

# install.packages("palmerpenguins")
# install.packages("tidytext")


# install.packages("palmerpenguins")
# install.packages("tidytext")
library(palmerpenguins)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(recipes)
library(ggpubr)
library(tidytext)
library(popdemo)
theme_set(theme_bw())



# Set working directory
getwd()
setwd("/Users/harmanjaggi/Documents/Research/ICCB workshop/")

# Throughout this lab, we'll be making use of the `%>%` operator.
# This is like a 'pipe', which takes whatever is on the left hand side and feeds
# it forward into a function. For example, below we are taking our data `d`,
# filtering it to only include Australia, and then printing the head (the first six rows).


head(penguins)

# check the data frame, how many columns, and what column names
penguins %>%
  dplyr::select(body_mass_g, ends_with("_mm")) %>%
  glimpse()


# Scatterplot example 1: penguin bill length versus bill dept
plot1 <- ggplot(data = penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(size = 3.5)+
  labs(x="Bill length in mm",
       y="Bill depth in mm",
       title="Plot for Penguin Bill length and Bill depth")+
  # stat_summary(fun.data=mean_cl_normal) +
  theme(axis.title.x = element_text(size=24,
                                    color="dark blue"),
        axis.title.y = element_text(size=24,
                                    color="dark blue"),
        title = element_text(size=20, face="bold"),
        legend.text = element_text(size=18))

plot1

# Scatterplot example 1: penguin bill length versus bill dept with linear fit line
plot2 <- ggplot(data = penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(size = 3.5)+
  # adding the linear fit line
  geom_smooth(method = "lm", fill = NA, size=2)+
  labs(x="Bill length in mm",
       y="Bill depth in mm",
       title="Plot for Penguin Bill length and Bill depth")+
  # stat_summary(fun.data=mean_cl_normal) +
  theme(axis.title.x = element_text(size=24,
                                    color="dark blue"),
        axis.title.y = element_text(size=24,
                                    color="dark blue"),
        title = element_text(size=20, face="bold"),
        legend.text = element_text(size=18))

plot2
# ggsave("plot1.png", plot1)

# Scatterplot example 1: penguin bill length versus bill dept
# grouped by different species
plot3 <- ggplot(data = penguins,
                aes(x = bill_length_mm, y = bill_depth_mm,
                group=species,
                color=species))+
  geom_point(size=3.5)+
  geom_smooth(method = "lm", fill = NA, size=2)+
  labs(x="Bill length in mm",
       y="Bill depth in mm",
       title="Plot for Penguin Bill length and Bill depth")+
  # stat_summary(fun.data=mean_cl_normal) +
  theme(axis.title.x = element_text(size=24,
                                    color="dark blue"),
        axis.title.y = element_text(size=24,
                                    color="dark blue"),
        title = element_text(size=20, face="bold"),
        legend.text = element_text(size=18))+
scale_color_manual(values = c("darkorange","darkorchid","cyan4"))

plot3

# ggsave("plot2.png", plot2)
# ggsave("plot3.png", plot2)

############################################################
# PCA
############################################################
# Let's simulate some data and do the pca from scratch!!
############################################################
# We'll then use a newer package recipe to carry out the pca!
############################################################

# Always set seed while simulating to make data repeatable

set.seed(1000)
# Random variable of size 50 and mean 50 with standard deviation 3
# Variable 1
var_1 <- rnorm(50, 50, sd = 3)

# Random variable of size 50 and mean scaled frpm the first variable with standard deviation 3
# Variable 2
var_2 <- 0.5*var_1 + rnorm(50, sd = sqrt(3))

# Both variables in a data frame
data_set_1 <- data.frame(var_1, var_2)
head(data_set_1)

# Find out the mean and variable of the two variables?

var(var_1)
var(var_2)

mean(var_1)
mean(var_2)

# A scatter plot with the two simulated variables
ggplot(data_set_1, aes(x = var_1, y = var_2)) +
  geom_point(color = "blue", size = 2) +
  xlab("Variable 1") +
  ylab("Variable 2") +
  theme_classic()

# Let's center the variables, that is
# Subtract the mean from each variable
data_set_1 <- data_set_1 %>%
  mutate(varc_1 = var_1 - mean(var_1),
         varc_2 = var_2 - mean(var_2))

head(data_set_1)

# Scatter plot for the centered data
ggplot(data_set_1, aes(x = varc_1, y = varc_2)) +
  geom_point(color = "blue", size = 2) +
  geom_vline(xintercept = 0, size = .5) +
  geom_hline(yintercept = 0, size = .5) +
  theme_bw()

# Now calculate Variance Covariance Matrix:
# Select just the centered variables
data_set_2 <- data_set_1 %>%
  select(varc_1, varc_2) %>%
  as.matrix()

# Calculate the covariance matrix
# Calculated as (X^T)X
cov_m <- (t(data_set_2) %*% data_set_2) / (nrow(data_set_2) - 1)
cov_m

# Use eigen() to obtain eigenvectors and eigenvalues
cov_e <- eigen(cov_m)

# Eigenvectors
e_vec <- cov_e$vectors

cov_e$vectors
cov_e$values

# Eigenvalues
e_val <- cov_e$values

# First eigenvector
ev_1 <- e_vec[,1]

# First eigenvector slope to get the direction along this vector
ev1_m <- ev_1[2] / ev_1[1]

# Second eigenvector
ev_2 <- e_vec[,2]

# Second eigenvector slope to get the direction along this vector
ev2_m <- ev_2[2] / ev_2[1]

# Scatter plot showing the span of both eigenvectors
ggplot(data.frame(data_set_2), aes(x = varc_1, y = varc_2)) +
  geom_point(color = "blue", size = 2) +
  geom_vline(xintercept = 0, size = .5) +
  geom_hline(yintercept = 0, size = .5) +
  geom_abline(slope = ev1_m, color = "blue", size = 0.7) +
  geom_abline(slope = ev2_m, color = "red", size = 0.7) +
  theme_classic()

# Multiply both eigenvectors and as expected these are orthogonal!! so their product is 0!
ev_1 %*% ev_2

# Calculate the estimated variance for each eigenvalue
e_var <- e_val / (nrow(data_set_2) - 1)

# Data frame with variance percentages
var_per <- data.frame(
  PC  = c("PC1", "PC2"),
  PER = c(e_var) * 100 / sum(e_var) # Calculate the percentage
)
var_per

# Scree plot
ggplot(var_per, aes(x = PC, y = PER)) +
  geom_col(width = 0.5, color = "black") +
  xlab("Principal component") +
  ylab("Percentage of variation (%)") +
  theme_classic()

# Norm or length of the first eigenvector
norm(as.matrix(ev_1), "F")

# Norm or lengthof the second eigenvector
norm(as.matrix(ev_2), "F")

# Data frame with both eigenvectors
loads <- data.frame(
  VAR   = c("var_1", "var_2"),
  PC1 = ev_1, # First eigenvecor
  PC2 = ev_2  # Second eigenvectors
)
loads

# Inverse of eigenvectors matrix
inv_evec <- solve(e_vec)

# Change the basis of the original data
# changing the projection of data along pc1 and pc2
data_set_3 <- data_set_2 %*% inv_evec

# Scatter showing the rotation
ggplot(data.frame(data_set_3), aes(X1, X2)) +
  geom_point(color = "blue", size = 2) +
  geom_vline(xintercept = 0, size = .5) +
  geom_hline(yintercept = 0, size = .5) +
  xlab("PC1 (78.8%)") +
  ylab("PC2 (21.2%)") +
  theme_classic()


# Scatter plot with the centered data
plot_data <- ggplot(data.frame(data_set_2), aes(x = varc_1, y = varc_2)) +
  geom_point(color = "blue", size = 2) +
  geom_vline(xintercept = 0, size = .5) +
  geom_hline(yintercept = 0, size = .5) +
  ylim(c(-8, 8.5)) +
  ggtitle("Original Data") +
  theme_classic()

# Scatter plot with the rotated data
plot_rotation <- ggplot(data.frame(data_set_3), aes(X1, X2)) +
  geom_point(color = "blue", size = 2) +
  geom_vline(xintercept = 0, size = .5) +
  geom_hline(yintercept = 0, size = .5) +
  xlab("PC1 (78.8%)") +
  ylab("PC2 (21.2%)") +
  ylim(c(-8, 8.5)) +
  ggtitle("Change of Basis to Eigenvectors") +
  theme_classic()

# Both graphs side by side
ggarrange(plot_data, plot_rotation)

# Data points just from PC 1
data_pc1 <- data.frame(v1 = data_set_3[,1], v2 = rep(0, nrow(data_set_3)))

# Scatter plot showing the projected points from PC1 (red points)
ggplot(data.frame(data_set_3), aes(X1, X2)) +
  geom_point(color = "blue", size = 2) +
  geom_point(data = data_pc1, aes(v1, v2), color = "red", size = 2) +
  geom_vline(xintercept = 0, size = .5) +
  geom_hline(yintercept = 0, size = .5) +
  xlab("PC1 (78.8%)") +
  ylab("PC2 (21.2%)") +
  ylim(c(-8, 8.5)) +
  theme_classic()


###################################################
# PCA with penguin data
###################################################
# this time we'll use the recipes packes and not calculate from scratch

# Variable names in penguin dataset
names(penguins)
penguins$species

# Check Correlations between different variables
penguins %>%
  select(species, body_mass_g,
         ends_with("_mm")) %>%
  GGally::ggpairs(aes(color = species),
                  columns = c("flipper_length_mm", "body_mass_g",
                              "bill_length_mm", "bill_depth_mm")) +
  scale_colour_manual(values = c("darkorange","purple","cyan4")) +
  scale_fill_manual(values = c("darkorange","purple","cyan4"))


flipper_hist <- ggplot(data = penguins, aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species),
                 alpha = 0.5,
                 position = "identity") +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  labs(x = "Flipper length (mm)",
       y = "Frequency",
       title = "Penguin flipper lengths")

flipper_hist

# You may consider recipes as an alternative method for
# creating and preprocessing design matrices (also known as model matrices)
# that can be used for modeling or visualization.

# Here's a link to learn more about recipes package later!
# https://www.tidymodels.org/start/recipes/
# turns out it is especially designed for machine learning algorithms in R

# The recipe() function as we used it here has two arguments:

  # A formula. Any variable on the left-hand side of the tilde (~) is considered the model outcome.
  # On the right-hand side of the tilde are the predictors.
  # Variables may be listed by name, or you can use the dot (.) to indicate all other variables as predictors.
  # The data. A recipe is associated with the data set used to create the model. (typically training data)
  # Naming a data set doesn’t actually change the data itself;
  # it is only used to catalog the names of the variables and their types, like factors, integers, dates, etc.
penguin_recipe <-
  recipe(~., data = penguins) %>%
  update_role(species, island, sex,
              year, new_role = "id") %>%
  step_naomit(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors(), id = "pca") %>%
  prep()

penguin_pca <-
  penguin_recipe %>%
  tidy(id = "pca")

penguin_pca

penguins %>%
  dplyr::select(body_mass_g, ends_with("_mm")) %>%
  tidyr::drop_na() %>%
  scale() %>%
  prcomp() %>%
  .$rotation

penguin_recipe %>%
  tidy(id = "pca", type = "variance") %>%
  dplyr::filter(terms == "percent variance") %>%
  ggplot(aes(x = component, y = value)) +
  geom_col(fill = "#b6dfe2") +
  xlim(c(0, 5)) +
  ylab("% of total variance")

penguin_pca %>%
  mutate(terms = tidytext::reorder_within(terms,
                                          abs(value),
                                          component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  tidytext::scale_y_reordered() +
  scale_fill_manual(values = c("#b6dfe2", "#0A537D")) +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  )


# get pca loadings into wider format
pca_wider <- penguin_pca %>%
  tidyr::pivot_wider(names_from = component, id_cols = terms)

# define arrow style
arrow_style <- arrow(length = unit(.05, "inches"),
                     type = "closed")

pca_plot <-
  juice(penguin_recipe) %>%
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = species, shape = species),
             alpha = 0.8,
             size = 2) +
  scale_colour_manual(values = c("darkorange","purple","cyan4"))

pca_plot +
  geom_segment(data = pca_wider,
               aes(xend = PC1, yend = PC2),
               x = 0,
               y = 0,
               arrow = arrow_style) +
  geom_text(data = pca_wider,
            aes(x = PC1, y = PC2, label = terms),
            hjust = 0,
            vjust = 1,
            size = 5,
            color = '#0A537D')

penguins %>%
  group_by(species) %>%
  summarize(across(c(flipper_length_mm, body_mass_g),
                   mean,
                   na.rm = TRUE))

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g, colour = species)) +
  geom_point() +
  scale_colour_manual(values = c("darkorange","purple","cyan4"))


# Life history stuff!
A <- matrix(c(0,5,10,
         0.3,0,0,
         0, 0.5, 0.2), nrow=3, ncol=3, byrow=T)

N0 <- as.vector(c(100,250,50))

time <- c(1:20)
N <- c()
ndat <- data.frame()
N <- N0
for (t in 1:length(time)) {

  N <- A %*% N
  dat <- data.frame(egg=N[1], juv=N[2], ad=N[3], total=sum(N))
  ndat <- rbind(ndat, dat)

}

ndat$prop.egg <- ndat$egg/ndat$total
ndat$prop.juv <- ndat$juv/ndat$total
ndat$prop.ad <- ndat$ad/ndat$total
ggplot(ndat)+
  geom_line(aes(x=time,y=prop.egg), color="red")+
  geom_line(aes(x=time,y=prop.juv), color="blue")+
  geom_line(aes(x=time,y=prop.ad), color="green")


require(dplyr)
require(ggplot2)
require(tidyverse)


A <- matrix(c(0, 5, 10,
              0.3, 0, 0,
              0, 0.5, 0.2 ), nr = 3, byrow = TRUE)

# set up the intial population vector
N0 <- matrix(c(100,250,50), ncol = 1)

years <- 20
N.projected <- matrix(0, nrow = nrow(A), ncol = years+1)
N.projected[, 1] <- N0

for (i in 1:years)
{
  N.projected[, i + 1] <- A %*% N.projected[,i]
}

### 1. How is the total population changing over time?

# get N in a form that's easier to ggplot
# make into a dataframe, name columns as years and add an age column
Ndf <- as.data.frame(N.projected)
colnames(Ndf) <- seq(1, to = years+1, by = 1)
Ndf <- cbind(stage = c("Offspring", "Juvenile", "Adult"), Ndf)
# View(Ndf)
# View(ndat)
# get in long format and then add proportion of population in each age group
Nlong <- Ndf %>%
  gather(year, population, -stage) %>%
  group_by(year) %>%
  mutate(proportion = population/sum(population),
         stage = as.factor(stage))

# View(Nlong)
# total population by year
tot_pop <- Nlong %>%
  group_by(year) %>%
  summarise(pop = sum(population)) %>% ungroup()

# plot total population over time
plot.tot <- ggplot(data = tot_pop,
                   aes(x = as.numeric(year),
                  y = pop, group = 1)) +
  geom_line(size=2, col="#009E73")+
  geom_point(size=3, col="#009E73") +
  xlab("Year") + ylab("N(t)")+
  ggtitle("Total population over time")+
  theme(axis.title.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        title = element_text(size=20, face="bold"),
        legend.text = element_text(size=18))

plot.tot
ggsave("plot.tot.png", plot.tot)

# Looks like the fish population is growing fast. Possibly exponentially?

  ### 2. What is the long-term equilibrium age structure and annual growth rate?
# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


cbcol <- c("#E69F00", "#009E73", "#CC79A7")
# plot proportion in each age group over time

plot.prop <- ggplot(data = Nlong, aes(x = as.numeric(year), y = proportion, group = stage, color = stage)) +
  geom_point(size=4) + geom_line(size=2)+
  xlab("Year") + ylab("N(t) for each stage")+
  ggtitle("Proportion of population in each stage")+
  scale_colour_manual(values=cbcol)+
  theme(axis.title.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        title = element_text(size=20, face="bold"),
        legend.text = element_text(size=18))

plot.prop
ggsave("plot.prop.png", plot.prop)

# Looks like the age-structure becomes stable after a few years! This is one of the key concepts of stable population theory - **fixed mortality and fertility rates produce a stable age structure over time.**

# Now, let's look at the growth rate. If we assume exponential growth (more on that during the workshop):

\( \textbf{N(t+1)} = \textbf{N (t)} e^{r}\) where \(r\) is the growth rate from projecting the population forward one year.

# let's plot using base R so we now how to do it with and without ggplot2

# plot r for each year
tot_pop <- tot_pop %>% arrange(as.numeric(year))
lambPop <- log(tot_pop$pop[2:(years+1)]/tot_pop$pop[1:years])
matplot(1:years, lambPop, type="b", pch=20, ylab = "r", xlab = "Time")


dat <- tibble(lamb=lambPop, time=years)

plot.lambda<- ggplot(dat, aes(x=1:years, y=lamb))+
  geom_line(size=2,col="#E69F00")+
  geom_point(size=3)+
  labs(x="Time",
       y="Growth rate")+
  theme(axis.title.x = element_text(size=24),
        axis.title.y = element_text(size=24),
        title = element_text(size=20, face="bold"),
        legend.text = element_text(size=18))

ggsave("plot.lambda.png", plot.lambda)

# Looks like in the long-run, the population grows annually at a constant rate, \(r\)!

### 3. What happens to the age structure and growth rate if we start with a different intial population and age distribution?


# Let's set up the experiment again, now with a different starting population.

# set up the example
#Leslie Matrix
A <- matrix(c(0, 5, 10, 0.3, 0, 0, 0, 0.5, 0.2 ), nr = 3, byrow = TRUE)

# set up the intial population vector
#N0 <- matrix(c(100,250,50), ncol = 1)
N0 <- matrix(c(50,75,300), ncol = 1) # changing this vector

years <- 20
N.projected <- matrix(0, nrow = nrow(A), ncol = years+1)
N.projected[, 1] <- N0

for (i in 1:years)
{
	N.projected[, i + 1] <- A %*% N.projected[,i]
}

# Now lets repeat what we did earlier.

# get N in a form that's easier to ggplot
# make into a dataframe, name columns as years and add an age column
Ndf <- as.data.frame(N.projected)
colnames(Ndf) <- seq(1, to = years+1, by = 1)
Ndf <- cbind(stage = c("Offspring", "Juvenile", "Adult"), Ndf)

# get in long format and then add proportion of population in each age group
Nlong <- Ndf %>%
  gather(year, population, -stage) %>%
  group_by(year) %>%
  mutate(proportion = population/sum(population),
         stage = as.factor(stage))


# total population by year
tot_pop <- Nlong %>%
  group_by(year) %>%
  summarise(pop = sum(population)) %>% ungroup()

# plot proportion in each age group over time
ggplot(data = Nlong, aes(x = as.numeric(year), y = proportion, group = stage, color = stage)) +
  geom_point() + geom_line()+
  xlab("Year") + ylab("N(t)")+
  ggtitle("Proportion of population in each age group over time")

# plot r for each year
tot_pop <- tot_pop %>% arrange(as.numeric(year))
lambPop<-log(tot_pop$pop[2:(years+1)]/tot_pop$pop[1:years])
matplot(1:years, lambPop, type="b", pch=1, ylab = "r", xlab = "Time")
```

# Wow! They look nearly identical. This is another key concept of stable population theory -
# **regardless of the characteristics of the starting population
# i.e. age structure and size, if constant mortality and fertility rates are applied
# for a long time, the population will eventually attain the characteristics
# (long-run \(r\) and stable age distribution) that are intrinsic to those rates.**

### 4. What happens to the age structure and growth rate if we multiply the 1st row by a constant like 2?
# change A
#Leslie Matrix
A <- matrix(c(0, 5*2, 10*2, 0.3, 0, 0, 0, 0.5, 0.2 ), nr = 3, byrow = TRUE)


# set up the intial population vector
N0 <- matrix(c(100,250,50), ncol = 1)

years <- 20
N.projected <- matrix(0, nrow = nrow(A), ncol = years+1)
N.projected[, 1] <- N0

for (i in 1:years)
{
  N.projected[, i + 1] <- A %*% N.projected[,i]
}

# get N in a form that's easier to ggplot
# make into a dataframe, name columns as years and add an age column
Ndf <- as.data.frame(N.projected)
colnames(Ndf) <- seq(1, to = years+1, by = 1)
Ndf <- cbind(stage = c("Offspring", "Juvenile", "Adult"), Ndf)

# get in long format and then add proportion of population in each age group
Nlong <- Ndf %>%
  gather(year, population, -stage) %>%
  group_by(year) %>%
  mutate(proportion = population/sum(population),
         stage = as.factor(stage))


# total population by year
tot_pop <- Nlong %>%
  group_by(year) %>%
  summarise(pop = sum(population)) %>% ungroup()

# plot total population over time
ggplot(data = tot_pop, aes(x = as.numeric(year), y = pop, group = 1)) +
  geom_point() + geom_line()+
  xlab("Year") + ylab("N(t)")+
  ggtitle("Total population over time")


# plot proportion in each age group over time
ggplot(data = Nlong, aes(x = as.numeric(year), y = proportion, group = stage, color = stage)) +
  geom_point() + geom_line()+
  xlab("Year") + ylab("N(t)")+
  ggtitle("Proportion of population in each age group over time")

# plot r for each year
tot_pop <- tot_pop %>% arrange(as.numeric(year))
lambPop<-log(tot_pop$pop[2:(years+1)]/tot_pop$pop[1:years])
matplot(1:years, lambPop, type="b", pch=1, ylab = "r", xlab = "Time")

# Looks like we have a higher proportion of Offspring (i.e. a 'younger' population),
# and a higher long-term growth rate.

### 5. What happens to the age structure and growth rate if you change the survival probabilities?
# change A
#Leslie Matrix
A <- matrix(c(0, 5, 10, 0.5*(0.3), 0, 0, 0, 0.5*(0.5), 0.5*(0.2) ), nr = 3, byrow = TRUE)


# set up the intial population vector
N0 <- matrix(c(100,250,50), ncol = 1)

years <- 20
N.projected <- matrix(0, nrow = nrow(A), ncol = years+1)
N.projected[, 1] <- N0

for (i in 1:years)
{
  N.projected[, i + 1] <- A %*% N.projected[,i]
}

# get N in a form that's easier to ggplot
# make into a dataframe, name columns as years and add an age column
Ndf <- as.data.frame(N.projected)
colnames(Ndf) <- seq(1, to = years+1, by = 1)
Ndf <- cbind(stage = c("Offspring", "Juvenile", "Adult"), Ndf)

# get in long format and then add proportion of population in each age group
Nlong <- Ndf %>%
  gather(year, population, -stage) %>%
  group_by(year) %>%
  mutate(proportion = population/sum(population),
         stage = as.factor(stage))


# total population by year
tot_pop <- Nlong %>%
  group_by(year) %>%
  summarise(pop = sum(population)) %>% ungroup()


# plot total population over time
ggplot(data = tot_pop, aes(x = as.numeric(year), y = pop, group = 1)) +
  geom_point() + geom_line()+
  xlab("Year") + ylab("N(t)")+
  ggtitle("Total population over time")

# plot proportion in each age group over time
ggplot(data = Nlong, aes(x = as.numeric(year), y = proportion, group = stage, color = stage)) +
  geom_point() + geom_line()+
  xlab("Year") + ylab("N(t)")+
  ggtitle("Proportion of population in each age group over time")

# plot r for each year
tot_pop <- tot_pop %>% arrange(as.numeric(year))
lambPop<-log(tot_pop$pop[2:(years+1)]/tot_pop$pop[1:years])
matplot(1:years, lambPop, type="b", pch=1, ylab = "r", xlab = "Time")

# Again, we see a younger population,
# but there's a dramatic drop in the growth rate when
# we reduce survivorship at all ages.
# You can further modify the Leslie matrix to answer questions such as:
# What happens when we only change survivorship from juveniles to adult?
# What about when we change just the survivorship from Offspring to juveniles?
# Does the adult survival rate have a strong influence on the population's future?
# Which transition is more important to the future of the population?

#Construction of a basic matrix population model and calculation of some demographic outputs
#Rob Salguero-Gomez - rob.salguero@zoo.ox.ac.uk

#First write in a vector that contains the transition probabilities and per-capita contributions from the life cycle you have drawn
#The one below is an example for a 4x4 MPM (i.e. four stages)
Avector <- c(0.5, 0, 2.9, 1.6,
             0.3, 0.4, 0, 0,
             0, 0.6, 0.5, 0,
             0, 0, 0.2, 0.3)

#Next form a matrix out of that vector. Note that in this case nrow = 4 because this is a 4x4 matrix, and that the vector is read by rows (it will start populating the matrix by rows rather than by columns)

A <- matrix(Avector, nrow=4, byrow=TRUE)

#Give names to your stages in the matrix A
dimnames(A)[[2]] <- 1:4

#Call the library Rage and plot the life cycle back out of your MPM:
library(Rage)
plotLifeCycle(A)

#Call the library popbio to calculate the rate of population growth:
library(popbio)
lambda(A)

#Is lambda >, =, or < than 1? That will tell you whether your imaginary population is expected to growth, stay at the same population size, or decline over time, under the assumption of density independence and environmental constancy.

# Deterministic models are those that have no density-dependence and the population matrix is constant: vital rates do not change overtime (timestep to timestep).
#
# For this exercise we will use the population matrix from the desert tortoise Gopherus agassizzii (See Doak et al. (1994) Ecol. Appl., 4, 446-460.); this population is found in the Mojave Desert, USA. The matrix has 8 age and size-based stages.
#
# Yearling (age 0-1)
# Juvenile 1 (<60 mm)
# Juvenile 2 (90-99mm)
# Immature 1 (100-139mm)
# Immature 2 (140-179mm)
# Subadult (180-207mm)
# Adult 1 (208-239mm)
# Adult 2 (>240mm)
# Add data

data(Tort)
Tort


