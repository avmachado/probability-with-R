# Script in R with the solution to Week1 Lab.

# Loading the packages
library(ggplot2)
library(dplyr)

# Loading the data
data("arbuthnot")

# The Arbuthnot baptism counts for boys and girls. This data set has 82 observations
# and 3 variables.
# The Arbuthnot data set refers to Dr. John Arbuthnot, an 18 century physician, 
# writer, and mathematician. He was interested in the ratio of newborn boys to 
# newborn girls, so he gathered the baptism records for children born in London
# for every year from 1629 to 1710.

# View the data
View(arbuthnot)

# See the dimensions of the data frame
dim(arbuthnot)

# See the names of the columns (variables)
names(arbuthnot)

# Question 1: How many variables are included in this data set?
# Answer: 3 variables.

# View the column "boys" 
arbuthnot$boys

# Question 2: What command would you use to extract just the counts of girls born?
# Answer: arbuthnot$girls

# Plot of the number of girls baptised per year
pdf('scatterplot-girls-year.pdf')
ggplot(data = arbuthnot, aes(x = year, y = girls)) +
  geom_point()
dev.off()

# Question 3: which of the following best describes the number of girls baptised
# over the years included in this dataset?
# Answer: There is initially an increase in the number of girls baptised, which
# peaks around 1640. After 1640 there is a decrease in the number of girls
# baptised but the number begins to increase again in 1660. Overall the trend is
# an increase in the number of girls baptised.

# Using the dplyr package to add a new column in the data set with mutate function.
arbuthnot <- arbuthnot %>%
  mutate(total = boys + girls)

# Plot of the total number os baptisms per year
pdf('scatterplot-total-year.pdf')
ggplot(data = arbuthnot, aes(x = year, y = total)) +
  geom_point()
dev.off()

# Plot of the total number os baptisms per year using lines and points
pdf('scatterplot-total-year2.pdf')
ggplot(data = arbuthnot, aes(x = year, y = total)) +
  geom_point() +
  geom_line()
dev.off()

# Question 4: Calculate the total number of births for each year and store these
# values in a new variable called total in the present dataset. Then, calculate 
# the proportion of boys born each year and store these values in a new variable
# called prop_boys in the same dataset. Plot these values over time and based on
# the plot determine if the following statement is true or false: The proportion
# of boys born in the US has decreased over time.

# Creating the variables total and prop_boys
arbuthnot <- arbuthnot %>%
  mutate(total = boys + girls) %>%
  mutate(prop_boys = boys / total)

# Generating a plot of the proportion of boys over time
pdf('scatterplot-boys-year.pdf')
ggplot(data = arbuthnot, aes(x = year, y = prop_boys)) +
  geom_point() +
  geom_line()
dev.off()
# Answer: FALSE. The proportion of boys increase over time.

# Question 5: Create a new variable called more_boys which contains the value of
# either TRUE if that year had more boys than girls, or FALSE if that year did 
# not. Based on this variable which of the following statements is true? 
pdf('scatterplot-boys-more-than-girls.pdf')
ggplot(data = arbuthnot, aes(x = year, y = boys > girls)) +
  geom_line() +
  geom_point()
dev.off()
# The outcome shows that it is true that in every year the number of boys is greater
# than the number of girls

# We can see as well the proportion of boys over girls each year
pdf('scatterplot-proportion-boys-girls.pdf')
ggplot(data = arbuthnot, aes(x = year, y = boys / girls)) +
  geom_line() +
  geom_point()
dev.off()
# In every year the proportion was greater than 1, which means that the number of
# boys outcomes the number of girls
# Answer: Every year there are more boys born than girls.

# Question 6: In what year did we see the most total number of births in the U.S.?
arbuthnot %>%
  arrange(desc(total)) %>%
  select(total, year)
