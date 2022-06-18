#CA_2_EPS_MELIKA_MINAEI_BIDGOLI_810198523
# FirstPart ------------------------------------------------------------------------------------------------
#1
countries_data <- read.csv("countries.csv", header = T, na.strings = c("", "<NA>"))

#3
main_correlation <- cor(x = countries_data[sapply(countries_data,is.numeric)],
            y = countries_data[sapply(countries_data,is.numeric)], use = "pairwise.complete.obs")

#4
correlaion_of_birthrate_and_agriculture <- cor(x = countries_data$Birthrate,
                                               y = countries_data$Agriculture,
                                               use = "pairwise.complete.obs")
#print(correlaion_of_birthdate_and_agriculture)
y = countries_data$Birthrate
x = countries_data$Agriculture
plot(x, y, 
     main = "Scatter plot of Agriculture and Population",
     xlab = "Agriculture", ylab = "Birthrate",
     pch = 19, frame = FALSE)
abline(lm(y ~ x), col = "blue")

#5
na_indexes <- which(is.na(x))
number = length(na_indexes)
for(i in 1 : number) {
  x[na_indexes[i]] = y[na_indexes[i]]
}

# SecondPart ------------------------------------------------------------------------------------------------
#1
Uniform_random_variable = runif(1, min = 0, max = 1)

#2
p = 0.6
Uniform_random_variable = runif(1, min = 0, max = 1)
# Depending on which range is considered a win ">" and "<" changes!
#Bernoulli_distribution = (Uniform_random_variable > p)
Bernoulli_distribution = (Uniform_random_variable < p)

#3/1
p = 0.6
n = 10
Uniform_random_variable = runif(n, min = 0, max = 1)
# Depending on which range is considered a win ">" and "<" changes!
#Binomial_variable = sum(Uniform_random_variable > p)
Binomial_variable = sum(Uniform_random_variable < p)

#3/2
number = 100
vector_of_random_vars <- c()
for(i in 1 : number) {
  p = 0.6
  # It was not vivid for me to use 1 or 10 for runif()
  #n = 1
  n = 10
  Uniform_random_variable = runif(n, min = 0, max = 1)
  # Depending on which range is considered a win ">" and "<" changes!
  #Binomial_variable = sum(Uniform_random_variable > p)
  Binomial_variable = sum(Uniform_random_variable < p)
  vector_of_random_vars[i] <- Binomial_variable
}
means = mean(vector_of_random_vars)
variance = var(vector_of_random_vars)

# ThirdPart ------------------------------------------------------------------------------------------------
#1
Uniform_random_variable = runif(n, min = 0, max = 1)
lambda = 1
Exponential_variable = (-1)/lambda * log(Uniform_random_variable)

#2
number = 1000
vector_of_random_vars <- c()
for(i in 1 : number) {
  Uniform_random_variable = runif(n, min = 0, max = 1)
  lambda = 1
  Exponential_variable = (-1)/lambda * log(Uniform_random_variable)
  vector_of_random_vars[i] <- Exponential_variable
}
hist(vector_of_random_vars, col = rainbow(10),
     xlab = "\"Number of variables (from 1 to 1000)\"", main = "\"Exponential random variables\"",
     ylab = "\"Values of Exponential random variables\"", cex.main = 1.5)


#plot(vector_of_random_vars, xlab = "\"Number of variables (from 1 to 1000)\"",
#     ylab = "\"Values of Exponential random variables\"", col = rainbow(10))
#barplot(vector_of_random_vars,
#       main = "\"Exponential random variables\"",
#       xlab = "\"Number of variables (from 1 to 1000)\"",
#       col = (rainbow(3)),
#       beside = T,
#       args.legend = list(title = "Countries", x = "topright", inset = c(-0.01, 0.02), cex = 0.5))