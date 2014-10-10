### Z--- Challenge: Recipe Calculator
# 2014-Jan-17 vwood
# sets data that will be used in the challenge
# (could read from a csv or other data source if preferred)
#
#
#


### data inputs (could read from a file if we were certain that the file would be in the current path)

# each subtotal gets rounded to the penny
subtotalRoundUp <- 0.01
# sales tax parameters
salesTaxRate <- 0.086
salesRoundUp <- 0.07
# wellness discount parameters
wellnessDiscountRate <- -0.05
wellnessRoundUp <- 0.01


# items database
price.List.Text <-
  "CategoryName, Description, PriceText
Produce, clove of organic garlic, $0.67
Produce, Lemon, $2.03
Produce, cup of corn, $0.87
Meat/poultry, chicken breast, $2.19
Meat/poultry, slice of bacon, $0.24
Pantry, ounce of pasta, $0.31
Pantry, cup of organic olive oil, $1.92
Pantry, cup of vinegar, $1.26
Pantry, teaspoon of salt, $0.16
Pantry, teaspoon of pepper, $0.17"


# Recipe 1
recipe1.Text <-
  "Quantity, Description
1, garlic clove
1, lemon
3/4, cup olive oil
3/4, teaspoons of salt
1/2, teaspoons of pepper"

recipe2.Text <-
  "Quantity, Description
1, garlic clove
4, chicken breasts
1/2, cup olive oil
1/2, cup vinegar"

recipe3.Text <-
  "Quantity, Description
1, garlic clove
4, cups of corn
4, slices of bacon
8, ounces of pasta
1/3, cup olive oil
1 1/4, teaspoons of salt
3/4, teaspoons of pepper"
