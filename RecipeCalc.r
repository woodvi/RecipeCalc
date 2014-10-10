#!/usr/bin/env Rscript

### Z--- Challenge: Recipe Calculator
# 2014-Jan-17 vwood
# 
# The intent of this assignment is to evaluate your design, development, and
# testing skills. Please approach it as if you were assigned it as part of your
# daily work. To complete it, you will need to provide a working application
# with tests written in any language you want. It's up to you to implement any
# mechanism you wish to feed data into the application and to display its
# results.
# 
# You're given a list of ingredients and recipes and asked to write an
# application that computes the following for each recipe:
#  Sales Tax (8.6% of the total price rounded up to the nearest 7 cents, applies to everything except produce)
#  Wellness Discount (-5% of the total price rounded up to the nearest cent, applies only to organic items)
#  Total Cost (should include the sales tax and the discount)
#
#
#
#


# set this to whatever path you are using
#setwd("~/Documents/Technical/dev/zynx");
source("RecipeCalc_data.r");
source("RecipeCalc_func.r");



### read the data

# read in the price list (from price.List.Text, defined in zynx_data.r, but could easily have been a text file)
ingredients <- tableToPriceList( 
  read.csv(
    text = price.List.Text,
    header = TRUE,
    as.is=TRUE
  )
)


# read in recipe 1
recipe1 <- read.csv( text = recipe1.Text, header = TRUE, as.is=TRUE )
recipe1$qty <- 
  mixedToFloat(recipe1$Quantity)

# read in recipe 2
recipe2 <- read.csv( text = recipe2.Text, header = TRUE, as.is=TRUE )
recipe2$qty <- 
  mixedToFloat(recipe2$Quantity)

# read in recipe 3
recipe3 <- read.csv( text = recipe3.Text, header = TRUE, as.is=TRUE )
recipe3$qty <- 
  mixedToFloat(recipe3$Quantity)



### calculate the recipes

# do recipe #1
output <- displayRecipe( 
  recipe1, ingredients, recipeTitle = "Recipe 1", 
  subtotalRoundUp, salesTaxRate, salesRoundUp, wellnessDiscountRate, wellnessRoundUp
)
writeLines(output)
writeLines("")

# do recipe #2
output <- displayRecipe( 
  recipe2, ingredients, recipeTitle = "Recipe 2", 
  subtotalRoundUp, salesTaxRate, salesRoundUp, wellnessDiscountRate, wellnessRoundUp
)
writeLines(output)
writeLines("")

# do recipe #3
output <- displayRecipe( 
  recipe3, ingredients, recipeTitle = "Recipe 3", 
  subtotalRoundUp, salesTaxRate, salesRoundUp, wellnessDiscountRate, wellnessRoundUp
)
writeLines(output)
writeLines("")

