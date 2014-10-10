#!/usr/bin/env Rscript

### Z--- Challenge: Recipe Calculator
# 2014-Jan-17 vwood
#
# runs tests
#
#   tableToPriceList(priceList)
#   mixedToFloat(x)
#   roundToFactor( x, round )
#   myCorpus( text )
#   linkIngredients(recipe, ingredients )
#   displayRecipe( 
#     recipe, ingredients, 
#     recipeTitle = "Recipe", 
#     subtotalRoundUp = 0.01, 
#     salesTaxRate = 0.086, 
#     salesRoundUp = 0.07, 
#     wellnessDiscountRate = -0.05, 
#     wellnessRoundUp = 0.01 )
#
# 2014-Jan-20 vw:   uses svUnit  (like RUnit, an xUnit compatible framework, allegedly has better GUI & is more compatible with Hudson/Jenkins)
#
#

library( svUnit )

# set this to whatever path you are using
setwd("~/Documents/Technical/dev/RecipeCalc/unitTests");
source("../RecipeCalc_func.r"); # assumes that it's being run from the unitTests project subdirectory



# unit test for fraction parser
test( mixedToFloat ) <- function() {
  checkEqualsNumeric(
    target=mixedToFloat(c('1 1/2', '2 3/4', '2/3', '11 1/4', '1')),
    current=c( 1.5, 2.75, 0.6666667, 11.25, 1 ),
    msg="mixedToFloat",
    tolerance=1.0e-6)
  
}


# unit tests for roundToFactor
test( roundToFactor ) <- function() {
  checkTrue( all(
    # ordinary integers
    roundToFactor( 8, 2 ) == 8,
    roundToFactor( 9, 2 ) == 10, 
    roundToFactor( 10, 2 ) == 10,
    roundToFactor( 8, 3 ) == 9,
    roundToFactor( 9, 3 ) == 9, 
    roundToFactor( 10, 3 ) == 12, 
    # fractions
    roundToFactor( 0.08, 0.02 ) == 0.08,
    roundToFactor( 0.09, 0.02 ) == 0.10, 
    roundToFactor( 0.10, 0.02 ) == 0.10,
    roundToFactor( 0.08, 0.03 ) == 0.09,
    # zero, negative numbers
    roundToFactor( 9, 0 ) == 9, 
    roundToFactor( -9, 4 ) == -12, 
    roundToFactor( 9, -2 ) == 10, 
    roundToFactor( 0, 2 ) == 0
  ), msg = "roundToFactor"
  )
}


# unit tests for myCorpus
test( myCorpus ) <- function() {
  checkEquals( 
    target = c("x","y","z"), 
    current = myCorpus( c(rep("a",5), "x", rep("b",5), "y", rep("c",5), "z", rep("a",5) ) ) ,
    msg = "myCorpus1" )
  
  checkEquals( 
    target =c( "a", "x" ),
    current = myCorpus( c("a","x") ),
    msg = "myCorpus2" )
  
  checkTrue(     
    is.null( myCorpus( "" ) ),
    msg = "myCorpus3" )
}



# items database
test( tableToPriceList ) <- function() {
  
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
  
  ingredients <- tableToPriceList( 
    read.csv(
      text = price.List.Text,
      header = TRUE,
      as.is=TRUE
    )
  )
  
  
  checkEquals( 
    target=c( " clove of organic garlic", " Lemon", " cup of corn", " chicken breast", " slice of bacon", " ounce of pasta", " cup of organic olive oil", " cup of vinegar", " teaspoon of salt", " teaspoon of pepper" ),   
    current = ( ingredients$Description ),
    msg="tableToPriceList1")
  
  checkEquals( 
    target=c( FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE ),
    current = ( ingredients$isTaxable ),
    msg="tableToPriceList2")
  
  checkEquals( 
    target=c( TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE,FALSE ),
    current = ( ingredients$isWellness ),
    msg="tableToPriceList3")
}



# tests the mixedToFloat function on data it reads from text
test_mixedToFloat <- svTest( function() {
  # read in recipe 1
  # Recipe 1
  recipe1.Text <-
"Quantity, Description
1, garlic clove
1, lemon
3/4, cup olive oil
3/4, teaspoons of salt
1/2, teaspoons of pepper"
  recipe1 <- read.csv( text = recipe1.Text, header = TRUE, as.is=TRUE )
  recipe1$qty <- 
    mixedToFloat(recipe1$Quantity)
  
  # read in recipe 2
  recipe2.Text <-
"Quantity, Description
1, garlic clove
4, chicken breasts
1/2, cup olive oil
1/2, cup vinegar"
  recipe2 <- read.csv( text = recipe2.Text, header = TRUE, as.is=TRUE )
  recipe2$qty <- 
    mixedToFloat(recipe2$Quantity)
  
  
  # read in recipe 3
  recipe3.Text <-
"Quantity, Description
1, garlic clove
4, cups of corn
4, slices of bacon
8, ounces of pasta
1/3, cup olive oil
1 1/4, teaspoons of salt
3/4, teaspoons of pepper"
  recipe3 <- read.csv( text = recipe3.Text, header = TRUE, as.is=TRUE )
  recipe3$qty <- 
    mixedToFloat(recipe3$Quantity)
  
  checkEqualsNumeric(
    c(1,1,0.75,0.75,0.5),
    recipe1$qty, 
    msg = "mixedToFloatB1", 
    tolerance = 1.0e-6 )
  
  checkEqualsNumeric(
    c(1,4,0.5,0.5),
    recipe2$qty, 
    msg = "mixedToFloatB2", 
    tolerance = 1.0e-6 )
  
  checkEqualsNumeric(
    c(1,4,4,8,1/3,1.25,0.75),
    recipe3$qty, 
    msg = "mixedToFloatB3", 
    tolerance = 1.0e-6 )
  
})




test(linkIngredients) <- function() {

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
  
  ingredients <- tableToPriceList( 
    read.csv(
      text = price.List.Text,
      header = TRUE,
      as.is=TRUE
    )
  )
  
  recipe1 <- data.frame(
    Quantity = c( "1", "1", "3/4", "3/4", "1/2" ),
    Description = c("garlic clove", "lemon", "cup olive oil", "teaspoons of salt", "teaspoons of pepper"),
    qty = c(1.00, 1.00, 0.75, 0.75, 0.50 )
  )
  
  recipe2 <- data.frame(
    Quantity = c("1", "4", "1/2", "1/2" ),
    Description = c(" garlic clove", " chicken breasts", " cup olive oil", " cup vinegar" ), 
    qty = c(1.0, 4.0, 0.5, 0.5 )
  )
  
  recipe3 <- data.frame(
    Quantity = c("1", "4", "4", "8", "1/3", "1 1/4", "3/4"),
    Description = c(" garlic clove"," cups of corn", " slices of bacon", " ounces of pasta"," cup olive oil", " teaspoons of salt", " teaspoons of pepper"), 
    qty = c(1.0, 4.0, 4.0, 8.0, 1/3, 1.25, 0.75)
  )
  
  
  
  
  recipe1 <- linkIngredients(recipe1, ingredients )
  recipe2 <- linkIngredients(recipe2, ingredients )
  recipe3 <- linkIngredients(recipe3, ingredients )
  
  checkEqualsNumeric(
    target=c( 0.67, 2.03, 1.92, 0.16, 0.17 ),
    current=recipe1$price,
    msg="linkIngredients1", 
    tolerance=1.0e-6)
  
  checkEqualsNumeric(
    target= c( 0.67, 2.19, 1.92, 1.26 ),
    current=recipe2$price,
    msg="linkIngredients2", 
    tolerance=1.0e-6)
  
  checkEqualsNumeric(
    target= c( 0.67, 0.87, 0.24, 0.31, 1.92, 0.16, 0.17 ),
    current=recipe3$price,
    msg="linkIngredients3", 
    tolerance=1.0e-6)
  
  #  (tolerance) > (recipe2$price - c( 0.67, 2.19, 1.92, 1.26 )),
  #  (tolerance) > (recipe3$price - c( 0.67, 0.87, 0.24, 0.31, 1.92, 0.16, 0.17 ))
  #  )
}  



test(displayRecipe) <- function() {
  
  
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
  
  ingredients <- tableToPriceList( 
    read.csv(
      text = price.List.Text,
      header = TRUE,
      as.is=TRUE
    )
  )
  
  recipe1 <- data.frame(
    Quantity = c( "1", "1", "3/4", "3/4", "1/2" ),
    Description = c("garlic clove", "lemon", "cup olive oil", "teaspoons of salt", "teaspoons of pepper"),
    qty = c(1.00, 1.00, 0.75, 0.75, 0.50 )
  )
  
  recipe2 <- data.frame(
    Quantity = c("1", "4", "1/2", "1/2" ),
    Description = c(" garlic clove", " chicken breasts", " cup olive oil", " cup vinegar" ), 
    qty = c(1.0, 4.0, 0.5, 0.5 )
  )
  
  recipe3 <- data.frame(
    Quantity = c("1", "4", "4", "8", "1/3", "1 1/4", "3/4"),
    Description = c(" garlic clove"," cups of corn", " slices of bacon", " ounces of pasta"," cup olive oil", " teaspoons of salt", " teaspoons of pepper"), 
    qty = c(1.0, 4.0, 4.0, 8.0, 1/3, 1.25, 0.75)
  )
  
  
  # do recipe #1
  output1 <- displayRecipe( 
    recipe1, ingredients, recipeTitle = "Recipe 1", 
    subtotalRoundUp = 0.01, salesTaxRate = 0.086, salesRoundUp = 0.07, wellnessDiscountRate = -0.05, wellnessRoundUp = 0.01
    #subtotalRoundUp, salesTaxRate, salesRoundUp, wellnessDiscountRate, wellnessRoundUp
  )
  checkTrue(
    output1 == list("Recipe 1","Tax = $0.21","Discount = ($0.11)","Total = $4.45"),
    msg = "displayRecipe1")
  
  # do recipe #2
  output2 <- displayRecipe( 
    recipe2, ingredients, recipeTitle = "Recipe 2", 
    subtotalRoundUp = 0.01, salesTaxRate = 0.086, salesRoundUp = 0.07, wellnessDiscountRate = -0.05, wellnessRoundUp = 0.01
  )
  checkTrue(
    output2 == c("Recipe 2","Tax = $0.91","Discount = ($0.09)","Total = $11.84"),
    msg = "displayRecipe2")
  
  # do recipe #3
  output3 <- displayRecipe( 
    recipe3, ingredients, recipeTitle = "Recipe 3", 
    subtotalRoundUp = 0.01, salesTaxRate = 0.086, salesRoundUp = 0.07, wellnessDiscountRate = -0.05, wellnessRoundUp = 0.01
  )
  checkTrue(
    output3 == c("Recipe 3","Tax = $0.42","Discount = ($0.07)","Total = $8.91"),
    msg = "displayRecipe3")
  
}






clearLog()
res <- runTest( mixedToFloat )
res <- runTest( roundToFactor )
res <- runTest( myCorpus ) 
res <- runTest( tableToPriceList )
res <- runTest(test_mixedToFloat)
res <- runTest(linkIngredients)
res <- runTest(displayRecipe)
stats(Log())

