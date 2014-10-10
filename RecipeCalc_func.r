### Z--- Challenge: Recipe Calculator
# 2014-Jan-17 vwood
# 
# sets up all the functions that will be used in the calculator:
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
# note that this last one (displayRecipe) is where all the business logic takes place
#
#


library(reshape2)


### FUNCTIONS

# table-to-price-list: creates a database from the table from read.csv
tableToPriceList <- function (priceList) {
  # define isProduce: based on the category name
  priceList$isProduce <-
    grepl( pattern="^Produce$", 
           x=priceList$CategoryName,
           ignore.case = TRUE)
  # define isMeatPoultry: based on the category name
  priceList$isMeatPoultry <-
    grepl( pattern="^Meat/poultry$", 
           x=priceList$CategoryName,
           ignore.case = TRUE)
  # define isPantry: based on the category name
  priceList$isPantry <-
    grepl( pattern="^Pantry$", 
           x=priceList$CategoryName,
           ignore.case = TRUE
    )
  # define isWellness: if the word "organic" appeard anywhere in the description
  priceList$isWellness <-
    grepl( pattern="organic", 
           x=priceList$Description,
           ignore.case = TRUE
    )
  priceList$isTaxable <- !(priceList$isProduce)
  
  priceList$price <-
    as.numeric( 
      gsub( 
        pattern="\\s*\\$\\s*", 
        replacement="", 
        x=priceList$PriceText, 
        perl=TRUE 
      ) 
    )
  
  # return the answer
  priceList
}  

# fraction parser (based on http://stackoverflow.com/questions/10674992/convert-a-character-vector-of-mixed-numbers-fractions-and-integers-to-numeric)
# eg: converts the text "1 1/2" to the numeric 1.5
mixedToFloat <- function(x){
  is.integer  <- grepl("^\\d+$", x)
  is.fraction <- grepl("^\\d+\\/\\d+$", x)
  is.mixed    <- grepl("^\\d+ \\d+\\/\\d+$", x)
  stopifnot(all(is.integer | is.fraction | is.mixed))
  
  numbers <- strsplit(x, "[ /]")
  
  ifelse(is.integer,  as.numeric(sapply(numbers, `[`, 1)),
         ifelse(is.fraction, as.numeric(sapply(numbers, `[`, 1)) /
                  as.numeric(sapply(numbers, `[`, 2)),
                as.numeric(sapply(numbers, `[`, 1)) +
                  as.numeric(sapply(numbers, `[`, 2)) /
                  as.numeric(sapply(numbers, `[`, 3))))
}


# rounds the vector x up (away from zero) to the nearest multiple of round
# eg round(60,8) is 64
roundToFactor <- function( x, round ) { 
  if( round == 0 ) {
    x
  } else {    
    ceiling( abs(x) / abs(round) ) * abs(round) * sign(x)
  }
}


# quick & dirty way to develop my text mining Corpus from the ingredients$Description
myCorpus <- function( text ) {
  wordCount <- t( rowSums( table( melt( 
    data = strsplit( x=tolower(
      gsub( pattern="\\s+", replacement=" ", text)
    ), split="\\s+" ) 
  ) ) ) ) 
  uniqueWords <- colnames( wordCount )[ wordCount == 1 ]
  
  # return the unique words
  uniqueWords
}


# finds which ingredients are called for in each line of a recipe
# and attaches the data for that ingredients to the reciple line
# the main problem here is that something like "garlic clove" must 
# resolve to something like "a clove of organic garlic roasted under a full moon"
linkIngredients <- function(recipe, ingredients ) {
  
  # find the words unique to each ingredient
  uniqueWords <- myCorpus( ingredients$Description );
  
  # find which lines in the recipe descriptions contain which unique words
  recipeWordMatches <- sapply(
    X = uniqueWords, 
    FUN = function(w) { grepl( pattern=tolower(w), x=tolower(recipe$Description), fixed=TRUE ) }
  )
  
  ingredientsWordMatches <- sapply(
    X = uniqueWords, 
    FUN = function(w) { grepl( pattern=tolower(w), x=tolower(ingredients$Description), fixed=TRUE ) }
  )
  
  # find out which unique words match an ingredient to a line in a recipe
  counts <- matrix(nrow=dim(recipeWordMatches)[1], ncol =dim(ingredientsWordMatches)[1] )
  for( idxRw in 1:dim(recipeWordMatches)[1] )
    for( idxIw in 1:dim(ingredientsWordMatches)[1] )
      counts[ idxRw, idxIw ] = sum( recipeWordMatches[idxRw,] & ingredientsWordMatches[idxIw,] )
  
  # make an array of ingredients that match up to the lines in the recipe
  idxIngrediant <- apply(X=counts, MARGIN=1, which.max ) # using which.max, but should never be more than one
  recipeWithIngredients <- cbind( recipe, ingredients[ idxIngrediant, ] )
  
  recipeWithIngredients
}


# this is the business logic of the program
# performs calculations
# creates a list of text lines that will be the text output for the program
displayRecipe <- function( 
  recipe, ingredients, 
  recipeTitle = "Recipe", 
  subtotalRoundUp = 0.01, 
  salesTaxRate = 0.086, 
  salesRoundUp = 0.07, 
  wellnessDiscountRate = -0.05, 
  wellnessRoundUp = 0.01
) {
  
  # link each row in the recipe to an ingredient
  recipeWithIngredients <- 
    linkIngredients(recipe, ingredients )
  
  subtotal <- roundToFactor( 
    x = recipeWithIngredients$qty * recipeWithIngredients$price, 
    round = subtotalRoundUp )
  # TODO: clarify specification
  # I'm supposing here that the wellness discount does applies to something other than the price of the food, and therefore cannot reduce the sales tax
  wellnessSubtotal <- subtotal[recipeWithIngredients$isWellness]
  taxableSubtotal <- subtotal[recipeWithIngredients$isTaxable]
  
  # calculate wellness discount
  wellnessDiscount <- roundToFactor(
    x = sum(wellnessSubtotal) * wellnessDiscountRate,
    round = wellnessRoundUp 
  )
  # calculate tax
  tax <- roundToFactor(
    x = sum(taxableSubtotal) * salesTaxRate,
    round = salesRoundUp
  )
  # calculate total
  total <- sum(subtotal) + tax + wellnessDiscount
  
  # output
  sprintf("Tax = $%0.2f", tax) -> taxOutput
  if( wellnessDiscount < 0 ) {
    # the normal case, it will be a discount, and therefore negative
    sprintf("Discount = ($%0.2f)", -wellnessDiscount) -> discountOutput
  } else {
    # this only comes up if it's zero (or erroneously positive)
    sprintf("Discount = $%0.2f", wellnessDiscount) -> discountOutput
  }
  sprintf("Total = $%0.2f", total ) -> totalOutput
  rbind( recipeTitle, taxOutput, discountOutput, totalOutput )
}
