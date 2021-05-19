#' Climate shapes
#' 
#' These shapes contain worldwide climate classifications for a given period. 
#' The shapes are grouped according to their period. 
#' 
#' Historical/obeserved climate classification shapes are grouped in the 
#' `observed` datapackage. 
#' These shapes all originate from Rubel & Kottek 2010.
#' 
#' Future scenarios are dependant on several variables like pollution levels.
#' Rubel & Kottek 2010 and Beck et al. 2018 describe several scenarios for the 
#' possible future climates. 
#' Currently the `future` datapackage contains the 
#' following scenarios:
#' - A1FI: from Rubel & Kottek 2010. Quick economic and technological 
#' growth through intensive use of fossil fuel.
#' - Beck: from Beck et al. 2018. high emissions
#' 
#' Legends are stored in the `legend` datapackage
#' 
#' @format Each is a large spatialpolygonsdataframe with 2 variables
#' @examples 
#' observed
#' future
#' legend
"observed"
"future"
"legend"