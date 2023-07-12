#---- FUNCTIONS ####

GB_classif <- function(x) {
  gb <- name_backbone(x)
  
  classif <- c(
    phylum = if (length(gb$phylum) > 0) gb$phylum else NA,
    class = if (length(gb$class) > 0) gb$class else NA,
    order = if (length(gb$order) > 0) gb$order else NA,
    family = if (length(gb$family) > 0) gb$family else NA,
    genus = if (length(gb$genus) > 0) gb$genus else NA,
    species = if (length(gb$species) > 0) gb$species else NA,
    x
  )
  
  return(classif)
}


NCBI_classif <- function(x, db) {
  gb <- try(classification(x, db = db, rows = 1), silent = TRUE)
  
  if (class(gb) == "try-error") {
    classif <- c(NA, NA, NA, NA, NA, NA, as.character(x))
  } else {
    rows <- length(gb[[1]])
    classification_levels <- c("phylum", "class", "order", "family", "genus", "species")
    
    classif <- sapply(classification_levels, function(level) {
      if (rows == 1) {
        NA
      } else {
        value <- as.character(gb[[1]][, 1][gb[[1]][, 2] %in% c(level, toupper(level))])
        ifelse(length(value) > 0, value, NA)
      }
    })
    
    classif <- c(classif, as.character(x))
  }
  
  return(classif)
}
