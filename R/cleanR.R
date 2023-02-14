
# Creat the R files

namer <- function(df) {
  # Remove spaces, digits, and non-letter characters
  # Replace spaces with underscores
  # Convert to lowercase

  colnames(df) <- tolower(gsub("[^[:alpha:]]", "_", gsub("\\s", "_", colnames(df))))
  return(df)
}

dupliDrop <- function(df) {
  # Remove columns with the same names
  df[, !duplicated(colnames(df))]
}

dupliFind <- function(df) {
  # Show index number and names of columns with the same names
  duplicates <- which(duplicated(colnames(df)))
  if (length(duplicates) == 0) {
    print("No duplicate column names found")
  } else {
    for (i in duplicates) {
      print(paste("Index:", i, "Column name:", colnames(df)[i]))
    }
  }
}

missCount <- function(df) {
  # Tabulate the total number of missing rows for each column
  missing_count <- colSums(is.na(df))
  return(missing_count)
}

# Create the NAMESPACE file

#' Remove spaces, digits, and any non-letter character from column names
#'
#' @param df A data frame.
#' @return A data frame with modified column names.
#' @export
#' @examples
#' namer(mtcars)
namer <- function(df) {
  colnames(df) <- tolower(gsub("[^[:alpha:]]", "_", gsub("\\s", "_", colnames(df))))
  return(df)
}

#' Remove columns with the same names
#'
#' @param df A data frame.
#' @return A data frame without columns with the same names.
#' @export
#' @examples
#' dupliDrop(mtcars)
dupliDrop <- function(df) {
  df[, !duplicated(colnames(df))]
}

#' Show index number and names of columns with the same names
#'
#' @param df A data frame.
#' @return A message indicating whether duplicate columns were found and their index number and names if found.
#' @export
#' @examples
#' dupliFind(mtcars)
dupliFind <- function(df) {
  duplicates <- which(duplicated(colnames(df)))
  if (length(duplicates) == 0) {
    print("No duplicate column names found")
  } else {
    for (i in duplicates) {
      print(paste("Index:", i, "Column name:", colnames(df)[i]))
    }
  }
}

#' Tabulate the total number of missing rows for each column
#'
#'
