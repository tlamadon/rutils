#' rename anything in the table (columns or cells)
#' you can specify a subset of columns to look at
#' if the selector is ambiguous.
#' note that you can have circular renaming, because the data is copied first
#' @export
renameany <-function(d,rules,cols=c()) {
  if (length(cols)==0) {
    cols=colnames(d)
  }

  # we use the original always
  dorig = d

  #2) try to rename the levels in the variadbles
  for ( n in names(rules)) {
    for ( c in cols) {
      
      # first check if the colum is string
      if ( is.character(dorig[1,c]) ) {
       # try to select all rows that have value n
       d[ dorig[,c] == n , c] = rules[n]
      }

      # for factors
      if (is.factor(dorig[,c])) {
        levels( d[,c]  )[levels(dorig[,c])==n] <- rules[n]
      }
    }
  }

  #1) we rename the columns
  d = rename(d,rules)


  return(d)
}



