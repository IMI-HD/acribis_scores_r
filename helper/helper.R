format_barcelona_scores <- function(scores) {
  result <- c() # Initialize an empty vector to store formatted results
  
  # Recursive function to traverse the nested list
  format_item <- function(prefix, item) {
    if (is.list(item)) {
      # If the item is a list, call the function recursively
      for (name in names(item)) {
        format_item(paste0(prefix, "$", name), item[[name]])
      }
    } else {
      # Otherwise, format the item as "Name: Values"
      result <<- c(result, paste(prefix, ":", paste(item, collapse = ", ")))
    }
  }
  
  # Start formatting from the top-level list
  for (name in names(scores)) {
    format_item(name, scores[[name]])
  }
  
  # Return the formatted results as a single string with line breaks
  paste(result, collapse = "\n")
}