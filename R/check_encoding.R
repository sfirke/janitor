#' @title Check if a file has a given encoding
#'
#' @description
#' Check whether a file has a given encoding. Encodings are complicated, 
#' and we cannot know for certain if a file has a certain enconding.
#' This function wrapes readr's \code{guess_encoding} to trigger an interruption 
#' if we have less than \code{min_confidence} that the file's encoding matches 
#' the supplied encoding. 
#' 
#' Note that \code{guess_encoding} is not perfect,
#' so this will not catch every encoding difference.
#' This function is intended to help you ensure that file 
#' encodings don't change when you know they are at risk
#' (e.g., someone opens your precious, clean csv file in Excel 
#' and saves it again, changing the encoding from UTF-8 to UTF-8BOM,
#' which is currently not supported by readr (see readr issue 500),
#' and to help you write tests when you find an unexpected encoding.
#' 
#' @param file the file (or string) to check for encoding
#' @param encoding quoted encoding that we want to check for
#' @param min_confidence interrupt if the probability of the encoding matching is below this threshold
#' @param level What should happen if there are duplicates. Can be "stop", "warning", "message" or "logical". Logical returns TRUE or FALSE. 
#' @param ...  additional arguements to \code{guess_encoding}
#' #' @return If level == "logical", TRUE/FALSE, otherwise the data frame is returned silently for further piping
#' @export
#' @examples
#' # Using examples from readr's locale
#' x <- "var1\nÉmigré cause célèbre déjà vu."
#' y <- stringi::stri_conv(x, "UTF-8", "Latin1")
#' Encoding(x)
#' Encoding(y)
#' 
#' # Check that x is utf-8
#' df <- x %>%
#'     check_encoding() %>%
#'     read_csv()
#' 
#' # ISO-8859-1 is another name for latin1
#' guess <- "ISO-8859-1"
#' df <- y %>%
#'     check_encoding(encoding = guess) %>%
#'     read_csv(locale = locale(encoding = guess))
#'     
#' # Note that ISO-8859-2 passes, too, even though it is not correct
#' guess <- "ISO-8859-1"
#' df <- y %>%
#'     check_encoding(encoding = guess) %>%
#'     read_csv(locale = locale(encoding = guess))
#'   


check_encoding <- function(file, encoding = "UTF-8", min_confidence = .2, level = "stop", ...){
  
  guess <- readr::guess_encoding(file, ...)
  
  # Print the tibble from guess_encoding
  print_guess <- paste(capture.output(print(guess)), collapse = "\n")
  
  # Check if the encoding isn't even in guess_encoding's suggestions
  if (sum(encoding %in% guess$encoding)==0){
    notice <- sprintf('%s is not a likely encoding for:\n\n%s\n\nLikely encodings:\n%s', encoding, file, print_guess)
    if (level == "logical"){
      return(FALSE)
    }
    else(
      do.call(level, list(notice))
    )
  }
  else{
    
    # Check if the encoding is below our threshold confidence
    confidence <- guess[guess$encoding == encoding , c("confidence")]
    if (confidence < min_confidence){
      notice <- sprintf('%#.2f confidence that %s is a possible encoding for\n\n%s\n\nPossible encodings:\n%s', confidence, encoding, file, print_guess)
      if (level == "logical"){
        return(FALSE)
      }
      else(
        do.call(level, list(notice))
      )
    }
    else{
      # Else passed; return true or the file (invisibly)
      if (level == "logical"){
        return(TRUE)
      }
      invisible(file)
    }
  }
}
  