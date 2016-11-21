

check_encoding <- function(file, encoding = "UTF-8", min_confidence = .2, level = "stop", ...){
  guess <- guess_encoding(file, ...) %>%
    filter(confidence >= min_confidence)
  
  if (!(encoding %in% guess$encoding)){
    notice <- sprintf('Less than %#.2f confidence that encoding is %s for: %s', min_confidence, encoding, file)
    stop(notice)
  }
  else(
    invisible(file)
  )
}

