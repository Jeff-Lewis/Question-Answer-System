# function to extract a vector of sentences from a document
getSentences <- function(text) {

  ## Need sentence and word token annotations.
  a2 <- annotate(text, list(sent_token_annotator))

  for (ind in 1:length(a2))  {
    if (ind == 1) {
      sent = substr(text, a2$start[ind], a2$end[ind])
    } else {
      sent = c(sent, substr(text, a2$start[ind], a2$end[ind]))
    }
  }
  return(sent)
}