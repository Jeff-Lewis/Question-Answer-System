#IEMS 395 Text Mining
#Nick Paras
library(tm)
#library(plyr)
library(openNLP)
require("NLP")
library(SnowballC)
#library(RWeka)

# function to initialize the openNLP annotation functions
initOpenNLP <- function() {
  #initialize openNLP fcns to do part of speech / sentence tagging
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
}


#Load the corpus
allFiles = DirSource("reformattedPostings","CP1252")
corp = Corpus(allFiles,readerControl = list(language="en"))

#Make an unprocessed corpus for sentence selection
corpSen = Corpus(allFiles,readerControl = list(language="en"))

# function to perform preprocessing on a corpus
preProc <- function(corpusObj) {
  corpusObj <- tm_map(corpusObj, removePunctuation)
  corpusObj <- tm_map(corpusObj, removeWords, stopwords("english"))
  corpusObj <- tm_map(corpusObj, stripWhitespace)
  corpusObj <- tm_map(corpusObj, stemDocument)
}


#options(mc.cores=1)
#CustomTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))}
#dtm2 <- DocumentTermMatrix(corp, control = list(tokenize = CustomTokenizer))
#options(mc.cores=4)

#create documentTermMatrix
dtm = DocumentTermMatrix(corp)

# function to extract type from the query
getQueryType <- function(query) {
  if (grepl("CEO",query)) {
    qType = "Query Type: CEO"
  } else if (grepl("bankrupt",query)) {
    qType = "Query Type: bankruptcy"
  } else if (grepl("GDP",query)) {
    qType = "Query Type: GDP"
  } else if (grepl("percentage",query)) {
    qType = "Query Type: percentage"
  } else {
    qType = "Invalid query, please try again"
  }
  return(qType)
}

# function to extract keywords from the queries
getKeywords <- function(query) {
  library(sqldf)
  keyPosDict = data.frame(tags=c("NNP", "NNPS", "NN", "NNS", "JJ", "JJR", "JJS", "CD"))
  ## Need sentence and word token annotations.
  a2 <- annotate(query, list(sent_token_annotator, word_token_annotator))
  a3 <- annotate(query, pos_tag_annotator, a2)
  a3w <- (subset(a3, type == "word"))
  a4 = data.frame(a3w)[,1:4]
  for (row in 1:nrow(a4)) {
    a4$tag[row] = data.frame(a3w)$features[[row]]$POS 
  }
  keys = sqldf('select * from a4 where tag in (select * from keyPosDict);')
  for (row in 1:nrow(keys)) {
    keys$token[row] = substr(query,keys$start[row],keys$end[row])
  }
  return(keys$token)
}

# function to extract documents with at least 1 keyword match
getDocuments <- function(keywords, docTermMat) {
  
  #stem the keywords 
  keywords = wordStem(keywords)
  
  #find the tokens/columns of the DTM that contain the keywords
  locTok = sapply(keywords, function(x) colnames(docTermMat)[grepl(tolower(x),colnames(docTermMat))])
  locTok = as.character(c(unlist(locTok)))
  tokCol = c(which(colnames(docTermMat) %in% locTok))
  
  #find the documents that have at least 1 of they keywords/similar keyword matches
  docInd = apply(docTermMat[,tokCol],1,function(x) sum(x) > 0)
  return(docInd)
}

# function to score and sort documents by TF-IDF on keywords
scoreDocuments <- function(keywords, redDocTermMat) {
  
}

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

# function to make new corpus out of sentences
getSentCorp <- function(sentVec) {
  
}

