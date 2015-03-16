#IEMS 395 Text Mining
#Nick Paras


# Initialization ----------------------------------------------------------

#set new heap size for java
options(java.parameters = "-Xmx4000m")

library(tm)
#library(plyr)
library(openNLP)
require("NLP")
library(SnowballC)
library(sqldf)
#library(RWeka)
#install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")
#install.packages("openNLPmodels.es", repos = "http://datacube.wu.ac.at/", type = "source")
#install.packages("openNLPmodels.nl", repos = "http://datacube.wu.ac.at/", type = "source")
library(openNLPmodels.en)
library(openNLPmodels.es)
library(openNLPmodels.nl)
library(stringr)
library(rJava)
library(slam)


#initialize openNLP fcns to do part of speech / sentence tagging
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()


#Load the corpus
allFiles = DirSource("reformattedPostings","CP1252")
corp = Corpus(allFiles,readerControl = list(language="en"))

#Make an unprocessed corpus for sentence selection
corpSen = corp

# function to perform preprocessing on a corpus
preProc <- function(corpusObj) {
  corpusObj <- tm_map(corpusObj, removePunctuation)
  corpusObj <- tm_map(corpusObj, removeWords, c(stopwords("english")))
  corpusObj <- tm_map(corpusObj, stripWhitespace)
  corpusObj <- tm_map(corpusObj, stemDocument)
  return(corpusObj)
}

corp = preProc(corp)

#options(mc.cores=1)
#CustomTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))}
#dtm2 <- DocumentTermMatrix(corp, control = list(tokenize = CustomTokenizer))
#options(mc.cores=4)

#create documentTermMatrix
dtm = DocumentTermMatrix(corp)
#remove empty documents from corpus
dtmInd <- rollup(dtm, 2, na.rm=TRUE, FUN = sum)
rowTotals <- as.matrix(dtmInd) > 0
empty.rows <- dtm[!rowTotals, ]$dimnames[1][[1]]
corp = corp[!(names(corp) %in% empty.rows)]
corpSen = corpSen[!(names(corpSen) %in% empty.rows)]
#recreate DTM
dtm = DocumentTermMatrix(corp)



# Define Functions --------------------------------------------------------

# function to extract type from the query
getQueryType <- function(query) {
  if (grepl("CEO",query)) {
    qType = "person"
  } else if (grepl("bankrupt",query)) {
    qType = "organization"
  } else if (grepl("percent",query)) {
    qType = "percentage"
  } else if (grepl("GDP",query)) {
    qType = "gdp"
  } else {
    qType = "misc"
  }
  return(qType)
}

# function to extract keywords from the queries
getKeywords <- function(query) {
  
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
  #locTok = sapply(keywords, function(x) colnames(docTermMat)[which(tolower(x) %in% colnames(docTermMat))])
  locTok = as.character(c(unlist(locTok)))
  tokCol = c(which(colnames(docTermMat) %in% locTok))
  
  #find the documents that have at least 1 of they keywords/similar keyword matches
  docInd = apply(docTermMat[,tokCol],1,function(x) sum(x) > 0)
  return(docInd)
}

# function to score and sort documents by TF-IDF on keywords
scoreDocuments <- function(keywords, redDocTermMat) {
  #give SMART weightings
  tfidfMat = weightSMART(redDocTermMat,spec="apc")
  #tfidfMat = weightTfIdf(redDocTermMat,normalize=FALSE)
  
  #stem the keywords 
  keywords = wordStem(keywords)
  
  #find the tokens/columns of the DTM that contain the keywords
  locTok = sapply(keywords, function(x) colnames(redDocTermMat)[grepl(tolower(x),colnames(redDocTermMat))])
  locTok = as.character(c(unlist(locTok)))
  tokCol = c(which(colnames(redDocTermMat) %in% locTok))
  
  #compute tfidf score for each document
  docScore = apply(tfidfMat[,tokCol],1,function(x) sum(x) )
  docRank = sort(docScore,decreasing=TRUE)
  
  #return top ten scoring documents
  return(docRank[1:10])
}

# function to extract a vector of sentences from (a) document(s)
getSentences <- function(documents) {
  
  sentences = vector()
  
  for (r in 1:length(documents)) {
    
    # extract text from document
    text = documents[[r]]
    
    # Convert text to class String from package NLP
    text <- as.String(text)
    ## Need sentence and word token annotations.
    a2 <- annotate(text, list(sent_token_annotator))
    
    # Extract sentences
    sent <- text[a2]
    sent = as.character(sent)
    #prevent empty sentences, floor of 3 characters found by trial/error
    sent = sent[nchar(sent) > 3]
    sentences = c(sentences, sent)
  }
  
  # return sentences
  return(sentences)
  
}

# function to extract a vector of sentences from (a) document(s)
getSentenceMatches <- function(sentenceVector, keys) {
  
  #find the indices that have at least one match
  sentInd = sapply(sentenceVector, countMatches, keywords = keys)
  sentInd = sentInd > 0
  
  return(sentenceVector[sentInd])
  
}


# function to determine if there are any Named Entities of typeEnt in a sentence
isSentType <- function(text, typeEnt) {
  #text is a sentence (character)
  #typeEnt is  one of "location", "organization", "percentage", "person", "misc"
  
  if (typeEnt != "misc") {
    
    # Convert text to class String from package NLP
    text <- as.String(text)
    
    if ((typeEnt == "percentage") ){
      en_ann <- Maxent_Entity_Annotator(language = "en", kind = typeEnt, probs = FALSE,model = NULL)
      pipeline <- list(sent_token_annotator,word_token_annotator,en_ann)
    
    
    ## Need sentence and word token annotations.
    a2 <- annotate(text, pipeline)
    
    # Determine if there are any entities of this type in the sentence, return TRUE/FALSE
    
    .jcall("java/lang/System", method = "gc")
    
    return((("entity" %in% names(table(a2$type))) || grepl("percentage point",text)))# && grepl("GDP", text))
    } else if ((typeEnt == "person") || (typeEnt == "organization")) {
      return(grepl("[A-Z]+[a-z]*\ [A-Z]+[a-z]+[A-Z]*[a-z]*",text))
    } else {
      return(grepl("GDP",text) || grepl("gdp",text))
    }
  } else {
    return(TRUE)
  }
  
  # Extract entities
  #sent <- text[a2[a2$type == "entity"]]
  #sent = as.character(sent)
  #return(sent)
}


#function to remove sentences that are not of the correct type
pruneSent <- function(sentVec, typeEnt) {
  sentLog = sapply(sentVec, isSentType,typeEnt = typeEnt)
  sentVec = sentVec[sentLog]
  return(sentVec)
}

# function to make new processed corpus out of sentences
getSentCorp <- function(sentVec, preProc) {
  sentCorp = Corpus(VectorSource(sentVec),readerControl = list(language="en"))
  if (preProc) {sentCorp = preProc(sentCorp)}
  return(sentCorp)
}

countMatches <- function(sentence, keywords) {
  #stem the keywords 
  keywords = wordStem(tolower(keywords))
  #break up sentence into words
  sentWords = unlist(strsplit(sentence," "))
  sentWords = wordStem(tolower(c(sentWords)))
  matchCt = lapply(keywords,function(x,comp) x %in% comp ,comp=sentWords)
  return(sum(unlist(matchCt)))
}

countCont <- function(sentence, keywords) {
  #stem the keywords
  keywords = wordStem(tolower(keywords))
  #check if the keywords are in the sentence, take sum
  mts = lapply(keywords,grepl,x=sentence)
  return(sum(unlist(mts)))
}

# function to score sentences
scoreSentences <- function(keywords, docTermMat, cleanCorp) {
  
  #stem the keywords 
  keywords = wordStem(keywords)
    
  #give tfidf weightings
  tfidfMat = weightSMART(docTermMat,spec="apc")
  #tfidfMat = weightTfIdf(docTermMat,normalize = TRUE)
    
  #find the tokens/columns of the DTM that contain the keywords
  locTok = sapply(keywords, function(x) colnames(docTermMat)[grepl(tolower(x),colnames(docTermMat))])
  #locTok = sapply(keywords, function(x) colnames(docTermMat)[which(tolower(x) %in% colnames(docTermMat))])
  locTok = as.character(c(unlist(locTok)))
  tokCol = c(which(colnames(docTermMat) %in% locTok))
  
  #error checking, ensure docMatLen is numeric even if NULL fcn call result
  docMatLen = sapply(cleanCorp, countCont, keywords=keywords)
  if (!is.numeric(docMatLen)) {
    docMatLen = 1
  }
    
  #compute tfidf score for each document
  docScoreMatches = apply(tfidfMat[,tokCol],1,function(x) sum(x) )
  docScoreNonMatches = apply(tfidfMat[,-tokCol],1,function(x) sum(x) )
  docScore = 5 * docMatLen +  2 * docScoreMatches - docScoreNonMatches;
  docRank = sort(docScore,decreasing=TRUE)
  
  #return top ten scoring documents
  return(docRank[1:10])
}

# function to add additional keywords based on question type for more robust answers
augKey <- function(keys, type) {
  #initialize return variable
  finalKeys = keys
  
  if (type == "gdp") {
    finalKeys = c(finalKeys, "factors", "components", "%", "percent", "growth")
  } else if (type == "organization") {
    if (substr(keys[4],1,2)=="20") {
      prefix = paste(substr(keys[4],3,4),"1",sep="-")
      finalKeys = c(finalKeys, "case", prefix)
    } else {
      finalKeys = c(finalKeys, "case")
    }
  }
  
  return(finalKeys)
}


questionAnswer <- function(query) {
  #note corp and dtm are global
  keys = getKeywords(query)
  type = getQueryType(query)
  cat("\n")
  cat(c("Query Type:",type,"\n"))
  cat(c("Query Keywords:", paste(keys, sep=", ")),"\n")
  keys = augKey(keys,type)
  cat("Retrieving Documents\n")
  docs = getDocuments(keys,dtm)
  cat("Scoring Documents with SMART TF-IDF\n")
  topDocs = scoreDocuments(keys,DocumentTermMatrix(corp[docs]))
  cat("Retrieving Sentences\n")
  sents = getSentences(corpSen[names(corpSen) %in% names(topDocs)])
  sents = getSentenceMatches(sents,keys)
  cat("Pruning Sentences by Type\n")
  sents = pruneSent(sents, type)
  sentCorpProc = getSentCorp(sents,TRUE)
  sentCorp = getSentCorp(sents,FALSE)
  cat("Scoring Sentences\n")
  topSents = scoreSentences(keys,DocumentTermMatrix(sentCorpProc),sentCorp)
  bestAns = sents[as.numeric(names(topSents[which(is.na(topSents)==FALSE)]))]
  cat("The 10 best scoring answers (in descending order) are:\n")
  cat("\n")
  return(bestAns)
}



# Sample Queries ----------------------------------------------------------

# 3 sample CEO queries
questionAnswer("Who is the CEO of Apple?")
questionAnswer("Who is the CEO of Facebook?")
questionAnswer("Who it the CEO of Microsoft?")

# 3 sample bankruptcy queries
questionAnswer("Which companies went bankrupt in September 2008?")
questionAnswer("Which companies went bankrupt in October 2011?")
questionAnswer("Which companies went bankrupt in September 2014?")

# GDP Query
questionAnswer("What affects GDP?")

# 3 sample follow-up queries
questionAnswer("What percentage is associated with personal consumption?")
questionAnswer("What percentage is associated with exports?")
questionAnswer("What percentage is associated with federal defense spending?")

















