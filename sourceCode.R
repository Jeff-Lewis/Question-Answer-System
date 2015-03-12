#IEMS 395 Text Mining
#Nick Paras

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

# function to extract type from the query
getQueryType <- function(query) {
  if (grepl("CEO",query)) {
    qType = "person"
  } else if (grepl("bankrupt",query)) {
    qType = "organization"
  } else if (grepl("percentage",query)) {
    qType = "percentage"
  } else if (grepl("GDP",query)) {
    qType = "misc"
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
  #stem the keywords 
  keywords = wordStem(keywords)
  
  #find the tokens/columns of the DTM that contain the keywords
  locTok = sapply(keywords, function(x) colnames(redDocTermMat)[grepl(tolower(x),colnames(redDocTermMat))])
  #locTok = sapply(keywords, function(x) colnames(redDocTermMat)[which(tolower(x) %in% colnames(redDocTermMat))])
  locTok = as.character(c(unlist(locTok)))
  tokCol = c(which(colnames(redDocTermMat) %in% locTok))
  
  #compute tfidf score for each document
  docScore = apply(tfidfMat[,tokCol],1,function(x) sum(x) )
  docRank = sort(docScore,decreasing=TRUE)
  
  #return top ten scoring documents
  return(docRank[1:5])
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

# function to determine if there are any Named Entities of typeEnt in a sentence
isSentType <- function(text, typeEnt) {
  #text is a sentence (character)
  #typeEnt is  one of "location", "organization", "percentage", "person", "misc"
  
  if (typeEnt != "misc") {
    
    # Convert text to class String from package NLP
    text <- as.String(text)
    
    if (typeEnt == "percentage") {
      en_ann <- Maxent_Entity_Annotator(language = "en", kind = typeEnt, probs = FALSE,model = NULL)
      pipeline <- list(sent_token_annotator,word_token_annotator,en_ann)
    } else {
      en_ann <- Maxent_Entity_Annotator(language = "en", kind = typeEnt, probs = FALSE,model = NULL)
      #es_ann <- Maxent_Entity_Annotator(language = "es", kind = typeEnt, probs = FALSE,model = NULL)
      #nl_ann <- Maxent_Entity_Annotator(language = "nl", kind = typeEnt, probs = FALSE,model = NULL)
      #pipeline <- list(sent_token_annotator,word_token_annotator,en_ann,es_ann,nl_ann)
      pipeline <- list(sent_token_annotator,word_token_annotator,en_ann)
    }
    
    ## Need sentence and word token annotations.
    a2 <- annotate(text, pipeline)
    
    # Determine if there are any entities of this type in the sentence, return TRUE/FALSE
    
    .jcall("java/lang/System", method = "gc")
    
    return("entity" %in% names(table(a2$type)))
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
  keywords = wordStem(keywords)
  #break up sentence into words
  sentWords = unlist(strsplit(sentence," "))
  
  matchCt = lapply(keywords,function(x,comp) x %in% comp ,comp=sentWords)
  return(sum(unlist(matchCt)))
}

# function to score sentences
scoreSentences <- function(keywords, docTermMat, cleanCorp) {
  
  #stem the keywords 
  keywords = wordStem(keywords)
  
  #compute number of shared tokens
  #find tokens that are not count = 0 in each document
  #sentTerms = apply(docTermMat,1, function(x) colnames(x)[which(!x == 0)])
  
  
  #give tfidf weightings
  tfidfMat = weightSMART(docTermMat,spec="apc")
    
  #find the tokens/columns of the DTM that contain the keywords
  locTok = sapply(keywords, function(x) colnames(docTermMat)[grepl(tolower(x),colnames(docTermMat))])
  #locTok = sapply(keywords, function(x) colnames(docTermMat)[which(tolower(x) %in% colnames(docTermMat))])
  locTok = as.character(c(unlist(locTok)))
  tokCol = c(which(colnames(docTermMat) %in% locTok))
  
  docMatLen = length(tokCol)
  
  
  #compute tfidf score for each document
  docScoreMatches = apply(tfidfMat[,tokCol],1,function(x) sum(x) )
  docScoreNonMatches = apply(tfidfMat[,-tokCol],1,function(x) sum(x) )
  docScore = docMatLen * ( docScoreMatches - docScoreNonMatches);
  docRank = sort(docScore,decreasing=TRUE)
  
  #return top ten scoring documents
  return(docRank[1:20])
}




questionAnswer <- function(query) {
  keys = getKeywords(query)
  print(c("Query Keywords:",keys))
  print("Retrieving Documents")
  docs = getDocuments(keys,dtm)
  print("Scoring Documents with SMART TF-IDF")
  topDocs = scoreDocuments(keys,DocumentTermMatrix(corp[docs]))
  print("Retrieving Sentences")
  sents = getSentences(corpSen[names(corpSen) %in% names(topDocs)])
  print("Pruning Sentences by type")
  sents = pruneSent(sents, getQueryType(query))
  sentCorpProc = getSentCorp(sents,TRUE)
  sentCorp = getSentCorp(sents,FALSE)
  print("Scoring Sentences")
  topSents = scoreSentences(keys,DocumentTermMatrix(sentCorpProc),sentCorp)
  #bestAns = sentCorp[names(sentCorp) %in% names(topSents)[1]]
  bestAns = sentCorp[names(sentCorp) %in% names(topSents)]
  print("The best scoring answer is:")
  return(inspect(bestAns))
}

#test = corpSen[names(corpSen) %in% names(documents)]
#inspect(corp[names(corp) %in% names(documents)])
#inspect(corpSen[names(corpSen) %in% names(documents)])



