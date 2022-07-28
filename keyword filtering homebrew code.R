## --- gen attempt at keyword filtering app code ---

rm(list=ls())

# load sample data 
require(stringr)
nokia = readLines('https://raw.githubusercontent.com/sudhir-voleti/sample-data-sets/master/text%20analysis%20data/amazon%20nokia%20lumia%20reviews.txt')
nokia  =  str_replace_all(nokia, "<.*?>", "") # get rid of html junk 
str(nokia)

# create sent-tokenized base-DF
require(dplyr)
require(tidytext)
textdf1 = nokia %>% tibble(text = .) %>%
  mutate(docID = row_number()) %>%    # row_number() is v useful.    
  group_by(docID) %>%
  unnest_tokens(sents, text, token="sentences", to_lower=FALSE) %>%
  mutate(sentID = row_number()) %>%
  select(docID, sentID, sents)

head(textdf1)  # have textdf1 as a downloadable, sent_tokenized DF

# build sample wordlist
wl0 = c("battery", "screen", "camera", "music", "app", "apps", "android", "ghz", "lock", "covaxin"); wl0

# thin the sample wl
corpus_lower = tolower(nokia)
wl1 = NULL
for (word in wl0){
  if (sum(str_detect(corpus_lower, word)) > 0) {wl1 = c(wl1, word)} }
wl1 # use this wordlist

# build unit func for wl against one doc
doc_proc <- function(i0=1, corpus0, textdf1){
  
  doc0 = corpus0[i0] 
  doc00 = textdf1[(textdf1$docID == i0),]
  sent_ind = NULL
  
  for (i1 in 1:nrow(doc00)){ # outer loop
    sent0 = doc00$sents[i1]
    for (word in wl1){ if (str_detect(sent0, word)) {sent_ind = c(sent_ind, i1); break} }
  } # i1 loop ends
  
  sent_ind1 = unique(sent_ind)
  df00 = doc00[(doc00$sentID %in% sent_ind1),]
  
  # rollback extracted sents into doc
  doc_sub = NULL
  for (i1 in 1:nrow(df00)){
    doc_sub = paste(doc_sub, df00$sents[i1], sep=" ")
  }
  
  df01 = data.frame(docID=i0, filtered_sents=doc_sub)
  
  return(df01) } # func ends

# wrapper func 
wrapper_corpus <- function(corpus0, textdf1){
  
  list_dfs = vector(mode="list", length=max(textdf1$docID)) # use in wrapper func
  for (i0 in 1:max(textdf1$docID)){
    list_dfs[[i0]] = doc_proc(i0, corpus0, textdf1)   } # i0 loop ends
  
  out_df = bind_rows(list_dfs)
  return(out_df) } # func ends

system.time({ outdf1 = wrapper_corpus(nokia, textdf1) }) # 1s for nokia
head(outdf1)  # display and downloadable

## --- what if we want a k-sentence window around each filtered sent? ---




