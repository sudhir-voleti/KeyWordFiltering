#################################################
#               KeyWord Filtering               #
#################################################

require(dplyr)
require(tidytext)

shinyServer(function(input, output,session) {
  set.seed=1082022   
  
  dataset <- reactive({
    if (is.null(input$file)) {return(NULL)}
    else {
      
      if(file_ext(input$file$datapath)=="txt"){
        Document = readLines(input$file$datapath)
        #colnames(Document) <- c("Doc.id","Document")
        Doc.id=seq(1:length(Document))
        calib=data.frame(Doc.id,Document)
        print(input$file$name)
        return(calib)}
      else{
        Document = read.csv(input$file$datapath ,header=TRUE, sep = ",", stringsAsFactors = F)
        Document[,1] <- str_to_title(Document[,1])
        Document[,1] <- make.names(Document[,1], unique=TRUE)
        Document[,1] <- tolower(Document[,1])
        Document[,1] <- str_replace_all(Document[,1],"\\.","_")
        Document<-Document[complete.cases(Document), ]
        Document <- Document[!(duplicated(Document[,1])), ]
        rownames(Document) <- Document[,1]
        
        # colnames(Document) <- c("Doc.id","Document")
        #Doc.id=seq(1:length(Document))
        # calib=data.frame(Doc.id,Document)
        #print(input$file$name)
        
        return(Document)
      }
      
    }
  })
  
  cols <- reactive({colnames(dataset())})
  
  output$pre_proc1 <- renderUI({if(is.null(dataset())){
    return(NULL)
  }else{
    
    checkboxInput('html',"Remove HTML tags",value = TRUE)
    
  }
  })
  
  output$pre_proc2 <- renderUI({if(is.null(dataset())){
    return(NULL)
  }else{
    checkboxInput('num',"Remove Numbers",value = TRUE)
    
  }
  })
  
  
  y_col <- reactive({
    x <- match(input$x,cols())
    y_col <- cols()[-x]
    return(y_col)
    
  })
  
  output$id_var <- renderUI({
    print(cols())
    selectInput("x","Select ID Column",choices = cols())
  })
  
  
  output$doc_var <- renderUI({
    selectInput("y","Select Text Column",choices = y_col())
  })
  
  
  output$up_size <- renderPrint({
    size <- dim(dataset())
    paste0("Dimensions of uploaded data: ",size[1]," (rows) X ", size[2]," (Columns)")
  })
  
  text_summ <- reactive({summary(quanteda::corpus(dataset()[,input$y]))})
  quant_mod <- reactive({quanteda::corpus(dataset()[,input$y])})
  
  output$text <- renderUI({
    req(input$file$datapath)
    str1 <- paste("Total no of documents:", nrow(dataset()))
    str2 <- paste("Range of sentences per document: ",min(text_summ()$Sentences),"-",max(text_summ()$Sentences))
    #str3 <- paste("Maximum number of sentence: ",)
    str4 <- paste("Average number of sentences per document: ",round(mean(text_summ()$Sentences),2))
    HTML(paste(str1, str2,str4, sep = '<br/>'))
  })
  
  output$text2 <- renderUI({
    req(input$file$datapath)
    str2 <- paste("Range of words per document: ",min(text_summ()$Tokens),'-',max(text_summ()$Tokens))
    #str3 <- paste("range of words per document:: ",max(text_summ()$Tokens))
    str4 <- paste("Average number of words: ",round(mean(text_summ()$Tokens),2))
    HTML(paste(str2,str4, sep = '<br/>'))
  })
  output$samp_data <- DT::renderDataTable({
    DT::datatable(head(dataset()),rownames = FALSE)
  })
  
 
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
  
  
  textdf =  eventReactive(input$apply,{
    
    textb = dataset()[,input$y]
    ids = dataset()[,input$x]
    
    textdf1 = textb %>% tibble(text = .) %>%
      mutate(docID = row_number()) %>%    # row_number() is v useful.    
      group_by(docID) %>%
      unnest_tokens(sents, text, token="sentences", to_lower=FALSE) %>%
      mutate(sentID = row_number()) %>%
      select(docID, sentID, sents)
   
  })
  
  
  
  
  
  
  
  
  



  
  
  

  

  

  
  

  
  


  
  output$concordance = renderDataTable({
    
    # a0 = concordance.r(dataset()$Document,input$concord.word, input$window)
    # concordance = a0
    # datatable(concordance, escape = F, options = list(dom = "lt"))
    datatable(df_reactive(), escape = F, options = list(dom = "lt"))
  })
  

  


  
  
  

  
  output$downloadData1 <- downloadHandler(
    filename = function() { "Nokia_Lumia_reviews.txt" },
    content = function(file) {
      writeLines(readLines("data/Nokia_Lumia_reviews.txt"), file)
    }
  )
  
  output$downloadData2 <- downloadHandler(
    filename = function() {"OnePlus.txt"},
    content = function(file){
      writeLines(readLines("data/onePlus8T_reviews.txt.txt"),file)
    }
  )
  
  output$downloadData3 <- downloadHandler(
    filename = function() { "uber_reviews_itune.csv" },
    content = function(file) {
      write.csv(read.csv("data/uber_reviews_itune.csv"), file, row.names=F, col.names=F, fileEncoding = "UTF-8")
    }
  )
  
  output$downloadData4 <- downloadHandler(
    filename = function() { "airline_sentiment.csv" },
    content = function(file) {
      write.csv(read.csv("data/airline_sentiment.csv"), file, row.names=F, col.names=F, fileEncoding = "UTF-8")
    }
  )
  
})