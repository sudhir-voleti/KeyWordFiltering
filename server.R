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
        return(calib)} else if(file_ext(input$file$datapath)=="pdf")
      {          
        pdf_text0 <- pdftools::pdf_text(input$file$datapath)                
        pdf_text1 <- str_replace_all(pdf_text0, 
                                     pattern = "([.!?])\n(\\w)", 
                                     replacement = "\\1\n\n\\2") 
  
        # Collapse multiple repetitions of newline into a paragraph break
        pdf_text1 <- gsub("\n{2,}", "\n\n", pdf_text1)
        pdf_text1 <- gsub("\n\\s{2,}", " ", pdf_text1)
  
        # Combine text from all pages while preserving line breaks
        pdf_text1 <- paste(pdf_text1, collapse = "\n\n")
        pdf_text2 <- str_split(pdf_text1, pattern = "\n\n")
        #Document = pdf_text2
          Doc.id <- seq(1, length(pdf_text2[[1]]))
          calib <- data.frame(Doc.id, pdf_text2)
          colnames(calib) <- c("Doc.id","Documents")
          print(input$file$name)
          return(calib)} else
      {
      Document = read.csv(input$file$datapath ,header=TRUE, sep = ",", stringsAsFactors = F)
      Document[,1] <- str_to_title(Document[,1])
      Document[,1] <- make.names(Document[,1], unique=TRUE)
      Document[,1] <- tolower(Document[,1])
      Document[,1] <- str_replace_all(Document[,1],"\\.","_")
      Document<-Document[complete.cases(Document), ]
      Document <- Document[!(duplicated(Document[,1])), ]
      rownames(Document) <- Document[,1]
      return(Document)
      }
      
    }
  })  
  cols <- reactive({colnames(dataset())})    
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
    DT::datatable(dataset(),rownames = FALSE)
  })

  build_dtm <- function(corpus0){
    tidy_df = dplyr::tibble(text = corpus0) |>        
      dplyr::mutate(doc_id = row_number()) |> # Add doc_id first
      dplyr::rename(text = text) |>       
      dplyr::select(doc_id, text) |> # Reorder columns      
      unnest_tokens(word, text) |>
      dplyr::anti_join(stop_words) |>
      dplyr::group_by(doc_id) |>
      dplyr::count(word, sort=TRUE) |>
      dplyr::rename(value = n)    
    
    dtm = tidy_df |> 
      cast_sparse(doc_id, word, value)    
    return(dtm) 
  }

  # t-test word-groups collecting below
  word1 <- eventReactive(input$apply,{
    if (is.null(input$wordl_t1)) {return(NULL)}
    else{
      return(unlist(strsplit(input$wordl_t1, ",")))
    }
  })
  
  word2 <- eventReactive(input$apply,{
    if (is.null(input$wordl_t2)) {return(NULL)}
    else{
      return(unlist(strsplit(input$wordl_t2, ",")))
    }
  })
  
  run_ttest <- function(dtm1, word1, word2){    
    test_words = c(word1, word2); test_words
    test_words = stringr::str_trim(tolower(test_words))
    word1 = stringr::str_trim(tolower(word1))
    word2 = stringr::str_trim(tolower(word2))    
    #dtm1 = t(dtm1)    
    logi0 = colnames(dtm1) %in% test_words # 0s
    dtm11 = dtm1[,logi0]; dim(dtm11) 
    logi1 = (apply(dtm11, 1, sum) > 0)
    dtm12 = dtm11[logi1,]; dim(dtm12)
    
    logi2= colnames(dtm12) %in% word1
    logi3 = colnames(dtm12) %in% word2
    
    word1_colm = apply(dtm12[,logi2], 1, sum)
    word2_colm = apply(dtm12[,logi3], 1, sum)
    
    return(t.test(word1_colm, word2_colm)) # test_statistic0    
  } # func ends
  
  corpus_dtm <- eventReactive(input$apply,{build_dtm(dataset()[,input$y])})
  output$summary <- renderPrint(run_ttest(corpus_dtm(), word1(), word2())) 
  
  wrdl <- reactive({
    if(is.null(input$file2$datapath)|input$apply){return(NULL)}
    else { return(readLines(input$file2$datapath)) }
  })
  
#  This chunk is working
  wordlist0 <- eventReactive(input$apply,{
    if (is.null(input$file)) {return(NULL)} 
    else {
       a00 = unlist(strsplit(input$wordl,","))
       a01 = wrdl()
       wordlist0 = unique(gsub("'","",c(a00,a01)))       
       return(wordlist0) }
  })
    
  finalwordlist <- eventReactive(input$apply,{
      if (is.null(input$file)) {return(NULL)}
      else {
        corpus_lower = dataset()
        wl1 = NULL
        for (word in wordlist0()){
          if (sum(str_detect(corpus_lower, word)) > 0) {wl1 = c(wl1, word)} }
        return(wl1) }
    })  
  output$wordl <- renderPrint(finalwordlist())
  
  #This Chunk is Working
  textdf =  eventReactive(input$apply,{    
    textb = dataset()[,input$y]  # user-chosen text colm
    textdf1 = data.frame("text" = textb) |> 
      mutate(docID = row_number()) |>    # row_number() is v useful.    
      group_by(docID) |>
      unnest_tokens(sents, text, token="sentences", to_lower=FALSE) |>
      mutate(sentID = row_number()) |>
      select(docID, sentID, sents);
    return(textdf1)
  })
    
 output$downloadThisToken <- downloadHandler(
   filename = function(){paste(str_split(input$file$name,"\\.")[[1]][1],"_SentenceTokenized.csv",collapse = "") },
   content = function(file) {      
      new_df <- textdf()
      write.csv(new_df, file, row.names=T)            
    }
   )
    
  # build unit func for wl against one doc
  doc_proc <- function(i0, textdf1, wl1){

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
  wrapper_corpus <- function(textdf, wl1){    
    list_dfs = vector(mode="list", length=max(textdf$docID)) # use in wrapper func
    
    for (i0 in 1:max(textdf$docID)){
      list_dfs[[i0]] = doc_proc(i0, textdf, wl1)   } # i0 loop ends
    
    out_df = bind_rows(list_dfs)
    return(out_df) } # func ends
  
  output$SentenceToken = renderDataTable({ datatable(textdf()) })
  
  filteredCorpus <- reactive({
    outdf1 = wrapper_corpus(textdf(), finalwordlist())    
    return(outdf1)
  })

  ## highlighted wala beta phase
  a00 <- eventReactive(input$apply,{
    corpus_lower = filteredCorpus()
    for (word in finalwordlist()){
      word00 = paste0("**",word,"**")
      corpus_lower$filtered_sents = str_replace_all(corpus_lower$filtered_sents, word, word00)
    }
    #a00 = unlist(corpus_lower)    
    a00 <- as.data.frame(corpus_lower[(corpus_lower$filtered_sents != " NA "),])    
    return(a00)    
  })
  
  output$highlighted <- downloadHandler(
    filename = function(){paste(str_split(input$file$name,"\\.")[[1]][1],"_highilighted.csv",collapse = "")},
    content = function(file){
      new_df <- a00()
      write.csv(new_df, file, row.names =T)
    }
  )
  
  output$downloadThisOne = renderDataTable({
    datatable(filteredCorpus())
  })
  
  output$checker <- renderPlot({
    new_df <- filteredCorpus()
    newdf <- nrow(filteredCorpus())
    newdf2 <- nrow(new_df[(new_df$filtered_sents !=" NA "),])
    x <- c(newdf, newdf2)
    labels <- c("No of Documents Containing Keywords","No of Documents Containing NAs")
    pie(x, labels)
  })
  
   output$downloadTheOne <- downloadHandler(
   filename = function(){paste(str_split(input$file$name,"\\.")[[1]][1],"_Full_filtered.csv",collapse = "") },
   content = function(file) {
      new_df <- filteredCorpus()
      write.csv(new_df, file, row.names=T)      
    }
   )
    
   output$downloadThisTwo <- downloadHandler(
   filename = function(){paste(str_split(input$file$name,"\\.")[[1]][1],"_filtered.csv",collapse = "") },
   content = function(file) {
      
      new_df <- filteredCorpus()
      #new_df <- subset(new_df, filtered_sents!=" NA ")
      new_df <- new_df[(new_df$filtered_sents!=" NA "),]
      write.csv(new_df, file, row.names=T)
            
    }
   )
     
  output$downloadData1 <- downloadHandler(
    filename = function() { "Nokia_Lumia_reviews.txt" },
    content = function(file) {
      writeLines(readLines("data/Nokia_Lumia_reviews.txt"), file)
    }
  )
  
   output$downloadData2 <- downloadHandler(
    filename = function() { "Wordlist_Nokia.txt" },
    content = function(file) {
      writeLines(readLines("data/dummywordl.txt"), file)
    }
  )
  
  output$downloadData3 <- downloadHandler(
    filename = function() { "modi-speech-2022.txt" },
    content = function(file) {
      writeLines(readLines("data/modi-speech-2022.txt"), file)
    }
  )
  
    output$downloadData5 <- downloadHandler(
    filename = function() { "SC Judgement Airtel case PDF" },
    content = function(file) {
      writeLines(readLines("data/SC on Airtel case_Judgement_03-Jan-2024.pdf"), file)
    }
  )
  
  output$downloadData0 <- downloadHandler(
    filename = function() { "airline_sentiment.csv" },
    content = function(file) {
      write.csv(read.csv("data/airline_sentiment.csv"), file, row.names=F, col.names=F, fileEncoding = "UTF-8")
    }
  )
  
})
