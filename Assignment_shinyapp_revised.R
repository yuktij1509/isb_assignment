library("shiny")
library("tm")
library("wordcloud")
library("dplyr")
library("tidytext")
library(ggplot2)

ui<-fluidPage(
  
  titlePanel("Text Filtering by Keywords"),
  
  # Input in sidepanel:
  sidebarPanel(
    
    fileInput("file", "Upload Review file in CSV"),
    textInput("keyword",('Enter key words seperated by comma (,)'),value = ''),
    textInput("stopw", ("Enter stop words separated by comma(,)"), value = "car,suv"),
    sliderInput("freq", "Minimum Frequency:", min = 1,  max = 20, value = 2),
    
    sliderInput("max",  "Maximum Number of Words in Wordcloud:", min = 1,  max = 70,  value = 20)
    
  ),
  
  mainPanel( 
    tabsetPanel(type = "tabs",
                tabPanel("Filtered Corpus",
                h4(p("Filtered Corpus text file")),br(),br(),
                verbatimTextOutput("filter_corp")),
   
                tabPanel("Frequency Bar Chart & Word Cloud",
                h4("Horizontal Bar Chart"),
                plotOutput("barchart",height = 600, width = 800),
                h4("Frequency Word Cloud"),
                plotOutput("wordcloud",height = 700, width = 700)
                
  )
 )
)
)



server<-function(input, output,session) {
  set.seed=1253
  
  Reviews <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE,stringsAsFactors=FALSE, sep = ","))
      return(Dataset)
    }
  })
  
  wordlist <- reactive({
  if (input$keyword == "") {key = ""} else {
    key = unlist(strsplit(input$keyword,","))}
  })
    
   arbit_wl = reactive({
      arbit_wl = wordlist() %>% data_frame()
      colnames(arbit_wl) = "word"
      arbit_wl
      })
   
   unnecessary_wl = reactive({
     unnecessary_wl = unlist(strsplit(input$stopw,",")) %>% data_frame()
     colnames(unnecessary_wl) = "word"
     unnecessary_wl
   })
   
   
   sentence = reactive({
     arbit_wl = arbit_wl()
     pred.an1 = tibble(Reviews()$Review.Text)
     #pred.an1 = pred.an[(pred.an != "")] %>% data_frame()
     #colnames(pred.an1)= "text"
     
     pred.an2 = pred.an1 %>% unnest_tokens(sentence, Reviews()$Review.Text, token = "sentences")
     
     a0 = pred.an2 %>%
       
       # setup a sentence index for later reference
       mutate(sent1 = seq(1:nrow(pred.an2))) %>% dplyr::select(sent1, sentence) %>%
       
       # tokenize words in each sentence usng group_by
       group_by(sentence) %>% unnest_tokens(word, sentence) %>% ungroup() %>%
       
       # now merge wordlists
       inner_join(arbit_wl, by = "word") %>%
       
       # de-duplicate sentence index
       dplyr::select(sent1) %>% unique()
     
     # filter corpus based on sentence list
     pred.an3 = pred.an2 %>%
       # sentence index construction
       mutate(sent1 = seq(1:nrow(pred.an2))) %>% dplyr::select(sent1, sentence) %>%
       
       # inner join and retain selected sents only
       inner_join(a0, by = "sent1") %>% dplyr::select(sentence, sent1)
     
     pred.an3 = data.frame(pred.an3)
     sentence = pred.an3$sentence
     return(sentence)

   })
   
   dataset<- reactive({
     sentence = sentence()
     df<- tibble(sentence)
     tidy_reviews<- df %>% unnest_tokens(word,sentence)
     return(tidy_reviews)
   })
   
   
   
  
   output$filter_corp = renderPrint({
     cat("Total ", length(sentence())," sentences.\n")
     cat(as.String(paste0(1:length(sentence())," -> ", sentence())))
   })
   
   output$barchart <- renderPlot({
     unnecessary_wl = unnecessary_wl()
     dataset() %>%
       anti_join(stop_words)%>%
       anti_join(unnecessary_wl())%>%
       count(word, sort = TRUE)%>%
       slice(1:15)%>% 
       filter(n >input$freq)%>%   # n is wordcount colname.
       mutate(word = reorder(word, n)) %>%  # mutate() reorders columns & renames too
       ggplot(aes(word, n)) +
       theme(text = element_text(size=15)) +
       geom_bar(stat = "identity", col = "red", fill = "blue") +
       xlab(NULL) +
       coord_flip()
   })
   
   output$wordcloud <- renderPlot({
     color = brewer.pal(8, "Dark2")
     unnecessary_wl = unnecessary_wl()
     dataset() %>%
       anti_join(stop_words)%>%
       anti_join(unnecessary_wl())%>%
       count(word, sort = TRUE) %>%
       filter(n >input$freq)%>%
       with(wordcloud(word, n, random.order = FALSE, max.words = input$max, colors=color))
   })
   
   
  }

shinyApp(ui = ui,server = server)