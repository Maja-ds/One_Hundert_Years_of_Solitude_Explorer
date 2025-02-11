library(shiny)
library(shinydashboard)
library(quanteda)
library(ggplot2)
library(dplyr)
library(textstem)
library(DT)
library(wordcloud)
library(shinycssloaders)
library(shinyWidgets)

df_neu <- readRDS("data/df_neu.RDS")
dtm <- readRDS("data/DTM.RDS")
korpus <- readRDS("data/Korpus.RDS")
df_emotions <- readRDS("data/emotions.RDS")
df <- readRDS("data/df.RDS")

to.plot_emotions <- reshape2::melt(df_emotions, id.var = "Chapter")

sentences <- readLines("data/Quotes.txt")
sentences <- sentences[grep("\\S", sentences)]



shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "One Hundred Years Explorer",
                    titleWidth = 500),
    dashboardSidebar(sidebarMenu(
      menuItem("Text Statistics", tabName = "linguistics"),
      menuItem("Word Distribution",  tabName = "distribution"),
      menuItem("Most Commons", tabName = "commons"),
      menuItem("Emotion Detection", tabName = "emotions"),
      menuItem("Quote of the Moment", tabName = "quote"),
      menuItem("Character List", tabName = "character"),
      menuItem("Information", tabName = "information")
    )),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css?v=1.0")
      ),
      tabItems(
        tabItem(tabName = "linguistics",
        h4("'It’s raining in Macondo'"),
        br(),
        fluidRow(column(width = 3,
        tags$h5("In the whole Book"),
        box(width = NULL, height = 75, title = "Types", background = "light-blue", "38809"),
        box(width = NULL, height = 75, title = "Tokens", background = "light-blue", "160020"),
        box(width = NULL, height = 75, title = "Sentenses", background = "light-blue", "5566"),
        box(width = NULL, height = 75, title = "Chapters", background = "light-blue", "20"),
        box(width = NULL, height = 75, title = "TTR", background = "light-blue", "0.24252")
        
        ),
        column(width = 9,
        tags$h5("Per Chapter"),
        box(width = NULL, status = "primary", height = 455, withSpinner(plotOutput("myPlot1", height = 440), color = "#48c9b0"), background = "light-blue") 
        )),
        br(),
        br(),
        br(),
        fluidRow(column(width = 12,
        box(
        title = "Table", 
        status = "primary", 
        solidHeader = TRUE, 
        width = 12, 
        DTOutput("table1"))
        ))),
        tabItem(tabName = "distribution",
        h4("'Cease, cows, life is short'"),
        br(),
        br(),
        tabsetPanel(
        tabPanel("My Distribution",
        fluidPage(column(width = 3,
        selectInput("dropdown2", 
        label = "", 
        choices = c("time", "solitude", "rain", "love", "church", "war", "god", "discover", "candy", "liberal", "conservative"), 
        selected = "time")
        )),
        fluidPage(column(width = 9,
        box(width = NULL, status = "primary", height = 400, withSpinner(plotOutput("myPlot2", height = 385), color = "#48c9b0"), background = "light-blue")                
        ),
        column(width = 3,
        box(width = NULL, status = "primary", title = "Sum Frequency",solidHeader = TRUE, height = 75, textOutput("text2"))
        )),
        fluidPage(column(width = 12,
        br(),
        br(),
        br(),
        br(),
        box(
        title = "Table",
        status = "primary", 
        solidHeader = TRUE, 
        width = 12, 
        DTOutput("table2"))
        ))
        ),
        tabPanel("Your Distribution",
        fluidPage(column(width = 3,
        textInput("input_text3", 
        label = "", 
        value = "enter your own word")
        )),
        fluidPage(column(width = 9,
        box(width = NULL, status = "primary", height = 400, uiOutput("message3"), plotOutput("myPlot3", height = 385), background = "light-blue")
        ),
        column(width = 3,
        box(width = NULL, status = "primary", title = "Sum Frequency",solidHeader = TRUE, height = 75, textOutput("text3"))
        )),
        br(),
        br(),
        br(),
        br(),
        br(),
        fluidPage(column(width = 12,
        box(title = "Table", status = "primary", solidHeader = TRUE,  width = 12, DTOutput("table3"))                
        ))
        )
        )),
        tabItem(tabName = "commons",
        h4("'Things have a life of their own. It’s simply a matter of waking up their souls'"),
        br(),
        br(),
        tabsetPanel(  
        tabPanel("Most Commons in the whole Book",
        br(),
        fluidPage(column(width = 3,
        sliderInput("slider4", 
        label = "Choose a value", 
        min = 5, 
        max = 50,
        value = 50)),
        column(width = 9,
        box(width = NULL, status = "primary", height = 455, withSpinner(plotOutput("myPlot4", height = 440), color = "#48c9b0"), background = "light-blue")      
        )), 
        br(),
        br(),
        br(),
        fluidPage(column(width = 12,
        box(
        title = "Table", 
        status = "primary", 
        solidHeader = TRUE, 
        width = 12, 
        DTOutput("table4"))    
        ))
        ),
        tabPanel("Most Commons per Chapter", 
        br(),
        fluidPage(column(width = 3,
        sliderInput("slider5", 
        label = "Choose a value", 
        min = 5, 
        max = 50,
        value = 50),
        br(),
        br(),
        selectInput("dropdown5", label = "Choose the Chapter", choices = korpus$Chapter)
        ),
        column(width = 9,
        box(width = NULL, status = "primary", height = 455, withSpinner(plotOutput("myPlot5", height = 440), color = "#48c9b0"), background = "light-blue") 
        )),
        br(),
        br(),
        br(),
        fluidPage(column(width = 12, 
        box(
        title = "Table", 
        status = "primary", 
        solidHeader = TRUE, 
        width = 12, 
        DTOutput("table5")
        )))
        ))
        ),
        tabItem(tabName = "emotions",
        h4("'In all the houses keys to memorizing objects and feelings had been written'"),
        br(),
        br(),
        tabsetPanel(
        tabPanel("Emotions in the whole Book",
        br(),
        fluidPage(column(width = 3),
        column(width = 9,
        box(width = NULL, status = "primary", height = 455, withSpinner(plotOutput("myPlot6", height = 440), color = "#48c9b0"), background = "light-blue")
        )),
        br(),
        br(),
        br(),
        fluidPage(column(width = 12,
        box(title = "Table", status = "primary", solidHeader = TRUE,  width = 12, DTOutput("table6"))              
        )),
        ),
        tabPanel("Emotions per Chapter",
        br(),
        fluidPage(column(width = 3,
        awesomeCheckboxGroup(
        inputId = "selected_emotions",
        label = "Choose emotion(s)",
        choices = unique(to.plot_emotions$variable),
        status = "primary",
        selected = "anger"),
        br(),
        h6("You can choose")
        ), 
        column(width = 9,
        box(width = NULL, status = "primary", height = 455, withSpinner(plotOutput("myPlot7", height = 440), color = "#48c9b0"), background = "light-blue")
        )),
        br(),
        br(),
        br(),
        fluidPage(column(width = 12,
        box(title = "Table", status = "primary", solidHeader = TRUE,  width = 12, DTOutput("table7"))               
        ))
        ))
        ),
        tabItem(tabName = "quote",
        h4("'I’m not shooting you. It’s the revolution that’s shooting you'"),
        br(),
        br(),
        br(),
        fluidPage(
        column(width = 12,
        actionBttn(inputId = "generate_btn", label = "Quote of the Moment", size = "md", style = "fill"))
        ),
        br(),
        fluidPage(column(width = 12,
        box(class = "custom-box", width = NULL, status = "primary", 
        uiOutput("random_sentence"))))
        ),
        tabItem(tabName = "character",
        h4("'It’s the largest diamond in the world.'-'No. It’s ice.'"),
        br(),
        fluidPage(column(width = 3,
        h5("Very Important Person"),
        h5("Buendía family"),
        h2("úrsula iguarán"),
        h2("josé arcadio buendía"),
        h2("amaranta"),
        h2("rebeca"),
        h2("colonel aureliano buendía"),
        h2("remedios moscote"),
        h2("josé arcadio"),
        h2("aureliano josé"),
        h2("arcadio"),
        h2("santa sofía de la piedad"),
        h2("remedios the beauty")
        ), column(width = 3,
        h5("Very Importan Person"),
        h5("Buendía family"),
        h2("josé arcadio segundo"),
        h2("aureliano segundo"),
        h2("fernanda del carpio"),
        h2("josé arcadio (II)"),
        h2("amaranta úrsula"),
        h2("gaston"),
        h2("renata remedios (meme)"),
        h2("aureliano (II)"),
        h2("aureliano babilonia")),
        column(width = 3,
        h5("Very Important Person"),
        h5("not Buendía"),
        h2("melquíades"),
        h2("pilar ternera"),
        h2("petra cotes"),
        h2("mauricio babilonia"),
        h2("pietro crespi"),
        h2("colonel gerineldo márquez"),
        h2("magnifico visbal"),
        h2("apolinar moscote")),
        column(width = 3,
        h5("Less Important Person"),
        h5("not Buendía"),
        h2("prudencio aguilar"),
        h2("visitación"),
        h2("cataure"),
        h2("catarino"),
        h2("francisco the man"),
        h2("father nicanor reyna"),
        h2("amparo moscote"),
        h2("colonel gregorio stevenson"),
        h2("colonel lorenzo gavilán"),
        h2("general teófilo vargas"),
        h2("general josé raquel moncada")))
        ),
        tabItem(tabName = "information",
        h4("'The earth is round, like an orange'"),
        br(),
        br(),
        tabsetPanel(
        tabPanel("About the Book",
        br(),
        h5(HTML("<span style='color: #ca6f1e;'>One Hundred Years of Solitude</span> by <span style='color: steelblue;'>Gabriel García Márquez</span> is a landmark novel
        in Latin American literature, <br> first published in 1967. It tells the multi-generational
        story of the Buendía family in the fictional town of Macondo, <br>
        blending magical realism with historical and political themes. <br>
        The novel explores themes of solitude, fate, and the cyclical nature of history, <br>
        offering a richly symbolic and imaginative narrative that has made it one of 
        the most influential works of the 20th century."))
        ),
        tabPanel("Terms",
        br(),
        br(),
        h5(HTML("<span style='color: #ca6f1e;'>Types:</span> term refers to the number of distinct words in a text")),
        br(),
        h5(HTML("<span style='color: #ca6f1e;'>Tokens:</span> term refers to the number of all words in a text")),
        br(),
        h5(HTML("<span style='color: #ca6f1e;'>TTR:</span> Types / Tokens, also knowns as Type-Token-Ratio, a simple measure of lexical diversity")),
        br(),
        h5(HTML("<span style='color: #ca6f1e;'>Stopwords:</span> set of commonly used words that are often filtered out during text processing  because they have no semantic meaning ")),
        br(),
        h5(HTML("<span style='color: #ca6f1e;'>Lemmatization:</span> Lemmatization is the process of reducing a word to its base or root form, called the lemma, while considering its meaning and context. For example, words like <span style = 'color: steelblue'>running</span>, <span style = 'color: steelblue'>ran</span>, and <span style = 'color: steelblue'>runs</span> are all reduced to their lemma, <span style = 'color: steelblue'>run</span>. This helps in analyzing text consistently, as different forms of the same word are treated as one."))),
        tabPanel("Methods",
        br(),
        h5(HTML("The number of types, tokens, and sentences
        was determined using the <span style='color: #ca6f1e;'>summary(corpus)</span> 
        function from the <span style='color: #ca6f1e;'>quanteda</span> package.<br><br>
        The text was then cleaned by removing punctuation marks and stop words.<br>
        Subsequently, all words in the text were lemmatized and converted to lowercase.<br><br>
        The identification of emotions was conducted using a dictionary-based approach,<br> applying the <span style='color: #ca6f1e;'>data_dictionary_NRC</span> from the <span style='color: #ca6f1e;'>quanteda.sentiment</span> package."))
        )
      )))
    )),
  server = function(input, output) {

    output$myPlot1 <- renderPlot({
      corpus <- corpus(df$Kapitel_Text, docvars = df$Kapitel_Nummer)
      korpus_summary <- summary(corpus)
      df_neu <- korpus_summary %>% select(-Text)
      to.plot <- reshape2::melt(df_neu, id.var = "docvars")
      ggplot(to.plot, aes(x = docvars, y = value, fill = variable, group = variable)) +
        geom_bar(stat = "identity", alpha = 0.7) +
        scale_fill_manual(values = c("lightgreen", "#b9770e", "#48c9b0")) +
        theme_minimal()+ 
        labs(x = "Chapter", y = "Frequency") +
        theme(legend.position = "none") +
        facet_wrap(.~variable, scales = "free")
      
    })
    output$table1 <- renderDT({
      df_neu$TTR <- df_neu$Types / df_neu$Tokens
      df_neu$TTR <- round(df_neu$TTR, 4)
      datatable(df_neu, 
                options = list(
                  searching = FALSE, 
                  lengthMenu = list(c(10, 15, -1), c('10', '15', 'alle')),
                  paging = TRUE,   
                  columnDefs = list(list(targets = 0, visible = FALSE)) 
                ))  
    })
    
    processedData2 <- reactive({
      
      terms <- input$dropdown2
      DTM.reduce <- as.matrix(dtm[, terms])
      to.plot <- reshape2::melt(DTM.reduce, id.var = "docs")
      to.plot$Chapter <- 1:nrow(to.plot)
      as.data.frame(to.plot)
    })
    
    output$myPlot2 <- renderPlot({
      to.plot <- processedData2()
      ggplot(to.plot, aes(x = Chapter, y = value, colour = features, group = features)) + 
        geom_line(size = 0.8) +
        labs(x = "Chapter", y = "Frequency") +
        scale_x_continuous(breaks = 1:20) + 
        scale_color_manual(values = c("#27ae60")) +
        scale_y_continuous(breaks = scales::breaks_pretty(n = 10)) +
        theme_minimal() +
        ggtitle(paste0("Frequency of ", input$dropdown2)) +
        theme(legend.position = "none") 
    })
    
    output$text2 <- renderText({
      to.plot <- processedData2()
      sum(to.plot$value, na.rm = TRUE)
    })
    
    output$table2 <- renderDT({
      to.plot <- processedData2()
      to.plot$Chapter <- paste("Chapter", seq_len(nrow(to.plot)))
      to.plot <- to.plot %>%
        rename(
          Frequency = value,  
          Word = features      
        )
      to.table <- to.plot[, c("Chapter", "Word", "Frequency")]
      datatable(to.table, 
                options = list(
                  searching = FALSE, 
                  lengthMenu = list(c(10, 15, -1), c('10', '15', 'alle')),
                  paging = TRUE,   
                  columnDefs = list(list(targets = 0, visible = FALSE)) 
                ))
    })  
    
    processedData3 <- reactive({
      terms <- input$input_text3
      DTM.reduce <- as.matrix(dtm[, terms])
      to.plot <- reshape2::melt(DTM.reduce, id.var = "docs")
      to.plot$Chapter <- 1:nrow(to.plot)
      as.data.frame(to.plot)
    })
    
    output$message3 <- renderUI({
      if (is.null(input$input_text3) || !all(input$input_text3 %in% colnames(dtm))) {
        tags$div(
          style = "color: black; font-weight: bold; line-height: 1.4; font-size: 14px; 
          overflow-y: auto; max-height: 400px; padding: 10px;",
          tags$p("Tips for selecting valid terms:"),
          tags$ul(
            tags$li("You can only search for single words, such as 'chestnut', but not phrases like 'chestnut tree'"),
            tags$li("All words in the corpus are lowercase, so make sure to search for 'arcadio' instead of 'Arcadio'"),
            tags$li("All words in the corpus are lemmatized, so search for 'run' instead of 'runs', for example"),
            tags$li("Stop words such as 'a', 'the', and others have been removed from the corpus"),
            tags$li("Some names in the corpus are written with special characters, such as 'úrsula', which can make searching more difficult. Please refer to the 'Character List' tab to see the correct spelling of the names"),
            tags$li("Several characters in the novel share the same name. For instance, searching for 'remedios' will return results for Remedios Moscote, Remedios the Beauty, as well as Renata Remedios")
          )
        )
      }
    })
    
    output$myPlot3 <- renderPlot({
      validate(
        need(all(input$input_text3 %in% colnames(dtm)), "")
      )
      to.plot <- processedData3()
      ggplot(to.plot, aes(x = Chapter, y = value, colour = features, group = features)) + 
        geom_line(size = 0.8) +
        labs(x = "Chapter", y = "Frequency") +
        scale_x_continuous(breaks = 1:20) + 
        scale_color_manual(values = c("#ca6f1e")) +
        scale_y_continuous(breaks = scales::breaks_pretty(n = 10)) +
        theme_minimal() +
        ggtitle(paste0("Frequency of ", input$input_text3)) +
        theme(legend.position = "none") 
    })
    
    output$text3 <- renderText({
      validate(
        need(all(input$input_text3 %in% colnames(dtm)), "0")
      )
      to.plot <- processedData3()
      sum(to.plot$value, na.rm = TRUE)
    })
    
    output$table3 <- renderDT({
      validate(
        need(all(input$input_text3 %in% colnames(dtm)), "")
      )
      to.plot <- processedData3()
      to.plot$Chapter <- paste("Chapter", seq_len(nrow(to.plot)))
      to.plot <- to.plot %>%
        rename(
          Frequency = value,  
          Word = features      
        )
      to.table <- to.plot[, c("Chapter", "Word", "Frequency")]
      datatable(to.table, 
                options = list(
                  searching = FALSE, 
                  lengthMenu = list(c(10, 15, -1), c('10', '15', 'alle')),
                  paging = TRUE,   
                  columnDefs = list(list(targets = 0, visible = FALSE)) 
    ))
    })
    
    processedData4 <- reactive({
      freqs <- colSums(dtm)
      words <- colnames(dtm)
      wordlist <- data.frame(words, freqs)
      wordlist_sorted <- wordlist[order(-wordlist$freqs), ]
      wordlist_sorted
    })
    
    output$myPlot4 <- renderPlot({
      wordlist <- processedData4()
      wordcloud(wordlist$words, wordlist$freqs, scale=c(4,0.1), max.words=input$slider4, colors=brewer.pal(8, "Dark2"))
    })
    
    output$table4 <- renderDT({
      wordlist_sorted <- processedData4()
      wordlist_sorted <- head(wordlist_sorted, 50)
      datatable(wordlist_sorted, 
                options = list(
                  searching = FALSE,
                  lengthMenu = list(c(10, 25, -1), c('10', '25', 'alle')),
                  paging = TRUE,
                  columnDefs = list(list(targets = 0, visible = FALSE))
      ))
    })
    
    processedData5 <- reactive({
      req(input$dropdown5)
      data <- korpus
      text_chapter <- data$Kapitel_Text[data$Chapter == input$dropdown5]
      corpus <- corpus(text_chapter)
      toks <- quanteda::tokens(as.character(corpus), remove_punct = TRUE, remove_symbols = TRUE)
      toks <- tokens_tolower(toks)
      toks <- tokens_remove(toks, stopwords("english"))
      toks <- lapply(toks, textstem::lemmatize_words)
      word_freq <- table(toks)
      sorted_word_freq <- sort(word_freq, decreasing = TRUE)
      top_50_words <- head(sorted_word_freq, 50)
      as.data.frame(top_50_words)
    })
    
    output$myPlot5 <- renderPlot({
      top_50_words <- processedData5()
      colnames(top_50_words) <- c("Word", "Frequency")
      wordcloud(top_50_words$Word, top_50_words$Frequency, 
      max.words = input$slider5, colors = brewer.pal(8, "Dark2"))
    })
    
    
    output$table5 <- renderDT({
      df_50 <- processedData5()
      df_50 <- data.frame(df_50)
      colnames(df_50) <- c("Word", "Frequency")
      datatable(df_50, 
      options = list(
      searching = FALSE, 
      lengthMenu = list(c(10, 25, -1), c('10', '25', 'alle')),
      paging = TRUE,
      columnDefs = list(list(targets = 0, visible = FALSE)) 
      ))
    })
    
    processedData6 <- reactive({
      emotions_neu<- to.plot_emotions[, !names(to.plot_emotions) %in% c("Chapter")]
      ergebnis <- aggregate(value ~ variable, data = emotions_neu, sum)
      ergebnis <- ergebnis %>%
        mutate(percent_label = ergebnis$value / sum(ergebnis$value) * 100,
        percent = paste0(round(percent_label, 1), "%"))
      as.data.frame(ergebnis)
    })
    
    output$myPlot6 <- renderPlot({
      ergebnis <- processedData6()
      ergebnis$variable <- factor(ergebnis$variable, levels = ergebnis$variable[order(ergebnis$value)])
      mycols <- c("#0073C2FF", "#ca6f1e", "#CD534CFF", "darkgreen", "darkred", "#EFC000FF" ,"olivedrab", "#868686FF" )
      ggplot(ergebnis, aes(x = 1.5, y = value, fill = variable)) +
        geom_bar(stat = "identity", color = "white", size = 1) +
        coord_polar(theta = "y", start = 0)+
        scale_fill_manual(values = mycols, breaks = rev(levels(ergebnis$variable))) +
        geom_text(aes(label = percent),
                  position = position_stack(vjust = 0.5), color = "white", size = 3) +
        theme_void() +
        labs(fill = "Emotions") +
        xlim(0.5, 2.5)
    })
    
    output$table6 <- renderDT({
      ergebnis <- processedData6() 
      ergebnis <- ergebnis %>% select(-percent_label)
      ergebnis$value <- as.character(ergebnis$value)
      ergebnis <- ergebnis[order(ergebnis$value, decreasing = TRUE), ]
      ergebnis <- ergebnis[, c("variable", "percent", "value")]
      datatable(ergebnis, 
      options = list(
      searching = FALSE, 
      paging = F,
      columnDefs = list(list(targets = 0, visible = FALSE)) 
      ))
    })
    
    output$myPlot7 <- renderPlot({
      filtered_data <- to.plot_emotions[to.plot_emotions$variable %in% input$selected_emotions, ]
      ggplot(filtered_data, aes(x = Chapter, y = value, group = variable, colour = variable)) +
        geom_line(linewidth = 1) + 
        scale_x_continuous(breaks = 1:20) +
        labs(x = "Chapter", y = "Frequency", color = "Emotions") +
        scale_color_brewer(palette="Set1") +
        theme_minimal()
    })
    
    output$table7 <- renderDT({
      df_emotions$Chapter <- paste("Chapter", seq_len(nrow(df_emotions)))
      datatable(df_emotions, 
          options = list(
          searching = FALSE, 
          lengthMenu = list(c(10, 15, -1), c('10', '15', 'alle')),
          paging = TRUE,
          columnDefs = list(list(targets = 0, visible = FALSE)) 
    ))
    })
    
    random_sentence <- reactive({
      input$generate_btn
      isolate({
        sample(sentences, 1) 
      })
    })
    
    output$random_sentence <- renderUI({
      HTML(paste0("<h6>", random_sentence(), "</h6>"))
    })
    
  }
)


  
