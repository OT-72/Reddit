
# required packages
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(tm)
library(ggplot2)
library(wordcloud)
library(syuzhet)
library(SnowballC)
library(qdap)



## -------------- data processing ----------------------------------------------

df <- read_csv("reddit_wsb.csv")



# getting rid of NA values
df <- na.omit(df)


text <- df$body

# cleaning the comment section

text_dta <- gsub("?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", text) ##to get rid of url links
text_dta <- gsub('\n', ' ', text_dta)
text_dta <- gsub('*', '', text_dta)
text_dta <- gsub("\"", ' ', text_dta)
text_dta <- gsub("/", ' ', text_dta)
text_dta <- gsub('Game Stop|GameStop|gamestop|game stop', 'gme', text_dta)


 



library(tm)
## Loading required package
CleanData <- text_dta

CleanData <- tolower(CleanData) #Turn the data into lower case
CleanData <- removePunctuation(CleanData)
CleanData <- removeWords(CleanData, stopwords("en")) #removing stop words
CleanData <- removeNumbers(CleanData)
CleanData <- stripWhitespace(CleanData) 
##CleanData <- wordStem(CleanData, language = "en")


df$Clean_Comment = CleanData


## ---------


ui <- fluidPage(
    # Title
    titlePanel("Reddit WallStreetBets Posts"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId = "top_words", label = "Number of Top Words", value = 50, min = 5, step = 1),
            numericInput(inputId = "n_char", label = "Least Number of Characters", value = 3, min = 1, step = 1),
            numericInput(inputId = "max_wc", label = "Maximum Words in the WordCloud", value = 20, min = 1, step = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h3("Most Frequent Words", align = "center"),
            plotOutput("most_frequent"),
            plotOutput("wordcloud"),
            h3("Sentiments from Top Words", align ="center"),
            plotOutput("sentimes_plot")
        )
    )
)

server <- function(input, output) {
    TextFrequency <- reactive({
        freq_terms(CleanData, top = input$top_words, at.least = input$n_char)
    })
    
    output$most_frequent <- renderPlot({
            ggplot(data = TextFrequency(), aes(x = reorder(WORD, FREQ), y = FREQ))+
            geom_col(fill = "Blue", alpha=.6)+ 
            coord_flip()+
            labs(x='Word', y='Frequency')
    })
    
    output$wordcloud <- renderPlot({
        tf <- TextFrequency()
        # Get frequently used words
        wordcloud(tf$WORD, tf$FREQ, colors = tf$FREQ, max.words = input$max_wc, random.order=FALSE, rot.per=0.40)
    })
    
    
    # sentiment graph output
    output$sentimes_plot <- renderPlot({
        tf <- TextFrequency()
        Sentiments <- get_nrc_sentiment(tf$WORD)
        Sentiments <- cbind("Words" = tf$WORD, Sentiments)
        SentimentsScore <- data.frame("Score" = colSums(Sentiments[, c(2,3,6,10,11)])) ##choosing relevant sentiment column 
        TotalSentiments <- cbind("Sentiments" = rownames(SentimentsScore), SentimentsScore) 
        ggplot(data = TotalSentiments, aes(x = Sentiments, y = Score))+
        geom_bar(stat = "identity", aes(fill = Sentiments))
    })    
}

# Run the application 
shinyApp(ui = ui, server = server)
