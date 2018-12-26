#Invoking Shiny Library
library(shiny)

shinyUI( # start of UI code
  fluidPage(theme = shinytheme("slate"), # start of fluid page - to adjust to screen dimension
            
            titlePanel(strong("Twitter Sentiment Analysis")), #title of the App
            
            sidebarLayout( #start of side bar layout
  
		    sidebarPanel(
    
    textInput("searchTerm", "Enter the data to be searched with '#'", "#"),
    # A slider input taking maximum of 3k tweets to be pulled from twitter and a minium of 5, with default as 1000
    sliderInput("maxTweets","Number of current tweets to be used for the text & sentiment analysis:",min=5,max=3000,value=1000), 
    submitButton(text="Analyse")
    
  ),
  
  mainPanel(
    
    
    tabsetPanel(
     
      tabPanel("WordCloud",HTML("<div><h3>Frequently used words associated with the hashtag</h3></div>"),plotOutput("word"),
               HTML
               ("<div><h4> Word clouds add simplicity and clarity. The most used keywords stand out better in a word cloud. A communication tool which is very easy to understand, impactful and more visually engaging than a table data </h4></div>")),
      
      
      tabPanel("Statistical Charts",HTML
               ("<div><h3> Pie Charts & Histograms graphically depict the Positivie and Negative sentiments of People , scored on a scale of 5' opinion about of the hashtag
                 </h3></div>"),plotOutput("piechart"), plotOutput("histPos"), plotOutput("histNeg")
               ),
	    
    
      
      tabPanel("Table",HTML( "<div><h3> Tabular Display of Sentiment Score </h3></div>"), tableOutput("tabledata"),
			HTML ("<div><h4> The table summarizes the sentiment (positive, negative or neutral) of the tweets 
			      associated with the search hashtag by showing the score for each type of sentiment. </h4></div>")),
      
      tabPanel("Top Hashtags of User",textInput("user", "Enter User Name", "@"),submitButton(text="Search"),plotOutput("tophashtagsplot"),HTML
               ("<div> <h3>Hastag frequencies in the tweets of the tweeter</h3></div>"))
    )#end of tabset panel
  )#end of main panel
  
))#end of shinyUI
