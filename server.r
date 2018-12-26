#Installing the required packages from the Cran repository
EnsurePackage<-function(x)
{x <- as.character(x)
if (!require(x,character.only=TRUE))
{
  install.packages(pkgs=x,repos="http://cran.r-project.org")
  require(x,character.only=TRUE)
}
}

#Identifying packages to be loaded
PrepareTwitter<-function()
{
  EnsurePackage("twitteR")
  EnsurePackage("stringr")
  EnsurePackage("ROAuth")
  EnsurePackage("RCurl")
  EnsurePackage("ggplot2")
  EnsurePackage("reshape")
  EnsurePackage("tm")
  EnsurePackage("RJSONIO")
  EnsurePackage("wordcloud")
  EnsurePackage("gridExtra")
  EnsurePackage("plyr")
  EnsurePackage("e1071")
  EnsurePackage("RTextTools")
  EnsurePackage("shinythemes")
}

#Twitter Authentication Keys set up
PrepareTwitter()

api_key <- "ovfjKNVvJN3cledZCbbZ1hnUp" #Consumer key from twitter app
api_secret <- "pOUKSOHxzIqiXxdfucJPxn3kmfuTufkyKFIVMD837HB040E8Fx" #Consumer secret from twitter app
access_token <- "1068933937727078400-BbmceKhrkXO71atrxJGKSJoDg428q5" #access token from twitter app
access_token_secret <- "vIxrfM7PUaJyY4mrCPalRAaoMFVSpzulNS3jAqMwk5ovm" #access secret from twitter app
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

shinyServer(function(input, output) {
  
  #Search tweets and create a data frame
  # Cleaning the tweets
  TweetFrame<-function(twtList)
  {
    
    df<- do.call("rbind",lapply(twtList,as.data.frame))
    #removes emoticons
    df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
    df$text <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text)
    df$text <- gsub("&amp", "", df$text)
#Cleaning up twitter handles
    df$text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", df$text)
#Removing User Names
    df$text <- gsub("@\\w+", "", df$text)
#Cleaning up punctuations and digits
    df$text <- gsub("[[:punct:]]", "", df$text)
    df$text <- gsub("[[:digit:]]", "", df$text)
#Cleaning up URL's
    df$text <- gsub("http\\w+", "", df$text)	      
 return (df$text)
  }
  
  
  # Function to create a data frame from tweets
  
  pos.words = scan('C:/Practicum/positive-words.txt', what='character', comment.char=';')
  neg.words = scan('C:/Practicum/negative-words.txt', what='character', comment.char=';')
  
  wordDatabase<-function()
  {
    pos.words<<-c(pos.words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader', 'power', 'powerful', 'latest')
    neg.words<<-c(neg.words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not')
  }
  
  score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
  {
    require(plyr)
    require(stringr)
    list=lapply(sentences, function(sentence, pos.words, neg.words)
    {
      sentence = gsub('[[:punct:]]',' ',sentence)
      sentence = gsub('[[:cntrl:]]','',sentence)
      sentence = gsub('\\d+','',sentence)
      sentence = gsub('\n','',sentence)
      
      sentence = tolower(sentence)
      word.list = str_split(sentence, '\\s+')
      words = unlist(word.list)
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      pp=sum(pos.matches)
      nn = sum(neg.matches)
      score = sum(pos.matches) - sum(neg.matches)
      list1=c(score, pp, nn)
      return (list1)
    }, pos.words, neg.words)
    score_new=lapply(list, `[[`, 1)
    pp1=score=lapply(list, `[[`, 2)
    nn1=score=lapply(list, `[[`, 3)
    
    scores.df = data.frame(score=score_new, text=sentences)
    positive.df = data.frame(Positive=pp1, text=sentences)
    negative.df = data.frame(Negative=nn1, text=sentences)
    
    list_df=list(scores.df, positive.df, negative.df)
    return(list_df)
  }
  
  #TABLE DATA	
  
  library(reshape)
  sentimentAnalyser<-function(result)
  {
    #Creating a copy of result data frame
    test1=result[[1]]
    test2=result[[2]]
    test3=result[[3]]
    
    #Creating three different data frames for Score, Positive and Negative
    #Removing text column from data frame
    test1$text=NULL
    test2$text=NULL
    test3$text=NULL
    #Storing the first row(Containing the sentiment scores) in variable q
    q1=test1[1,]
    q2=test2[1,]
    q3=test3[1,]
    qq1=melt(q1, ,var='Score')
    qq2=melt(q2, ,var='Positive')
    qq3=melt(q3, ,var='Negative') 
    qq1['Score'] = NULL
    qq2['Positive'] = NULL
    qq3['Negative'] = NULL
    #Creating data frame
    table1 = data.frame(Text=result[[1]]$text, Score=qq1)
    table2 = data.frame(Text=result[[2]]$text, Score=qq2)
    table3 = data.frame(Text=result[[3]]$text, Score=qq3)
    
    #Merging three data frames into one
    table_final=data.frame(Text=table1$Text, Positive=table2$value, Negative=table3$value, Score=table1$value)
    return(table_final)
  }
  
  percentage<-function(table_final)
  {
    #Positive Percentage
    
    #Renaming
    posSc=table_final$Positive
    negSc=table_final$Negative
    
    #Adding column
    table_final$PosPercent = posSc/ (posSc+negSc)
    
    #Replacing Nan with zero
    pp = table_final$PosPercent
    pp[is.nan(pp)] <- 0
    table_final$PosPercent = pp*100
    
    #Negative Percentage
    
    #Adding column
    table_final$NegPercent = negSc/ (posSc+negSc)
    
    #Replacing Nan with zero
    nn = table_final$NegPercent
    nn[is.nan(nn)] <- 0
    table_final$NegPercent = nn*100
    
    return(table_final)
  }
  
  wordDatabase()
  
  twtList<-reactive({twtList<-searchTwitter(input$searchTerm, n=input$maxTweets, lang="en") })
  tweets<-reactive({tweets<-TweetFrame(twtList() )})
  
  result<-reactive({result<-score.sentiment(tweets(), pos.words, neg.words, .progress='none')})
  
  table_final<-reactive({table_final<-sentimentAnalyser(  result() )})
  table_final_percentage<-reactive({table_final_percentage<-percentage(  table_final() )})
  
  output$tabledata<-renderTable(table_final_percentage())	
  
  #WORDCLOUD
  wordclouds<-function(text)
  {
    library(tm)
    library(wordcloud)
    corpus <- Corpus(VectorSource(text))
    #clean text
    clean_text <- tm_map(corpus, removePunctuation)
    clean_text <- tm_map(clean_text, content_transformer(tolower))
   
    clean_text <- tm_map(clean_text, removeWords, stopwords("english"))
    clean_text <- tm_map(clean_text, removeNumbers)
    clean_text <- tm_map(clean_text, stripWhitespace)
    return (clean_text)
  }
  text_word<-reactive({text_word<-wordclouds( tweets() )})
  
  output$word <- renderPlot({ wordcloud(text_word(),random.order=F,max.words=75, col=rainbow(100), scale=c(4.5, 1)) })
                      
  
  #HISTOGRAM
  output$histPos<- renderPlot({ hist(table_final()$Positive, col= 'green', main="Frequency of the Positive Sentiment Score", xlab = "Positive Score from 0 to 10") })
  output$histNeg<- renderPlot({ hist(table_final()$Negative, col='red', main="Frequency of the Negative Sentiment Score", xlab = "Negative Scores from 0 to 10") })
  
  #PIE CHART
	
	slices <- reactive({c(sum(table_final()$Positive), sum(table_final()$Negative)) })
	labels <- c("Positive", "Negative")
	library(plotrix)
	#output$piechart<-renderPlot({pie(slices(), labels = labels, col=rainbow(length(labels)),explode=0.00, main="Pie Chart : Positive vs Negative") })
	output$piechart<-renderPlot({pie(slices(), labels = labels, col=c("green","red"), explode=0.00, main="Pie Chart : Positive vs Negative") })		 

  #TOP HASHTAGS OF USER HANDLE ENTERED
  
  tw1 <- reactive({ tw1 = userTimeline(input$user, n = 3200) })
  tw <- reactive({ tw = twListToDF(tw1()) })
  vec1<-reactive ({ vec1 = tw()$text })
  
  extract.hashes = function(vec){
    
    hash.pattern = "#[[:alpha:]]+"
    have.hash = grep(x = vec, pattern = hash.pattern)
    
    hash.matches = gregexpr(pattern = hash.pattern,
                            text = vec[have.hash])
    extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
    
    df = data.frame(table(tolower(unlist(extracted.hash))))
    colnames(df) = c("tag","freq")
    df = df[order(df$freq,decreasing = TRUE),]
    return(df)
  }
  
  dat<-reactive({ dat = head(extract.hashes(vec1()),50) })
  dat2<- reactive ({ dat2 = transform(dat(),tag = reorder(tag,freq)) })
  
  p<- reactive ({ p = ggplot(dat2(), aes(x = tag, y = freq)) + geom_bar(stat="identity", fill = "blue")
  p + coord_flip() + labs(title = "Hashtag frequencies in the tweets of the tweeter") })
  output$tophashtagsplot <- renderPlot ({ p() })	
}) #shiny server
