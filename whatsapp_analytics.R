#-------------------------->>>   
# [0] LIBRERIAS Y DEPENDENCIAS 
#-------------------------->>>  

library(rwhatsapp)
library(plyr)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(stringi)
library(purrr)
library(tidyverse)
library(dplyr)

# install.packages(c("wordcloud","wordcloud2", "tm"), dependencies = T)
#-------------------------->>>   
# [1] PREPARACION DE DATOS Y PARAMETROS  
#-----
chat <- "./data/_chat.txt"
readLines(chat)

chat <- rwa_read(x = chat, encoding = "utf-8")
#para filtrar fechas
chat$time <- as.Date(chat$time)
chat_2021 <- chat[chat$time >= "2021-04-01",]
chat_2021 <- chat_2021[,!(names(chat_2021) %in% c("emoji", "emoji_name", "source")),]

# quito NA pero sirve para los ardidos después
chat_2021 <- chat_2021[!(chat$author %in% c("Victor Herrera")),]

# revisar
chat_2021 <- chat_2021[!(is.na(chat_2021$author)),]


#Para análisis de texto
chat_2021$text <- gsub("[^\x01-\x7F]", "", (chat_2021$text))
vPosts<- tm::VCorpus(VectorSource(stringi::stri_trans_general(chat_2021$text, "Latin-ASCII")))
inspect(vPosts)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

vPosts <- tm_map(vPosts, toSpace, "/")
vPosts <- tm_map(vPosts, toSpace, "@")
vPosts <- tm_map(vPosts, toSpace, "\\|")


# Convert the text to lower case
vPosts <- tm_map(vPosts, content_transformer(tolower))
# Remove numbers
vPosts <- tm_map(vPosts, removeNumbers)
# Remove english common stopwords
vPosts <- tm_map(vPosts, removeWords, stopwords("spanish")) # mejor crear
#omite archivos imagenes, stickers, audios
vPostCheckpointSpanish <- vPosts
vPosts <- tm_map(vPosts, removeWords, c("omitted", "sticker", "image", "audio", 
                                        "video", "los", "vos", "vas", "pues",
                                        "https", "bueno", "dice", "Twitter.com", "gif",
                                        "dice" ))
# Remove your own stop word
# specify your stopwords as a character vector
# vPosts <- tm_map(vPosts, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
vPosts <- tm_map(vPosts, removePunctuation)
# Eliminate extra white spaces
vPosts <- tm_map(vPosts, stripWhitespace)


if(.Platform$OS.type == "windows") withAutoprint({
  memory.size()
  memory.size(TRUE)
  memory.limit()
})
memory.limit(size=56000)

#Matriz General
dtm <- TermDocumentMatrix(vPosts)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

#wordcloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 5,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
head(d, 20)
participacionTotal <- length(df_Result$posts)

# PReparación de resumenes
length(chat_2021$text)
summary(chat_2021 )

# Reporte Mensual ####

chat <- "./data/_chat.txt"
chat <- rwa_read(x = chat, encoding = "utf-8")

chat$time <- as.Date(chat$time)
chat_202104 <- chat[chat$time >= "2021-04-01",]
chat_202104 <- chat_202104[,!(names(chat_202104) %in% c("emoji", "emoji_name", "source", "id")),]

chat_202104 <- chat_202104[!(chat_202104$text == 'â€Žsticker omitted'),]
chat_202104 <- chat_202104[!(is.na(chat_202104$author)),]
countxPersonaxfecha <- count(df = chat_202104, vars = c("author","time"))
countxPersona <- count(df = chat_202104, vars = c("author"))
countxPersona[order(countxPersona$freq,decreasing = T),]
countxPersonaxfecha[order(countxPersona$freq,decreasing = T),]

chat_202104_EP <- chat_202104[chat_202104$author == 'Emiliano Plazaola',]

vPosts<- tm::VCorpus(VectorSource(stringi::stri_trans_general(chat_202104_EP$text, "Latin-ASCII")))
inspect(vPosts)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

vPosts <- tm_map(vPosts, toSpace, "/")
vPosts <- tm_map(vPosts, toSpace, "@")
vPosts <- tm_map(vPosts, toSpace, "\\|")


# Convert the text to lower case
vPosts <- tm_map(vPosts, content_transformer(tolower))
# Remove numbers
vPosts <- tm_map(vPosts, removeNumbers)
# Remove english common stopwords
vPosts <- tm_map(vPosts, removeWords, stopwords("spanish")) # mejor crear
#omite archivos imagenes, stickers, audios
vPostCheckpointSpanish <- vPosts
vPosts <- tm_map(vPosts, removeWords, c("omitted", "sticker", "€zimage", "€zaudio", "image", "audio", 
                                        "video", "los", "vos", "vas", "pues",
                                        "https", "bueno", "dice", "Twitter.com", "gif",
                                        "dice" ))
# Remove your own stop word
# specify your stopwords as a character vector
# vPosts <- tm_map(vPosts, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
vPosts <- tm_map(vPosts, removePunctuation)
# Eliminate extra white spaces
vPosts <- tm_map(vPosts, stripWhitespace)

#Matriz General
dtm <- TermDocumentMatrix(vPosts)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

#wordcloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 5,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
head(d, 20)
participacionTotal <- length(df_Result$posts)

length(chat$text)
summary(chat)

#-------------------------->>> 
# [3] PREPARACION DE RESULTADOS
#-------------------------->>> 

# # Escrimos resutlado final
# writexl::write_xlsx(x = lsFinal, path = "Resumen Jamboard Jeveando vs Liderando.xlsx")
