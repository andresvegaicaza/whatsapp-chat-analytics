library(rwhatsapp)
library(plyr)
# history <- system.file("extdata", "C:/Users/andres.vega/Google Drive/Git/whastapp_chat_analytics/WhatsApp Chat - Si No Jodas/_chat.txt", package = "rwhatsapp")

chat <- "C:/Users/andres.vega/Google Drive/Git/whastapp_chat_analytics/WhatsApp Chat - Si No Jodas/_chat.txt"
# system.file("extdata", "C:/Users/andres.vega/Google Drive/Git/whastapp_chat_analytics/WhatsApp Chat - Si No Jodas/_chat.txt", package = "rwhatsapp")

# history <- readLines(con = , encoding = "utf-8")

chat <- rwa_read(chat)
chat <- chat[,!(names(chat) %in% c("emoji", "emoji_name", "source"))]
chat$time <- as.Date(chat$time)

chat_2021 <- chat[chat$time >= "2021-01-01",]
chat_2021 <- chat_2021[!chat$author == "Victor Herrera",]

length(chat_2021$text)
summary(chat_2021 )
countxPersona <- count(df = chat_2021, vars = "author")
countxPersona[order(countxPersona$freq),]




  length(chat$text)
summary(chat)
