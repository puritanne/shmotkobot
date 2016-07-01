library("dplyr")
source("telegramr.r")

Sys.setlocale("LC_ALL", "Ru_Ru")

# Setup my bot

#Sys.setenv(R_TELEGRAM_BOT_GetNewDress_bot="227815923:AAHIlLFG8QowvPpq0aWtHqrQJY2A3QW9Ibw")

bot <- TGBot$new(token = "227815923:AAHIlLFG8QowvPpq0aWtHqrQJY2A3QW9Ibw")
bot$getMe()
default_chat_id <- 209952956
bot$set_default_chat_id(default_chat_id)
#old.updates <- bot$getUpdates()

adressbook <- data.frame(
  chat_id=as.numeric(1111),
  first_name="Olga",
  last_name="D",
  param_shoulders="average",
  param_breast="average",
  param_waist="narrow",
  param_hips="wide",
  param_btype="pear"
)


# Тестовый код для того, чтобы быстро отправлять мне товары

matchingClothes <- shortlist.hourglass
            
for (k in 1:nrow(matchingClothes)) {
    download.file(as.character(matchingClothes$picture[k]), "pic.jpeg")
    bot$sendPhoto('pic.jpeg', caption=matchingClothes$score_hourglass[k], chat_id=default_chat_id)
    bot$sendMessage (text= matchingClothes$url[k], parse_mode = 'markdown', chat_id=default_chat_id)
    cat(k)
}

#Главный цикл бота
offset <- NULL

repeat {
  updates <- bot$getUpdates(offset=offset)

  if (!is.null(nrow(updates)))
    for (i in 1:nrow(updates)) {
        for (j in 1:nrow(updates[i,]$message)) {

### Основная логика бота          
        
          if (updates[i,]$message$text == '/start') {
            
            #Если этого chat_id не было в адресной книге, записать
            if (!(updates[i,]$message$chat$id %in% adressbook$chat_id)) {
              adressbook <- rbind(
                adressbook,
                data.frame(chat_id=as.character(updates[i,]$message$chat$id),
                           first_name=updates[i,]$message$from$first_name,
                           last_name=updates[i,]$message$from$last_name,
                           param_shoulders=NA, 
                           param_breast=NA, 
                           param_waist=NA, 
                           param_hips=NA, 
                           param_btype=NA)
              )
              saveRDS(adressbook, "data/adressbook.RDS")
            }
            
            bot$sendMessage (text= paste('Привет! Я помогаю подбирать шмотки в интернет-магазинах. Сначала я задам несколько вопросов про тип фигуры и размер, а потом выберу из каталога Ламоды те вещи, которые, скорее всего, будут нормально сидеть.'), 
                             parse_mode = 'markdown', 
                             reply_markup='{}',
                             chat_id=updates[i,]$message$chat$id)
 
            bot$sendMessage (text= paste('Пока я умею подбирать блузы, рубашки и топы, только для девочек.'), 
                             parse_mode = 'markdown', 
                             chat_id=updates[i,]$message$chat$id)

            bot$sendMessage (text= paste('Лайкай те вещи, которые понравятся, чтобы я научился подбирать шмотки по вкусу.'), 
                             parse_mode = 'markdown', 
                             reply_markup='{}',
                             chat_id=updates[i,]$message$chat$id)
          
            bot$sendMessage (text= paste('Начнём?'), 
                             reply_markup='{"keyboard":[["Да!"],["Ну, удиви меня"]]}',
                             parse_mode = 'markdown', 
                             chat_id=updates[i,]$message$chat$id)
          }
          
          if (grepl("Да!|Ну, удиви меня", updates[i,]$message$text)) {
            bot$sendMessage (text= paste('Отлично!'), 
                             reply_markup='{}',
                             parse_mode = 'markdown', 
                             chat_id=updates[i,]$message$chat$id)

            bot$sendMessage (text= paste('Я задам несколько вопросов про Ваш тип фигуры и размер.'), 
                             parse_mode = 'markdown', 
                             reply_markup='{}',
                             chat_id=updates[i,]$message$chat$id)

            bot$sendMessage (text= paste('Какое строение у Ваших плеч?'), 
                             reply_markup='{"keyboard":[["Широкие, атлетические"],["Узкие, покатые"],["Ни широкие, ни узкие, что-то среднее"]]}',
                             parse_mode = 'markdown', 
                             chat_id=updates[i,]$message$chat$id)
          }
          
          if (grepl("Широкие, атлетические|Узкие, покатые|Ни широкие, ни узкие, что-то среднее", updates[i,]$message$text)) {
            
             c.id<-updates[i,]$message$chat$id
             c.text<-updates[i,]$message$text
             
             adressbook[adressbook$chat_id==c.id,] <- adressbook[adressbook$chat_id==c.id,] %>%
               mutate(param_shoulders = ifelse(c.text=="Широкие, атлетические", "wide", param_shoulders)) %>%
               mutate(param_shoulders = ifelse(c.text=="Узкие, покатые", "narrow", param_shoulders)) %>%
               mutate(param_shoulders = ifelse(c.text=="Ни широкие, ни узкие, что-то среднее", "average", param_shoulders)) 
                                              
            bot$sendMessage (text= paste('Какого объема Ваша грудь?'), 
                reply_markup='{"keyboard":[["Маленькая"],["Средняя"],["Большая"]]}',
                           parse_mode = 'markdown', 
                           chat_id=updates[i,]$message$chat$id)
            Sys.sleep(1)
          }
          
          if (grepl("Маленькая|Средняя|Большая", updates[i,]$message$text)) {
            
            c.id<-updates[i,]$message$chat$id
            c.text<-updates[i,]$message$text
            
            adressbook[adressbook$chat_id==c.id,] <- adressbook[adressbook$chat_id==c.id,] %>%
              mutate(param_breast = ifelse(c.text=="Маленькая", "small", param_breast)) %>%
              mutate(param_breast = ifelse(c.text=="Средняя", "average", param_breast)) %>%
              mutate(param_breast = ifelse(c.text=="Большая", "big", param_breast)) 
            
             bot$sendMessage (text= paste('Насколько выражена талия?'), 
                              reply_markup='{"keyboard":[["Прямая, почти не выражена"],["Объемная, есть круглый животик"],["Сравнительно узкая, ярко выражена"]]}',
                              parse_mode = 'markdown', 
                              chat_id=updates[i,]$message$chat$id)
            Sys.sleep(1)
          }
          
          if (grepl("Прямая, почти не выражена|Объемная, есть круглый животик|Сравнительно узкая, ярко выражена", updates[i,]$message$text)) {
            
            c.id<-updates[i,]$message$chat$id
            c.text<-updates[i,]$message$text
            
            adressbook[adressbook$chat_id==c.id,] <- adressbook[adressbook$chat_id==c.id,] %>%
              mutate(param_waist = ifelse(c.text=="Сравнительно узкая, ярко выражена", "small", param_waist)) %>%
              mutate(param_waist = ifelse(c.text=="Прямая, почти не выражена", "average", param_waist)) %>%
              mutate(param_waist = ifelse(c.text=="Объемная, есть круглый животик", "big", param_waist)) 
            
            bot$sendMessage (text= paste('Что лучше подходит к описанию Ваших бедер?'), 
                             reply_markup='{"keyboard":[["Узкие, мальчишеские"],["Широкие, округлые"],["Ни узкие, ни широкие, что-то среднее"]]}',
                             parse_mode = 'markdown', 
                             chat_id=updates[i,]$message$chat$id)
            Sys.sleep(1)
          }
          
          if (grepl("Узкие, мальчишеские|Широкие, округлые|Ни узкие, ни широкие, что-то среднее", updates[i,]$message$text)) {
            
            c.id<-updates[i,]$message$chat$id
            c.text<-updates[i,]$message$text
            
            adressbook[adressbook$chat_id==c.id,] <- adressbook[adressbook$chat_id==c.id,] %>%
              mutate(param_hips = ifelse(c.text=="Узкие, мальчишеские", "small", param_hips)) %>%
              mutate(param_hips = ifelse(c.text=="Ни узкие, ни широкие, что-то среднее", "average", param_hips)) %>%
              mutate(param_hips = ifelse(c.text=="Широкие, округлые", "big", param_hips)) 
            
            bot$sendMessage (text= paste('Спасибо! Мы все запомнили!'), 
                             reply_markup='{}',
                             parse_mode = 'markdown', 
                             chat_id=updates[i,]$message$chat$id)
          }
          
          
          
### Конец основной логики бота            
        }
    }
  offset <- max(updates$update_id+1)
}







