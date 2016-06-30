library("dplyr")
source("telegramr.r")

Sys.setlocale("LC_ALL", "Ru_Ru")

# Setup my bot

#Sys.setenv(R_TELEGRAM_BOT_GetNewDress_bot="227815923:AAHIlLFG8QowvPpq0aWtHqrQJY2A3QW9Ibw")

bot <- TGBot$new(token = "227815923:AAHIlLFG8QowvPpq0aWtHqrQJY2A3QW9Ibw")
bot$getMe()
default_chat_id <- 209952956
bot$set_default_chat_id(default_chat_id)
old.updates <- bot$getUpdates()

adressbook <- data.frame(
  chat_id=1111,
  first_name="Olga",
  last_name="D",
  param_shoulders=as.factor("average"),
  param_chest=as.factor("average"),
  param_waist=as.factor("narrow"),
  param_hips=as.factor("wide"),
  param_btype=as.factor("pear")
)


# Тестовый код для того, чтобы быстро отправлять мне товары

matchingClothes <- shortlist.hourglass
            
for (k in 1:10) {
    download.file(as.character(matchingClothes$picture[k]), "pic.jpeg")
    bot$sendPhoto('pic.jpeg', caption=matchingClothes$score_hourglass[k], chat_id=default_chat_id)
    bot$sendMessage (text= matchingClothes$url[k], parse_mode = 'markdown', chat_id=default_chat_id)
    cat(k)
}

#Главный цикл бота

repeat {
  updates <- bot$getUpdates()
  updates$is.old <- updates$update_id %in% old.updates$update_id
  
  if (!is.null(nrow(updates)))
    for (i in 1:nrow(updates)) {
      if (updates[i,]$is.old == FALSE) {
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
                           param_chest=NA, 
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
            
#             adressbook <- adressbook %>% filter(chat_id == updates[i,]$message$chat$id) %>%
#               mutate(prop_shoulders=ifelse(updates[i,]$message$text=="Широкие, атлетические", as.factor("wide"), prop_shoulders)) %>%
#               mutate(prop_shoulders=ifelse(updates[i,]$message$text=="Узкие, покатые", as.factor("narrow"), prop_shoulders)) %>%
#               mutate(prop_shoulders=ifelse(updates[i,]$message$text=="Ни широкие, ни узкие, что-то среднее", as.factor("average"), prop_shoulders))
            
            bot$sendMessage (text= paste('Какого объема Ваша грудь?'), 
                reply_markup='{"keyboard":[["Маленькая"],["Средняя"],["Большая"]]}',
                           parse_mode = 'markdown', 
                           chat_id=updates[i,]$message$chat$id)
            Sys.sleep(1)
          }
          
          if (grepl("Маленькая|Средняя|Большая", updates[i,]$message$text)) {
             bot$sendMessage (text= paste('Насколько выражена талия?'), 
                              reply_markup='{"keyboard":[["Прямая, почти не выражена"],["Объемная, есть круглый животик"],["Сравнительно узкая, ярко выражена"]]}',
                              parse_mode = 'markdown', 
                              chat_id=updates[i,]$message$chat$id)
            Sys.sleep(1)
          }
          
          if (grepl("Прямая, почти не выражена|Объемная, есть круглый животик|Сравнительно узкая, ярко выражена", updates[i,]$message$text)) {
            bot$sendMessage (text= paste('Что лучше подходит к описанию Ваших бедер?'), 
                             reply_markup='{"keyboard":[["Узкие, мальчишеские"],["Широкие, округлые"],["Ни узкие, ни широкие, что-то среднее"]]}',
                             parse_mode = 'markdown', 
                             chat_id=updates[i,]$message$chat$id)
            Sys.sleep(1)
          }
          
          
          
### Конец основной логики бота            
        }
      }
      updates[i,]$is.answered <- TRUE
    }
  old.updates <- updates
}







