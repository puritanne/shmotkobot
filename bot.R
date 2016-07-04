library("dplyr")
source("telegramr.r")

source('config.R')

Sys.setlocale("LC_ALL", "Ru_Ru")


# Setup my bot
bot <- TGBot$new(token = bot_token)
bot$getMe()
bot$set_default_chat_id(default_chat_id)

# Вспомогательные функции

get_address_book <- function() {
  address_book <- readRDS ("data/address_book.rds")
  address_book
}

save_address_book <- function(address_book) {
  saveRDS (address_book, "data/address_book.rds")
}

is.new.user <- function (chat_id) {
  address_book <- get_address_book()
  if (chat_id %in% address_book$chat_id) 
    is.new <- FALSE 
  else
    is.new <- TRUE
  is.new
}

add_new_user <- function (message) {
  address_book <- get_address_book()
  new.address_book <- rbind(
    address_book,
    data.frame(chat_id=as.character(message$chat$id),
               first_name=message$from$first_name,
               last_name=message$from$last_name,
               param_shoulders=NA, 
               param_breast=NA, 
               param_waist=NA, 
               param_hips=NA, 
               param_btype=NA)
  )
  save_address_book(new.address_book)
  new.address_book
}

get_user_body_type <- function (c_id) {
  address_book <- get_address_book()
  if (chat_id %in% address_book$chat_id) {
    user <- address_book %>% filter(chat_id == c_id)
    btype <- user$param_btype
  } else {
    btype <- NA   
  }
 btype
}

get_user_state <- function (c_id) {
  address_book <- get_address_book()
  if (c_id %in% address_book$chat_id) {
    user <- address_book %>% filter(chat_id == c_id)
    state <- user$user_state
  } else {
    state <- NA   
  }
   state
}

set_user_state <- function (c_id, state) {
  address_book <- get_address_book()
  if (c_id %in% address_book$chat_id) {
    address_book[address_book$chat_id==c_id,]$user_state <- state
    save_address_book(address_book)
  } 
  state
}

get_user_param_shoulders <- function (c_id) {
  address_book <- get_address_book()
  if (c_id %in% address_book$chat_id) {
    user <- address_book %>% filter(chat_id == c_id)
    p_shoulders <- user$param_shoulders
  } else {
    p_shoulders <- NA   
  }
  p_shoulders 
}

get_user_param_btype <- function (c_id) {
  address_book <- get_address_book()
  if (c_id %in% address_book$chat_id) {
    user <- address_book %>% filter(chat_id == c_id)
    p_btype <- user$param_btype
  } else {
    p_btype <- NA   
  }
  p_btype
}

set_user_param_shoulders <- function (c_id, p_shoulders) {
  address_book <- get_address_book()
  if (c_id %in% address_book$chat_id) {
    address_book[address_book$chat_id==c_id,]$param_shoulders <- p_shoulders
    save_address_book(address_book)
    res <- TRUE
  } else {
    res <- FALSE
  }
  res
}

set_user_param_breast <- function (c_id, p) {
  address_book <- get_address_book()
  if (c_id %in% address_book$chat_id) {
    address_book[address_book$chat_id==c_id,]$param_breast <- p
    save_address_book(address_book)
    res <- TRUE
  } else {
    res <- FALSE
  }
  res
}

set_user_param_waist <- function (c_id, p) {
  address_book <- get_address_book()
  if (c_id %in% address_book$chat_id) {
    address_book[address_book$chat_id==c_id,]$param_waist <- p
    save_address_book(address_book)
    res <- TRUE
  } else {
    res <- FALSE
  }
  res
}

set_user_param_hips <- function (c_id, p) {
  address_book <- get_address_book()
  if (c_id %in% address_book$chat_id) {
    address_book[address_book$chat_id==c_id,]$param_hips <- p
    save_address_book(address_book)
    res <- TRUE
  } else {
    res <- FALSE
  }
  res
}

set_user_param_btype <- function (c_id) {

  # /TODO: Сейчас здесь хардкод, а надо расчитывать по-нормальному
  
  address_book <- get_address_book()
  if (c_id %in% address_book$chat_id) {
    address_book[address_book$chat_id==c_id,]$param_btype <- "pear"
    save_address_book(address_book)
    res <- TRUE
  } else {
    res <- FALSE
  }
  res
}

get_history <- function() {
  history <- readRDS("data/items_history.rds")
  history
}

save_history <- function(history) {
  saveRDS(history, "data/items_history.rds")
}

get_product_list <- function() {
  productList <-  readRDS("data/productList.rds")
  productList
}

###############################################################
#
# MHmakeRandomString(n, length)
# function generates a random string random string of the
# length (length), made up of numbers, small and capital letters

MHmakeRandomString <- function(n=1, lenght=12)
{
  randomString <- c(1:n)                  # initialize vector
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                    lenght, replace=TRUE),
                             collapse="")
  }
  return(randomString)
}

#  > MHmakeRandomString()
#  [1] "XM2xjggXX19r"

###############################################################

get_next_item <- function(c_id, category, productList, history) {
  btype <- get_user_param_btype(c_id)
  df <- productList[[category]]
  
#  if (btype=="pear") { #/ сейчас всегда pear
    shortlist <- df %>% filter(score_pear >= 6) %>% 
      filter(duplicated(description) == FALSE) %>%
      
      filter(grepl("-лето", description)) %>%
      filter(!grepl("Размер:6", param)) %>%
      
      filter(prop_tailored.fit==1) %>%
      filter(is.na(prop_flared.fit)) %>% 
      filter(grepl("Размер:46", param)) %>%
      arrange(desc(score_pear)) 
#  }
    
    unsent <- shortlist %>% filter(!(url %in% history$url))
    
    unsent[1,]
    
}


#Главный цикл бота
offset <- NULL

repeat {
  updates <- bot$getUpdates(offset=offset)

  if (!is.null(nrow(updates)))
    for (i in 1:nrow(updates)) {
      
## Пока вообще не хэндлим инлайн-сообщения
      if (is.na(updates[i,]$message$chat$id))
        break # и это не работает, т.к. updates[i,]$message$chat$id может вообще не быть
      
### Основная логика бота
      current_chat_id <- updates[i,]$message$chat$id
      current_message <- updates[i,]$message$text
      user_state <- get_user_state(current_chat_id)
     
      cat(i, '\n')
      cat(current_chat_id, '\n')
      cat(current_message, '\n')
      cat(user_state, '\n')
      
      # State#1: Новый пользователь
      if (current_message == '/start') {
        user_state <- set_user_state(current_chat_id, "new")
        
        #сохранить пользователя
        if (is.new.user(current_chat_id)) {
          add_new_user(updates[i,]$message)
        }
        
        #отправить ему приветствие
        bot$sendMessage (text= paste('Привет! Я помогаю подбирать шмотки в интернет-магазинах. Сначала я задам несколько вопросов про тип фигуры и размер, а потом выберу из каталога Ламоды те вещи, которые, скорее всего, будут нормально сидеть.'), 
                         parse_mode = 'markdown', 
                         reply_markup='{}',
                         chat_id=current_chat_id)
        
        bot$sendMessage (text= paste('Пока я умею подбирать блузы, рубашки и топы, только для девочек.'), 
                         parse_mode = 'markdown', 
                         chat_id=current_chat_id)
        
        bot$sendMessage (text= paste('Лайкай те вещи, которые понравятся, чтобы я научился подбирать шмотки по вкусу.'), 
                         parse_mode = 'markdown', 
                         reply_markup='{}',
                         chat_id=current_chat_id)
        
        bot$sendMessage (text= paste('Начнём?'), 
                         reply_markup='{"keyboard":[["Да!"]],"one_time_keyboard":true,"resize_keyboard":true}',
                         parse_mode = 'markdown', 
                         chat_id=current_chat_id)
        
      }
      
      # State#2: Пользователь без настроек
      if  (user_state=="new" && current_message=="Да!") {
        bot$sendMessage (text= paste('Я задам тебе ряд вопросов'), 
                         parse_mode = 'markdown', 
                         reply_markup='{}',
                         chat_id=current_chat_id)
        
        user_state <- set_user_state(current_chat_id, "param_shoulders definition")
      }
        
        if  (user_state=="param_shoulders definition") {
          bot$sendMessage (text= paste('Какое строение у Ваших плеч?'), 
                           reply_markup='{"keyboard":[["Широкие, атлетические"],["Узкие, покатые"],["Ни широкие, ни узкие, что-то среднее"]],"one_time_keyboard":true,"resize_keyboard":true}',
                           parse_mode = 'markdown', 
                           chat_id=current_chat_id)
          user_state <- set_user_state(current_chat_id, "param_shoulders definition waiting")
        }
      
      if (user_state=="param_shoulders definition waiting" && grepl("Широкие, атлетические|Узкие, покатые|Ни широкие, ни узкие, что-то среднее", current_message)) {
        set_user_param_shoulders(current_chat_id, current_message)
        user_state <- set_user_state(current_chat_id, "param_breast definition")
      } 
      
      if  (user_state=="param_breast definition") {
        bot$sendMessage (text= paste('Какого объема Ваша грудь?'), 
                         reply_markup='{"keyboard":[["Маленькая"],["Средняя"],["Большая"]],"one_time_keyboard":true,"resize_keyboard":true}',
                         parse_mode = 'markdown', 
                         chat_id=current_chat_id)
        user_state <- set_user_state(current_chat_id, "param_breast definition waiting")
      }
      
      if (user_state=="param_breast definition waiting" && grepl("Маленькая|Средняя|Большая", current_message)) {
        set_user_param_breast(current_chat_id, current_message)
        user_state <- set_user_state(current_chat_id, "param_waist definition")
      } 
      
      if  (user_state=="param_waist definition") {
        bot$sendMessage (text= paste('Насколько выражена талия?'), 
                         reply_markup='{"keyboard":[["Прямая, почти не выражена"],["Объемная, есть круглый животик"],["Сравнительно узкая, ярко выражена"]],"one_time_keyboard":true,"resize_keyboard":true}',
                         parse_mode = 'markdown', 
                         chat_id=current_chat_id)
        user_state <- set_user_state(current_chat_id, "param_waist definition waiting")
      }
      
      if (user_state=="param_waist definition waiting" && grepl("Прямая, почти не выражена|Объемная, есть круглый животик|Сравнительно узкая, ярко выражена", current_message)) {
        set_user_param_waist(current_chat_id, current_message)
        user_state <- set_user_state(current_chat_id, "param_hips definition")
      } 
      
      if  (user_state=="param_hips definition") {
        bot$sendMessage (text= paste('Что лучше подходит к описанию Ваших бедер?'), 
                         reply_markup='{"keyboard":[["Узкие, мальчишеские"],["Широкие, округлые"],["Ни узкие, ни широкие, что-то среднее"]],"one_time_keyboard":true,"resize_keyboard":true}',
                         parse_mode = 'markdown', 
                         chat_id=current_chat_id)
        user_state <- set_user_state(current_chat_id, "param_hips definition waiting")
      }
      
      if (user_state=="param_hips definition waiting" && grepl("Узкие, мальчишеские|Широкие, округлые|Ни узкие, ни широкие, что-то среднее", current_message)) {
        set_user_param_hips(current_chat_id, current_message)
        user_state <- set_user_state(current_chat_id, "params defined")
      } 
      
      if  (user_state=="params defined") {
        bot$sendMessage (text= paste('Спасибо, мы все поняли!'), 
                         reply_markup='{}',
                         parse_mode = 'markdown', 
                         chat_id=current_chat_id)
        set_user_param_btype(current_chat_id)
        user_state <- set_user_state(current_chat_id, "ready")
      }
# /TODO: Дописать прием и обсчет всех параметров: м/ж, рост, тип фигуры
      
      # State#3: Начинаем подбор шмоток
      
      if  (user_state=="ready") {
        bot$sendMessage (text= paste('Какие вещи будем подбирать?'), 
                         reply_markup='{"keyboard":[["👕 Блузы и рубашки"]],"one_time_keyboard":true,"resize_keyboard":true}',
                         parse_mode = 'markdown', 
                         chat_id=current_chat_id)
      }
      
      if (user_state=="ready" && grepl("Блузы и рубашки", current_message)) {
        user_state <- set_user_state(current_chat_id, "p_tops") }
      
      if (user_state=="p_tops" && grepl("Дальше", current_message)) {
        user_state <- set_user_state(current_chat_id, "p_tops") 
        
        category <- "p_tops"
        
          history <- get_history()
        
        
          productList <- get_product_list()
        
        
        item <- get_next_item(current_chat_id, "p_tops", productList, history)
        
        if (!is.na(item[1,]$url)) {
          download.file(as.character(item[1,]$picture), "pic.jpeg")
          bot$sendPhoto('pic.jpeg', caption=item[1,]$score_pear, chat_id=updates[i,]$message$chat$id)
          
          inline_keyboard <- paste('{"inline_keyboard": [[{"text": "Смотреть на сайте","url": "', item[1,]$url,'"}],[{"text": "В избранное","callback_data": "like"}],[{"text": "Не ношу такое","callback_data": "no"}]]}', sep='')

           bot$sendMessage (text=item[1,]$name, 
                            parse_mode = 'markdown', 
                            reply_markup=inline_keyboard,
                            chat_id=updates[i,]$message$chat$id)
          bot$sendMessage (text='1', 
                           parse_mode = 'markdown', 
                           reply_markup='{"keyboard":[["Дальше"]],"resize_keyboard":true}',
                           chat_id=updates[i,]$message$chat$id)
          
          a <- data.frame(
            id = as.numeric(updates[i,]$message$message_id),
            chat_id=as.numeric(updates[i,]$message$chat$id),
            url=item[1,]$url
          )
          
          history <- rbind(history, a)
          save_history(history)
        }
      }
        
      
### Конец основной логики бота
      
    }
  offset <- max(updates$update_id) + 1
}