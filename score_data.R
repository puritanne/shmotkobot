Sys.setlocale("LC_ALL", "Ru_Ru")

productList <- readRDS("data/productList.rds")

#######
# Размечаем данные
# model_tops

model_tops <- as.data.frame(productList$p_shirts)

model_tops$prop_design <- NA
model_tops$prop_design <- ifelse(grepl("Ультрамодный|оригин|Оригин", model_tops$lamoda_desc), 1, model_tops$prop_design)

## fit
model_tops$prop_tailored.fit <- NA
model_tops$prop_tailored.fit <- ifelse(grepl("притален|Притален", model_tops$lamoda_desc), 1, model_tops$prop_tailored.fit)

model_tops$prop_free.fit <- NA
model_tops$prop_free.fit <- ifelse(grepl("свободн", model_tops$lamoda_desc), 1, model_tops$prop_free.fit)

model_tops$prop_flared.fit <- NA
model_tops$prop_flared.fit <- ifelse(grepl("расклшенный крой|расклешенного|расширенная книзу|расширенный книзу|расширенный к низу|расширенного к низу|по низу|расклешенный книзу|расклешенный к низу|баска", model_tops$lamoda_desc), 1, model_tops$prop_flared.fit)

model_tops$prop_straight.fit <- NA
model_tops$prop_straight.fit <- ifelse(grepl("прям|Прям", model_tops$lamoda_desc), 1, model_tops$prop_straight.fit)

# подплечники

model_tops$prop_shoulder.pads <- NA
model_tops$prop_shoulder.pads <- ifelse(grepl("подплечник", model_tops$lamoda_desc), 1, model_tops$prop_shoulder.pads)

model_tops$prop_low.shoulder <- NA
model_tops$prop_low.shoulder <- ifelse(grepl("заниженная линия плеч", model_tops$lamoda_desc), 1, model_tops$prop_low.shoulder)


#рукава

model_tops$prop_cuff <- NA
model_tops$prop_cuff <- ifelse(grepl("манжет", model_tops$lamoda_desc), 1, model_tops$prop_cuff)
model_tops$prop_cuff <- ifelse(grepl("без рукав", model_tops$lamoda_desc), 0, model_tops$prop_cuff)
model_tops$prop_cuff <- ifelse(grepl("эластичные манжеты|эластичными манжетами|резинки на манжетах", model_tops$lamoda_desc), 0, model_tops$prop_cuff)

model_tops$prop_wide.sleeve <- NA
model_tops$prop_wide.sleeve <- ifelse(grepl("фонарик|объемные рукава|объемный рукав|расклешенные рукава", model_tops$lamoda_desc), 1, model_tops$prop_wide.sleeve)


model_tops$prop_no.sleeve <- NA
model_tops$prop_no.sleeve <- ifelse(grepl("короткие рукава|без рукав", model_tops$lamoda_desc), 1, model_tops$prop_no.sleeve)

model_tops$prop_reglan.sleeve <- NA
model_tops$prop_reglan.sleeve <- ifelse(grepl("реглан|летучая мышь|Летучая мышь", model_tops$lamoda_desc), 1, model_tops$prop_no.sleeve)


#вырез и воротник

model_tops$prop_decollete <- NA
model_tops$prop_decollete <- ifelse(grepl("v-вырез|лодочка|бретел|V-образн.* вырез|v-образн.* вырез|Воротник-шаль|запах|запАх|отложной|Отложной", model_tops$lamoda_desc), 1, model_tops$prop_decollete)

model_tops$prop_no.stand.collar <- NA
model_tops$prop_no.stand.collar <- ifelse(grepl("стойка", model_tops$lamoda_desc), 0, 1)


#талия

model_tops$prop_acc.waist <- NA
model_tops$prop_acc.waist <- ifelse(grepl("пояс|ремень", model_tops$lamoda_desc), 1, model_tops$prop_acc.waist)

model_tops$prop_raised.waist <- NA
model_tops$prop_raised.waist <- ifelse(grepl("под грудью", model_tops$lamoda_desc), 1, model_tops$prop_raised.waist)


#ткани

model_tops$prop_textures <- NA
model_tops$prop_textures <- ifelse(grepl("фактур|текстур|жат|ажур|кружев|лапша|рюш|жабо|вставк|камн|окантовк|заплатк|отделк|вышивка|люверс", model_tops$lamoda_desc), 1, model_tops$prop_textures)

model_tops$prop_soft.fabric <- NA
model_tops$prop_soft.fabric <- ifelse(grepl("шелк|атлас|трикотаж|хлопок|струящ|мягк", model_tops$lamoda_desc), 1, model_tops$prop_soft.fabric)

model_tops$prop_elastic.fabric <- NA
model_tops$prop_elastic.fabric <- ifelse(grepl("Эластан|эластан|хлопок|шерсть|шелк|шифон|Хлопок|Шерсть|Шелк|Шифон", model_tops$lamoda_desc), 1, model_tops$prop_elastic.fabric)


#цвета

model_tops$prop_bright.color <- NA
model_tops$prop_bright.color <- ifelse(grepl("белый|черный", model_tops$lamoda_desc), 0, 1)

model_tops$prop_modest.color <- NA
model_tops$prop_modest.color <- ifelse(grepl("белый|голубой|бежевый", model_tops$lamoda_desc), 1, model_tops$prop_modest.color)

model_tops$prop_mono.color <- NA
model_tops$prop_mono.color <- ifelse(grepl("Однотонный", model_tops$lamoda_desc), 1, model_tops$prop_mono.color)


# драпировка и детали
model_tops$prop_drapery <- NA
model_tops$prop_drapery <- ifelse(grepl("драпировк", model_tops$lamoda_desc), 1, model_tops$prop_drapery)

model_tops$prop_chest.details <- NA
model_tops$prop_chest.details <- ifelse(grepl("карман", model_tops$lamoda_desc), 1, model_tops$prop_chest.details)
model_tops$prop_chest.details <- ifelse(grepl("Без карманов", model_tops$lamoda_desc), 0, model_tops$prop_chest.details)
model_tops$prop_chest.details <- ifelse(grepl("без карманов", model_tops$lamoda_desc), 0, model_tops$prop_chest.details)


model_tops$prop_layers.details <- NA
model_tops$prop_layers.details <- ifelse(grepl("оборк|драпировк|слой|складк", model_tops$lamoda_desc), 1, model_tops$prop_drapery)

model_tops$prop_geom.print <- NA
model_tops$prop_geom.print <- ifelse(grepl("клетк|Клетк", model_tops$lamoda_desc), 1, model_tops$prop_geom.print)


# длина

model_tops$prop_midhip.low <- NA
model_tops$prop_midhip.low <- ifelse(grepl("укороченная|Укороченная", model_tops$lamoda_desc), 0, model_tops$prop_midhip.low)

model_tops$prop_midhip.low <- NA
model_tops$prop_midhip.low <- ifelse(grepl("туника|Туника", model_tops$lamoda_desc), 1, model_tops$prop_midhip.low)
model_tops$prop_midhip.low <- ifelse(grepl("Длина 80 см|Длина 81 см|Длина 82 см|Длина 83 см|Длина 84 см|Длина 85 см|Длина 86 см|Длина 87 см|Длина 88 см|Длина 90 см|Длина 91 см|Длина 92 см|Длина 93 см|Длина 94 см|Длина 95 см", model_tops$lamoda_desc), 1, model_tops$prop_midhip.low)

# скоринг качества шмотки

model_tops$prop_shirt_color <- NA
model_tops$prop_shirt_color <- ifelse(grepl("белый|Белый|голубой|Голубой", model_tops$lamoda_desc), 1, model_tops$prop_shirt_color)

model_tops$prop_shirt_long_back <- NA
model_tops$prop_shirt_long_back <- ifelse(grepl("удлиненная спинка", model_tops$lamoda_desc), 1, model_tops$prop_shirt_long_back)

#Scoring

model_tops$score_pear <- ifelse(is.na(model_tops$prop_design), 0, as.numeric(model_tops$prop_design)) + 
  ifelse(is.na(model_tops$prop_tailored.fit), 0, as.numeric(model_tops$prop_tailored.fit)) +
  ifelse(is.na(model_tops$prop_cuff), 0, as.numeric(model_tops$prop_cuff)) +
  ifelse(is.na(model_tops$prop_decollete), 0, as.numeric(model_tops$prop_decollete)) +
  ifelse(is.na(model_tops$prop_acc.waist), 0, as.numeric(model_tops$prop_acc.waist)) +
  ifelse(is.na(model_tops$prop_textures), 0, as.numeric(model_tops$prop_textures)) +
  ifelse(is.na(model_tops$prop_bright.color), 0, as.numeric(model_tops$prop_bright.color)) +
  ifelse(is.na(model_tops$prop_wide.sleeve), 0, as.numeric(model_tops$prop_wide.sleeve)) +
  ifelse(is.na(model_tops$prop_chest.details), 0, as.numeric(model_tops$prop_chest.details)) + 
  ifelse(is.na(model_tops$prop_shoulder.pads), 0, as.numeric(model_tops$prop_shoulder.pads)) +
  ifelse(is.na(model_tops$prop_reglan.sleeve), 1, 0) +
  
  # скор за качество шмотки
  ifelse(is.na(model_tops$prop_shirt_color), 0, as.numeric(model_tops$prop_shirt_color)) +
  ifelse(is.na(model_tops$prop_shirt_long_back), 0, as.numeric(model_tops$prop_shirt_long_back)) +
  
  1  # за параметр длины 


model_tops$score_hourglass <- ifelse(is.na(model_tops$prop_tailored.fit), 0, as.numeric(model_tops$prop_tailored.fit)) +
  ifelse(is.na(model_tops$prop_acc.waist), 0, as.numeric(model_tops$prop_acc.waist)) +
  ifelse(is.na(model_tops$prop_decollete), 0, as.numeric(model_tops$prop_decollete)) +
  ifelse(is.na(model_tops$prop_wide.sleeve), 1, 0) +
  ifelse(is.na(model_tops$prop_reglan.sleeve), 0, as.numeric(model_tops$prop_reglan.sleeve)) +
  ifelse(is.na(model_tops$prop_mono.color), 0, as.numeric(model_tops$prop_mono.color)) +
  ifelse(is.na(model_tops$prop_elastic.fabric), 0, as.numeric(model_tops$prop_elastic.fabric))  +
  1 # за подплечники

model_tops$score_apple <- ifelse(is.na(model_tops$prop_free.fit), 0, as.numeric(model_tops$prop_free.fit)) + 
  ifelse(is.na(model_tops$prop_no.sleeve), 0, as.numeric(model_tops$prop_no.sleeve)) +
  ifelse(is.na(model_tops$prop_no.stand.collar), 0, as.numeric(model_tops$prop_no.stand.collar)) +  
  ifelse(is.na(model_tops$prop_decollete), 0, as.numeric(model_tops$prop_decollete)) +
  ifelse(is.na(model_tops$prop_modest.color), 0, as.numeric(model_tops$prop_modest.color)) +
  ifelse(is.na(model_tops$prop_drapery), 0, as.numeric(model_tops$prop_drapery)) +
  ifelse(is.na(model_tops$prop_midhip.low), 0, as.numeric(model_tops$prop_midhip.low)) +
  ifelse(is.na(model_tops$prop_soft.fabric), 0, as.numeric(model_tops$prop_soft.fabric)) +
  ifelse(is.na(model_tops$prop_raised.waist), 0, as.numeric(model_tops$prop_raised.waist)) +
  ifelse(is.na(model_tops$prop_flared.fit), 0, as.numeric(model_tops$prop_flared.fit))



model_tops$score_inv.triangle <- ifelse(is.na(model_tops$prop_reglan.sleeve), 0, as.numeric(model_tops$prop_reglan.sleeve)) +
  ifelse(is.na(model_tops$prop_tailored.fit), 0, as.numeric(model_tops$prop_tailored.fit)) +
  ifelse(is.na(model_tops$prop_decollete), 0, as.numeric(model_tops$prop_decollete)) 

model_tops$score_column <- ifelse(is.na(model_tops$prop_tailored.fit), 0, as.numeric(model_tops$prop_tailored.fit)) +
  ifelse(is.na(model_tops$prop_decollete), 0, as.numeric(model_tops$prop_decollete)) +
  ifelse(is.na(model_tops$prop_wide.sleeve), 0, as.numeric(model_tops$prop_wide.sleeve)) +
  ifelse(is.na(model_tops$prop_flared.fit), 0, as.numeric(model_tops$prop_flared.fit)) +
  ifelse(is.na(model_tops$prop_shoulder.pads), 0, as.numeric(model_tops$prop_shoulder.pads)) +
  ifelse(is.na(model_tops$prop_chest.details), 0, as.numeric(model_tops$prop_chest.details)) + 
  ifelse(is.na(model_tops$prop_textures), 0, as.numeric(model_tops$prop_textures)) +
  ifelse(is.na(model_tops$prop_layers.details), 0, as.numeric(model_tops$prop_layers.details)) +
  ifelse(is.na(model_tops$prop_low.shoulder), 0, as.numeric(model_tops$prop_low.shoulder)) 

model_tops$score_rectangle <- ifelse(is.na(model_tops$prop_straight.fit), 0, as.numeric(model_tops$prop_straight.fit)) +
  ifelse(is.na(model_tops$prop_decollete), 0, as.numeric(model_tops$prop_decollete)) +
  ifelse(is.na(model_tops$prop_chest.details), 0, as.numeric(model_tops$prop_chest.details)) + 
  ifelse(is.na(model_tops$prop_shoulder.pads), 0, as.numeric(model_tops$prop_shoulder.pads)) +
  ifelse(is.na(model_tops$prop_soft.fabric), 0, as.numeric(model_tops$prop_soft.fabric)) +
  ifelse(is.na(model_tops$prop_geom.print), 0, as.numeric(model_tops$prop_geom.print)) 



# shortlists

shortlist.pear <- model_tops %>% filter(score_pear >= 6) %>% 
  filter(duplicated(description) == FALSE) %>%
  
  filter(!grepl("Размер:6", param)) %>%
  
  filter((prop_tailored.fit==1) | (prop_acc.waist==1)) %>%
  filter(is.na(prop_flared.fit)) %>% 
  filter(grepl("Размер:46", param)) %>%
  arrange(desc(score_pear)) 

shortlist.hourglass <- model_tops %>% filter(score_hourglass >= 6) %>%
  filter(duplicated(description) == FALSE) %>%
  
  filter(grepl("-лето", description)) %>%
  filter(!grepl("Размер:6", param)) %>%
  filter(prop_mono.color==1) %>%
  filter(grepl("Размер:46", param)) %>%
  arrange(desc(score_hourglass))

# Возвращаем в productList

productList$p_blouse <- model_tops
saveRDS(productList, "data/productList.rds")

##
shortlist.pear.jshirts <- shortlist.pear %>% filter(grepl("Рубашка джинсовая", description))
shortlist.pear.shirts <- shortlist.pear %>% 
  mutate(score_pear = ifelse(grepl("Цвет: голубой", description), score_pear+1, score_pear))
  
shortlist.pear.blouses <- shortlist.pear %>% filter(grepl("Блуза", description))
