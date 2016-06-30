Sys.setlocale("LC_ALL", "Ru_Ru")

productList <- readRDS("data/productList.rds")

#######
# Размечаем данные
# p_tops

p_tops <- as.data.frame(productList$p_tshirts)

p_tops$prop_design <- NA
p_tops$prop_design <- ifelse(grepl("Ультрамодный|оригин|Оригин", p_tops$lamoda_desc), 1, p_tops$prop_design)

## fit
p_tops$prop_tailored.fit <- NA
p_tops$prop_tailored.fit <- ifelse(grepl("притален|Притален", p_tops$lamoda_desc), 1, p_tops$prop_tailored.fit)
p_tops$prop_tailored.fit <- ifelse(grepl("свободн", p_tops$lamoda_desc), 0, p_tops$prop_tailored.fit)

p_tops$prop_free.fit <- NA
p_tops$prop_free.fit <- ifelse(grepl("свободн", p_tops$lamoda_desc), 1, p_tops$prop_free.fit)

p_tops$prop_flared.fit <- NA
p_tops$prop_flared.fit <- ifelse(grepl("расклшенный крой|расклешенного|расширенная книзу|расширенный книзу|расширенный к низу|расширенного к низу|по низу|расклешенный книзу|расклешенный к низу|баска", p_tops$lamoda_desc), 1, p_tops$prop_flared.fit)

p_tops$prop_straight.fit <- NA
p_tops$prop_straight.fit <- ifelse(grepl("прям|Прям", p_tops$lamoda_desc), 1, p_tops$prop_straight.fit)

# подплечники

p_tops$prop_shoulder.pads <- NA
p_tops$prop_shoulder.pads <- ifelse(grepl("подплечник", p_tops$lamoda_desc), 1, p_tops$prop_shoulder.pads)

p_tops$prop_low.shoulder <- NA
p_tops$prop_low.shoulder <- ifelse(grepl("заниженная линия плеч", p_tops$lamoda_desc), 1, p_tops$prop_low.shoulder)


#рукава

p_tops$prop_cuff <- NA
p_tops$prop_cuff <- ifelse(grepl("манжет", p_tops$lamoda_desc), 1, p_tops$prop_cuff)
p_tops$prop_cuff <- ifelse(grepl("без рукав", p_tops$lamoda_desc), 0, p_tops$prop_cuff)
p_tops$prop_cuff <- ifelse(grepl("эластичные манжеты|эластичными манжетами|резинки на манжетах", p_tops$lamoda_desc), 0, p_tops$prop_cuff)

p_tops$prop_wide.sleeve <- NA
p_tops$prop_wide.sleeve <- ifelse(grepl("фонарик|объемные рукава|объемный рукав", p_tops$lamoda_desc), 1, p_tops$prop_wide.sleeve)


p_tops$prop_no.sleeve <- NA
p_tops$prop_no.sleeve <- ifelse(grepl("короткие рукава|без рукав", p_tops$lamoda_desc), 1, p_tops$prop_no.sleeve)

p_tops$prop_reglan.sleeve <- NA
p_tops$prop_reglan.sleeve <- ifelse(grepl("реглан|летучая мышь|Летучая мышь", p_tops$lamoda_desc), 1, p_tops$prop_no.sleeve)


#вырез и воротник

p_tops$prop_decollete <- NA
p_tops$prop_decollete <- ifelse(grepl("v-вырез|лодочка|бретел|V-образн.* вырез|v-образн.* вырез|Воротник-шаль|На запах", p_tops$lamoda_desc), 1, p_tops$prop_decollete)

p_tops$prop_no.stand.collar <- NA
p_tops$prop_no.stand.collar <- ifelse(grepl("стойка", p_tops$lamoda_desc), 0, 1)


#талия

p_tops$prop_acc.waist <- NA
p_tops$prop_acc.waist <- ifelse(grepl("пояс|ремень", p_tops$lamoda_desc), 1, p_tops$prop_acc.waist)

p_tops$prop_raised.waist <- NA
p_tops$prop_raised.waist <- ifelse(grepl("под грудью", p_tops$lamoda_desc), 1, p_tops$prop_raised.waist)


#ткани

p_tops$prop_textures <- NA
p_tops$prop_textures <- ifelse(grepl("фактур|текстур|жат|ажур|кружев|лапша|рюш|жабо|вставк|камн|окантовк", p_tops$lamoda_desc), 1, p_tops$prop_textures)

p_tops$prop_soft.fabric <- NA
p_tops$prop_soft.fabric <- ifelse(grepl("шелк|атлас|трикотаж|хлопок|струящ|мягк", p_tops$lamoda_desc), 1, p_tops$prop_soft.fabric)

p_tops$prop_elastic.fabric <- NA
p_tops$prop_elastic.fabric <- ifelse(grepl("Эластан|эластан|хлопок|шерсть|шелк|шифон|Хлопок|Шерсть|Шелк|Шифон", p_tops$lamoda_desc), 1, p_tops$prop_elastic.fabric)


#цвета

p_tops$prop_bright.color <- NA
p_tops$prop_bright.color <- ifelse(grepl("белый|черный", p_tops$lamoda_desc), 0, 1)

p_tops$prop_modest.color <- NA
p_tops$prop_modest.color <- ifelse(grepl("белый|голубой|бежевый", p_tops$lamoda_desc), 1, p_tops$prop_modest.color)

p_tops$prop_mono.color <- NA
p_tops$prop_mono.color <- ifelse(grepl("Однотонный", p_tops$lamoda_desc), 1, p_tops$prop_mono.color)


# драпировка и детали
p_tops$prop_drapery <- NA
p_tops$prop_drapery <- ifelse(grepl("драпировк", p_tops$lamoda_desc), 1, p_tops$prop_drapery)

p_tops$prop_chest.details <- NA
p_tops$prop_chest.details <- ifelse(grepl("карман", p_tops$lamoda_desc), 1, p_tops$prop_chest.details)
p_tops$prop_chest.details <- ifelse(grepl("Без карманов", p_tops$lamoda_desc), 0, p_tops$prop_chest.details)
p_tops$prop_chest.details <- ifelse(grepl("без карманов", p_tops$lamoda_desc), 0, p_tops$prop_chest.details)


p_tops$prop_layers.details <- NA
p_tops$prop_layers.details <- ifelse(grepl("оборк|драпировк|слой|складк", p_tops$lamoda_desc), 1, p_tops$prop_drapery)

p_tops$prop_geom.print <- NA
p_tops$prop_geom.print <- ifelse(grepl("клетк|Клетк", p_tops$lamoda_desc), 1, p_tops$prop_geom.print)


# длина

p_tops$prop_midhip.low <- NA
p_tops$prop_midhip.low <- ifelse(grepl("укороченная|Укороченная", p_tops$lamoda_desc), 0, p_tops$prop_midhip.low)

p_tops$prop_midhip.low <- NA
p_tops$prop_midhip.low <- ifelse(grepl("туника|Туника", p_tops$lamoda_desc), 1, p_tops$prop_midhip.low)
p_tops$prop_midhip.low <- ifelse(grepl("Длина 80 см|Длина 81 см|Длина 82 см|Длина 83 см|Длина 84 см|Длина 85 см|Длина 86 см|Длина 87 см|Длина 88 см|Длина 90 см|Длина 91 см|Длина 92 см|Длина 93 см|Длина 94 см|Длина 95 см", p_tops$lamoda_desc), 1, p_tops$prop_midhip.low)


#Scoring

p_tops$score_pear <- ifelse(is.na(p_tops$prop_design), 0, as.numeric(p_tops$prop_design)) + 
  ifelse(is.na(p_tops$prop_tailored.fit), 0, as.numeric(p_tops$prop_tailored.fit)) +
  ifelse(is.na(p_tops$prop_cuff), 0, as.numeric(p_tops$prop_cuff)) +
  ifelse(is.na(p_tops$prop_decollete), 0, as.numeric(p_tops$prop_decollete)) +
  ifelse(is.na(p_tops$prop_acc.waist), 0, as.numeric(p_tops$prop_acc.waist)) +
  ifelse(is.na(p_tops$prop_textures), 0, as.numeric(p_tops$prop_textures)) +
  ifelse(is.na(p_tops$prop_bright.color), 0, as.numeric(p_tops$prop_bright.color)) +
  ifelse(is.na(p_tops$prop_wide.sleeve), 0, as.numeric(p_tops$prop_wide.sleeve)) +
  ifelse(is.na(p_tops$prop_chest.details), 0, as.numeric(p_tops$prop_chest.details)) + 
  ifelse(is.na(p_tops$prop_shoulder.pads), 0, as.numeric(p_tops$prop_shoulder.pads)) +
  ifelse(is.na(p_tops$prop_reglan.sleeve), 1, 0) +
  1 # за параметр длины

p_tops$score_hourglass <- ifelse(is.na(p_tops$prop_tailored.fit), 0, as.numeric(p_tops$prop_tailored.fit)) +
  ifelse(is.na(p_tops$prop_acc.waist), 0, as.numeric(p_tops$prop_acc.waist)) +
  ifelse(is.na(p_tops$prop_decollete), 0, as.numeric(p_tops$prop_decollete)) +
  ifelse(is.na(p_tops$prop_wide.sleeve), 1, 0) +
  ifelse(is.na(p_tops$prop_reglan.sleeve), 0, as.numeric(p_tops$prop_reglan.sleeve)) +
  ifelse(is.na(p_tops$prop_mono.color), 0, as.numeric(p_tops$prop_mono.color)) +
  ifelse(is.na(p_tops$prop_elastic.fabric), 0, as.numeric(p_tops$prop_elastic.fabric))  +
  1 # за подплечники

p_tops$score_apple <- ifelse(is.na(p_tops$prop_free.fit), 0, as.numeric(p_tops$prop_free.fit)) + 
  ifelse(is.na(p_tops$prop_no.sleeve), 0, as.numeric(p_tops$prop_no.sleeve)) +
  ifelse(is.na(p_tops$prop_no.stand.collar), 0, as.numeric(p_tops$prop_no.stand.collar)) +  
  ifelse(is.na(p_tops$prop_decollete), 0, as.numeric(p_tops$prop_decollete)) +
  ifelse(is.na(p_tops$prop_modest.color), 0, as.numeric(p_tops$prop_modest.color)) +
  ifelse(is.na(p_tops$prop_drapery), 0, as.numeric(p_tops$prop_drapery)) +
  ifelse(is.na(p_tops$prop_midhip.low), 0, as.numeric(p_tops$prop_midhip.low)) +
  ifelse(is.na(p_tops$prop_soft.fabric), 0, as.numeric(p_tops$prop_soft.fabric)) +
  ifelse(is.na(p_tops$prop_raised.waist), 0, as.numeric(p_tops$prop_raised.waist)) +
  ifelse(is.na(p_tops$prop_flared.fit), 0, as.numeric(p_tops$prop_flared.fit))



p_tops$score_inv.triangle <- ifelse(is.na(p_tops$prop_reglan.sleeve), 0, as.numeric(p_tops$prop_reglan.sleeve)) +
  ifelse(is.na(p_tops$prop_tailored.fit), 0, as.numeric(p_tops$prop_tailored.fit)) +
  ifelse(is.na(p_tops$prop_decollete), 0, as.numeric(p_tops$prop_decollete)) 

p_tops$score_column <- ifelse(is.na(p_tops$prop_tailored.fit), 0, as.numeric(p_tops$prop_tailored.fit)) +
  ifelse(is.na(p_tops$prop_decollete), 0, as.numeric(p_tops$prop_decollete)) +
  ifelse(is.na(p_tops$prop_wide.sleeve), 0, as.numeric(p_tops$prop_wide.sleeve)) +
  ifelse(is.na(p_tops$prop_flared.fit), 0, as.numeric(p_tops$prop_flared.fit)) +
  ifelse(is.na(p_tops$prop_shoulder.pads), 0, as.numeric(p_tops$prop_shoulder.pads)) +
  ifelse(is.na(p_tops$prop_chest.details), 0, as.numeric(p_tops$prop_chest.details)) + 
  ifelse(is.na(p_tops$prop_textures), 0, as.numeric(p_tops$prop_textures)) +
  ifelse(is.na(p_tops$prop_layers.details), 0, as.numeric(p_tops$prop_layers.details)) +
  ifelse(is.na(p_tops$prop_low.shoulder), 0, as.numeric(p_tops$prop_low.shoulder)) 

p_tops$score_rectangle <- ifelse(is.na(p_tops$prop_straight.fit), 0, as.numeric(p_tops$prop_straight.fit)) +
  ifelse(is.na(p_tops$prop_decollete), 0, as.numeric(p_tops$prop_decollete)) +
  ifelse(is.na(p_tops$prop_chest.details), 0, as.numeric(p_tops$prop_chest.details)) + 
  ifelse(is.na(p_tops$prop_shoulder.pads), 0, as.numeric(p_tops$prop_shoulder.pads)) +
  ifelse(is.na(p_tops$prop_soft.fabric), 0, as.numeric(p_tops$prop_soft.fabric)) +
  ifelse(is.na(p_tops$prop_geom.print), 0, as.numeric(p_tops$prop_geom.print)) 

# shortlists

shortlist.pear <- p_tops %>% filter(score_pear >= 6) %>% 
  filter(duplicated(description) == FALSE) %>%
  
  filter(grepl("-лето", description)) %>%
  filter(!grepl("Размер:6", param)) %>%
  
  filter(prop_tailored.fit==1) %>%
  filter(is.na(prop_flared.fit)) %>% 
  arrange(desc(score_pear))

shortlist.hourglass <- p_tops %>% filter(score_hourglass >= 1) %>%
  filter(duplicated(description) == FALSE) %>%
  
  filter(grepl("-лето", description)) %>%
  filter(!grepl("Размер:6", param)) %>%
  filter(prop_mono.color==1) %>%
  arrange(desc(score_hourglass))
