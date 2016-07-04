library (dplyr)
library (rvest)
library (data.table)

source('config.R')

Sys.setlocale("LC_ALL", "Ru_Ru")

######
# CONSTS
#


####### 
# FUNCTIONS
#

# Получаем список товаров с Ламоды

get_products <- function(fileUrl) {
  data <- read.csv(url(fileUrl), header = TRUE, sep = ";", encoding="UTF-8")
  data
}

# Получаем описание товаров

get_product_descriptions <- function(product_df) {
  
  df <- product_df

  if (nrow(df)!=0) {
      for (i in 1:nrow(df)) {
        if (is.na(df[i,]$lamoda_desc)) {
          try({
              df[i,]$lamoda_desc <- read_html(as.character(df[i,]$url)) %>%
                html_nodes("div.ii-product__description-text") %>%
                html_text() %>% 
                toString()
              print (df[i,]$lamoda_desc)},
              silent=TRUE
              )
        }
        print (i)
      }
  }
  df
}

######
# Code

products <- get_products(fileUrl)
saveRDS(products, "products_all_lamoda.rds")

# Убираем дубликаты
p_norm <-  products %>% group_by(categoryId, real_category_name, typePrefix, vendorCode, name, description, picture, price, url) %>% 
  summarise(param=paste(param, collapse=" ")) %>%
  ungroup() 

# Выбираем только те шмотки, с которыми работаем

p_work <- p_norm %>% 
  filter(grepl("Женская одежда", categoryId)) %>%
  filter (grepl ("Блуза|Рубашка|Рубашка джинсовая|Футболка", typePrefix))

# Получаем описания

descriptions <- readRDS("data/descriptions.rds")

p_work <- merge (p_work, descriptions, by="url", all.x=TRUE)
p_work <- p_work %>% get_product_descriptions()

descriptions <- p_work %>%
  filter(!is.na(lamoda_desc)) %>%
  select(url, lamoda_desc)

saveRDS(descriptions, "data/descriptions.rds")

# Формируем итоговый список товаров

productList <- list()
productList[["p_shirts"]] <- p_work %>% 
  filter (typePrefix == "Рубашка")
productList[["p_jeans_shirts"]] <- p_work %>% 
  filter (typePrefix == "Рубашка джинсовая")
productList[["p_blouse"]] <- p_work %>% 
  filter (typePrefix == "Блуза")

saveRDS(productList, "data/productList.rds")

