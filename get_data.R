library (dplyr)
library (rvest)
library (data.table)

Sys.setlocale("LC_ALL", "Ru_Ru")

######
# CONSTS
#

fileUrl <- 'http://export.admitad.com/ru/webmaster/websites/449834/products/export_adv_products/?user=puritanne&code=a61b66e9b9&feed_id=1001&format=csv'

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
          try(
              df[i,]$lamoda_desc <- read_html(as.character(df[i,]$url)) %>%
                html_nodes("div.ii-product__description-text") %>%
                html_text() %>% 
                toString(),
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
p_norm <-  products %>% group_by(categoryId, vendorCode, name, description, picture, price, url, lamoda_desc) %>% 
  summarise(param=paste(param, collapse=" ")) %>%
  ungroup() 

# Выбираем только те шмотки, с которыми работаем

p_work <- p_norm %>% 
  filter (grepl ("Блузки и кофточки", categoryId))

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
productList[["p_tops"]] <- p_work %>% 
  filter (grepl ("Блузки и кофточки", categoryId))

saveRDS(productList, "data/productList.rds")

