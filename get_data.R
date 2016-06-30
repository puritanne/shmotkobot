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
  data$lamoda_desc <- NA
  data
}

update_products <- function(old.data, fileUrl) {
  new.data <- get_products(fileUrl)
  
  keys <- c("id")
  df.old <- data.table(old.data, key=keys)
  df.new <- data.table(new.data, key=keys)
  
  df.added.records <- df.new[df.old, isOld := 1L] %>% filter(is.na(isOld))
  
  df.deleted.records <- df.old[df.new, isSaved := 1L] %>% filter(is.na(isSaved))
  df.deleted.records[,isSaved:=NULL]
  
  data <- rbind (old.data, df.added.records) %>%
    subset(!(id %in% df.deleted.records$id))
  
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

# init
# only run once
products <- get_products(fileUrl)
saveRDS(products, "data/products_all_lamoda.rds")

#work
old.products <- readRDS("data/products_all_lamoda.rds")

products <- update_products(old.products, fileUrl)
saveRDS(products, "data/products_all_lamoda.rds")

productList <- list()
productList[["p_tops"]] <- products %>% 
  filter (grepl ("Блузки и кофточки", categoryId)) %>%
  filter(duplicated(url) == FALSE)
productList[["p_tshirts"]] <- products %>% 
  filter (grepl ("Женская одежда/Футболки и топы", categoryId)) %>%
  filter(duplicated(url) == FALSE)

productList$p_tops[1:nrow(productList$p_tops),] <- productList$p_tops[1:nrow(productList$p_tops),] %>% get_product_descriptions()
productList$p_tshirts[1:nrow(productList$p_tshirts),] <- productList$p_tshirts[1:nrow(productList$p_tshirts),] %>% get_product_descriptions()

saveRDS(productList, "data/productList.rds")


 