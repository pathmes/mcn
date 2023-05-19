pdfpath <- here::here("sources", "2015_National_Nutrition_micronutruent_survey_Pregnant women_ (1).pdf")

library(pdftools)
library(tidyverse)


PDFInfo <- pdftools::pdf_info(pdfpath)
txt <- pdftools::pdf_text(pdfpath)

page1 <- txt[[44]]
page1split <- str_split(page1, "\\n", simplify = TRUE) %>% as.vector()

table42 <- tibble(RawText=page1split) %>% 
  filter(row_number()>=17) %>%
  filter(row_number()<=32) %>% 
  mutate(TextSquish=str_replace_all(RawText, "(\\.\\s)+", "\\.")) %>% 
  mutate(TextSquish=str_squish(TextSquish),
         TextSquish=str_replace_all(TextSquish, "\\(X\\)", "NA")) %>% 
  separate(TextSquish,
           into=c("District", "anemic", "tested", "id", "ida", "ferritin"),
           sep="\\s", convert=TRUE) 

table42 <- table42[1:25,2:7]

table42 <- table42 %>% mutate(anemicn = as.numeric(anemic))
table42 <- table42 %>% mutate(testedn = as.numeric(tested))
table42 <- table42 %>% mutate(anN = round(anemicn*testedn/100))
table42 %>% summarise(sum(anN)*100/sum(testedn))



