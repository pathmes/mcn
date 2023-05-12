pdfpath <- here::here("sources", "Nutrition Month 2022 Report Final.pdf")
library(pdftools)
library(tidyverse)


PDFInfo <- pdftools::pdf_info(pdfpath)
txt <- pdftools::pdf_text(pdfpath)

page1 <- txt[[16]]
page1split <- str_split(page1, "\\n", simplify = TRUE) %>% as.vector()



cmc <- page1split[12]
cmc <- str_replace(cmc, "Colombo MC", "ColomboMC")
page1split[12] <- cmc
ne <- page1split[23]
ne <- str_replace(ne, "Nuwara Eliya", "NuwaraEliya")
page1split[23] <- ne

ntmnth2022 <- tibble(RawText=page1split) %>%
  filter(row_number()>=8) %>%
  filter(row_number()<=28) %>% 
  mutate(TextSquish=str_replace_all(RawText, "(\\.\\s)+", "\\.")) %>% 
  mutate(TextSquish=str_squish(TextSquish),
         TextSquish=str_replace_all(TextSquish, "\\(X\\)", "NA")) %>% 
  separate(TextSquish,
           into=c("District", "Chidren_Measured", "Growth_Problem", 
                  "Underweight", "SAM", "MAM", "Stunted", "Overweight_Obese"),
           sep="\\s", convert=TRUE) 

ntmnth2022 <- ntmnth2022[ , 2:9]

write.csv(ntmnth2022, "ntmnth2022.csv")

rm(list = ls())

