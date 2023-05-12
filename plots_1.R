source("C:/AP/Dropbox/data/WHO_Colombo/manjula/mcn/cr-ntmnth2022.R")
library(forcats)
ntmnth2022 <- ntmnth2022 %>% mutate(Underweight_p = Underweight*100/Chidren_Measured)
sl_underweight_p <- sum(ntmnth2022$Underweight)*100/sum(ntmnth2022$Chidren_Measured)

p1 <- ggplot(data = ntmnth2022, mapping = aes(x = District, y = Underweight_p))
p1 + geom_col()

p1 + geom_col() + coord_flip()

p1 + geom_col() + geom_hline(yintercept = sl_underweight_p) + coord_flip()


p2 <- ggplot(data = ntmnth2022, 
             mapping = aes(x = fct_reorder(District, Underweight_p), y = Underweight_p))

p2 + geom_col() + geom_hline(yintercept = sl_underweight_p) + coord_flip()
