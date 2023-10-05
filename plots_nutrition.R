# Plots using 2023 nutrition month data and DHS data
#
# Get data
source("C:/AP/Dropbox/data/WHO_Colombo/manjula/mcn/nutrition_data.R")

my_bar <- function(x) {
  ggplot(data = ntmnth_age,
         mapping = aes(x = year, y = {{x}}, fill = agegp)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_text(
      aes(label = {{x}}),
      vjust = -0.2,
      color = "black",
      position = position_dodge(0.9),
      size = 5
    )
}



camcorder::gg_record(
  dir = 'imgs',
  width = 12,
  height = 12 * 9 / 16,
  dpi = 300,
  bg = 'white'
  # Makes sure background of plot is actually white, not transparent
)
# stunt_age_nm
my_bar(stunt) + 
  theme_light(base_size = 16) + 
  labs(x = "", y = "Percent", fill = "", title = "") +
  scale_y_continuous(limits = c(0, 14)) +
  theme(legend.position = "bottom") + 
  scale_fill_manual("", values = c("Infants" = "#9ECAE1", "1-2 years" = "#2171B5", "3-5 years" = "#08306B"))

#waste_age_nm
my_bar(waste) + 
  theme_light(base_size = 16) + 
  labs(x = "", y = "Percent", fill = "", title = "") +
  scale_y_continuous(limits = c(0, 14)) +
  theme(legend.position = "bottom") +
  scale_fill_manual("", values = c("Infants" = "#9ECAE1", "1-2 years" = "#2171B5", "3-5 years" = "#08306B"))

# dhs_trend
ggplot(data = dhs_sdg, mapping = aes(x = year, y = Percent, group = ind)) +
  theme_light(base_size = 16) +   
  geom_line(aes(colour = ind), size = 2) +
  labs(x = "", y = "Percent", fill = "", title = "") +
  geom_point(aes(colour = ind), size = 3) +
  scale_x_continuous(breaks= c(2000, 2007, 2016)) +
  scale_y_continuous(limits = c(0, 20)) +
  scale_color_manual("", values=c("#CC6666", "#9999CC"),
                     breaks = c("stunt", "waste"),
                     labels = c("Stunted", "Wasted")) +
  theme(legend.position = "bottom")+
  geom_text(
    aes(label = Percent),
    vjust = -0.3,
    color = "black",
    position = position_dodge(2),
    size = 5
  )

# nm_trend
ggplot(data = ntmnth_sdg, mapping = aes(x = year, y = Percent, group = ind)) +
  geom_line(aes(colour = ind), size = 2) +
  geom_point(aes(colour = ind), size = 3) +
  theme_light(base_size = 16) + 
  labs(x = "", y = "Percent", fill = "", title = "") +
  scale_x_continuous(breaks= c(2021, 2022, 2023))+
  scale_y_continuous(limits = c(0, 20)) +
  scale_color_manual(values=c("#CC6666", "#9999CC"),
                     name = "",
                     breaks = c("stunt", "waste"),
                     labels = c("Stunted", "Wasted")) +
  theme(legend.position = "bottom") +
  geom_text(
    aes(label = Percent),
    vjust = -0.3,
    color = "black",
    position = position_dodge(.3),
    size = 5
  )
