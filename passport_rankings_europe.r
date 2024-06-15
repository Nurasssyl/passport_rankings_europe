library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)

# Подготовка данных о паспортных рейтингах
passport_data <- data.frame(
  country = c("France", "Germany", "Italy", "Spain", "Austria", "Finland", 
              "Ireland", "Luxembourg", "Netherlands", "Sweden", "Belgium", 
              "Denmark", "United Kingdom", "Norway", "Portugal", "Switzerland", 
              "Greece", "Malta", "Czechia", "Hungary", "Poland", "Lithuania", 
              "Estonia", "Latvia", "Slovakia", "Slovenia", "Iceland", "Croatia", 
              "Liechtenstein", "Cyprus", "Bulgaria", "Romania", "Serbia", 
              "Montenegro", "North Macedonia", "Albania", "Bosnia and Herz.", 
              "Georgia", "Moldova", "Russia", "Turkey", "Kazakhstan", "Armenia", 
              "Azerbaijan", "Belarus", "Kosovo", "Ukraine"),
  visa_free_score = c(194, 194, 194, 194, 193, 193, 193, 193, 193, 193, 192, 
                      192, 192, 191, 191, 191, 191, 190, 190, 190, 190, 188, 
                      187, 186, 186, 186, 185, 184, 182, 181, 179, 179, 139, 
                      127, 127, 124, 123, 122, 122, 120, 118, 78, 69, 72, 81, 76, 143)
)

# Загрузка географических данных для Европы
europe <- ne_countries(scale = "medium", returnclass = "sf")

# Фильтрация для включения только европейских стран и европейских частей России, Турции, Казахстана и кавказских стран
europe <- europe %>%
  filter(
    region_un == "Europe" |
      name %in% c("Russia", "Turkey", "Kazakhstan", "Armenia", "Azerbaijan", "Georgia", 
                  "Belarus", "Kosovo", "Ukraine", "Cyprus", "Bosnia and Herz.")
  )

# Обрезка географических данных для отображения карты Европы
europe <- st_crop(europe, xmin = -25, xmax = 60, ymin = 10, ymax = 75)
europe_ratings <- europe %>%
  left_join(passport_data, by = c("name" = "country"))


# Визуализация карты с рейтингами паспортов
p <- ggplot(data = europe_ratings) +
  geom_sf(aes(fill = visa_free_score), color = "white") +
  scale_fill_gradientn(
    name = "Visa-free score",
    colors = c("green", "yellow", "orange", "red"),
    values = scales::rescale(c(194, 190, 150, 120)),  # Рескейлинг значений
    na.value = "grey50"
  ) +
  coord_sf(xlim = c(-25, 60), ylim = c(10, 75), expand = FALSE) +
  theme_minimal() +
  labs(
    title = "Рейтинг паспортов в Европе",
    fill = "Безвизовые страны"
  ) +
  theme(
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 8)
  )


ggsave("C:/Users/Admin/Рабочий стол/R/passport_rankings_europe.png", plot = p, width = 10, height = 8)
