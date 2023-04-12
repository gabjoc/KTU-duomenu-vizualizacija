library(readr)
library(dplyr)
library(ggplot2)

data = read_csv("../data/lab_sodra.csv")
summary(data)

#1 uzduotis

# duomenu atfiltravimas pagal mano ecoActCode 
dataCode = data %>% 
  filter(ecoActCode == 471100)
summary(dataCode)

# grafikas
dataCode %>%
  ggplot(aes(x = avgWage)) +
  geom_histogram(bins = 100, fill = "#CC99CC", color = 'black') + 
  scale_y_continuous(name = "Count", breaks = seq(0, 1000, 100)) +
  scale_x_continuous(name = "Average wage", breaks = seq(0, 6500, 500)) +
  theme_classic() +
  labs(title = "Average wage of employees")

#2 uzduotis

# atrenkamos 5 imones, kuriu vidutinis atlyginimas yra didziausias
top5 = dataCode %>%
  group_by(name) %>%
  summarize(avgWage = max(avgWage)) %>%
  arrange(desc(avgWage)) %>%
  head(5)

# atrenkame tu imoniu duomenis, pakeiciame "month" stulpelio reiksme, isskirdami tik menesi
top5 = dataCode %>%
  filter(name %in% top5$name) %>%
  mutate(month = substr(month, 5,6))

# grafikas
top5 %>%
  ggplot(aes(x = month, y = avgWage, group = name)) + 
  geom_line(aes(color = name)) +
  theme_classic() +
  labs(title = "Top 5 companies with most average wage during 2022", x = "Months", y = "Average wage") +
  scale_color_manual(values = c("#00BFFF","#FF7F50", "#DAA520", "#FF69B4", "#7FFF00"),name = "Company names") +
  scale_y_continuous(breaks = seq(0, 6500, 1000))

#3 uzduotis

# isrenkame didziausia skaiciu apdraustu darbuotoju imonese
# grafikas
top5 %>%
  group_by(name) %>%
  summarize(insured = max(numInsured)) %>%
  arrange(insured) %>%
  ggplot(aes(x = reorder(name, -insured), y = insured, group = name)) +
  geom_col(aes(fill = name)) +
  theme_classic() +
  labs(title = "Number of insured employees in 2022", x = "Company", y = "Insured employees") +
  theme(axis.text.x = element_text(size = 7)) +
  scale_fill_manual(values = c("#00BFFF","#7FFF00", "#DAA520", "#FF69B4", "#FF7F50"),name="Company names") +
  scale_y_continuous(breaks = seq(0, 150, 10))
