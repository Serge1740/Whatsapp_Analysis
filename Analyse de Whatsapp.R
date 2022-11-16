library(rwhatsapp)
library(lubridate)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(RColorBrewer)
library(knitr)
library(ggplot2); theme_set(theme_minimal())
library(esquisse)
library(ggimage)
library(tidytext)
library(stopwords)
library(tidyverse)
library(tidymodels)
library(gganimate)
library(wordcloud2)

#Importation des donn??es
chat <- rwa_read("C:/Users/GANHOUNOUTO JOANES/Documents/Discussion_WhatsApp.txt")%>% 
  filter(!is.na(author))

paleta.saison <- brewer.pal(8,"Dark2")

#On cache les noms des membres du groupe
levels(chat$author) <- paste("member", 1:length(unique(chat$author)), sep = "_")

## Regroupement par groupe


myChat <- chat %>%
  mutate(day = date(time)) %>%
  mutate(
    saison = case_when(
      day <= dmy(31122021) ~ "Periode sans soucis",
      day >= dmy(01012022) & day <= dmy(17012022) ~ "Pre Soutenance",
      day >= dmy(18012022) & day <= dmy(10022022) ~ "Soutenance",
      day >= dmy(11022022) & day <= dmy(31032022) ~ "Fevrier - Mars",
      day >= dmy(01042022) & day <= dmy(30042022) ~ "Avril",
      day >= dmy(01052022) & day <= dmy(31052022) ~ "Mai",
      day >= dmy(01062022) & day <= dmy(30062022) ~ "Juin",
      day >= dmy(01072022) & day <= dmy(31122022) ~ "Deuxieme Semestre")) %>%
  mutate( saison = factor(saison)) %>%
  filter(!is.na(author))%>% 
  mutate(count_character= nchar(text),words= nchar(gsub('[^ ]+', '',text))+1)


# Evolution de la fr??quence de messages

p <- myChat %>%
  group_by(saison) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n, fill=saison)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.saison) +
  ylab("Nombre de messages") + xlab("Date") +
  labs(title = "Nombre de messages par jour",
       subtitle = "Fr??quence par saison") +
  theme_minimal() +
  theme( legend.title = element_blank(),
         legend.position = "bottom",
         plot.title = element_text(hjust = 0.5, size = 20),
         plot.subtitle = element_text(hjust = 0.5))

p_anim <-  p +
  transition_time(day) +
  shadow_mark(past = TRUE,
              future = FALSE) +
  enter_fade() +
  exit_fade()


p


## Par jour sur toute la p??riode

chat %>%
  mutate(day = date(time)) %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_bar(stat = "identity") +
  ylab("") + xlab("") +
  ggtitle("Messages per day")


## Par jour de semaine

myChat %>%
  mutate( num_jsem = wday(day),
          nom_jsem = weekdays(day)) %>%
  group_by(saison, num_jsem, nom_jsem) %>%
  count() %>%
  ggplot(aes(x = reorder(nom_jsem, -num_jsem), y = n, fill = saison)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = paleta.saison) +
  ylab("") + xlab("") +
  coord_flip() +
  labs(title = "Nombre de messages par jour de la semaine",
       subtitle = "Fr??quence selon les saisons de l'ann??e") +
  theme_minimal() +
  theme( legend.title = element_blank(),
         legend.position = "bottom",
         plot.title = element_text(hjust = 0.5, size = 20),
         plot.subtitle = element_text(hjust = 0.5))


## Message par heure et par semaine

jours_semaine <- c("dimanche", "lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi" )
names(jours_semaine) <- 1:7
myChat %>%
  mutate( heure = hour(time),
          num_jsem = wday(day),
          nom_jsem = weekdays(day)) %>%
  count(saison, num_jsem, nom_jsem, heure) %>%
  ggplot(aes(x = heure, y = n, fill = saison)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = paleta.saison) +
  ylab("Nombre de messages") + xlab("Horaire") +
  labs(title = "Nombre de messages par heure de la journC)e",
       subtitle = "FrC)quence selon les saisons de l'annC)e") +
  facet_wrap(~num_jsem, ncol=7, labeller = labeller(num_jsem = jours_semaine)) +
  theme_minimal() +
  theme( legend.title = element_blank(),
         legend.position = "bottom",
         panel.spacing.x = unit(0.0, "lines"),
         axis.text.x = element_text(size = 5),
         plot.title = element_text(hjust = 0.5, size = 20),
         plot.subtitle = element_text(hjust = 0.5))


# Les ??mojis les plus utilis??s

plotEmojis <- myChat %>%
  unnest(c(emoji, emoji_name))  %>%
  mutate(emoji = str_sub(emoji, end = 1)) %>%
  count(emoji, emoji_name) %>%
  top_n(20, n) %>%
  arrange(desc(n)) %>%
  mutate( emoji_url = map_chr(emoji,
                              ~paste0("https://abs.twimg.com/emoji/v2/72x72/",
                                      as.hexmode(utf8ToInt(.x)),".png")))

# graphe des emojis les plus utilis??s

plotEmojis <- myChat %>%
  unnest(c(emoji, emoji_name))  %>%
  mutate(emoji = str_sub(emoji, end = 1)) %>%
  count(emoji, emoji_name) %>%
  top_n(20, n) %>%
  arrange(desc(n)) %>%
  mutate( emoji_url = map_chr(emoji,
                              ~paste0("https://abs.twimg.com/emoji/v2/72x72/",
                                      as.hexmode(utf8ToInt(.x)),".png")))


plotEmojis %>%
  ggplot(aes(x = reorder(emoji_name, n), y = n)) +
  geom_col(aes(fill = n), show.legend = FALSE, width = .2) +
  geom_point(aes(color = n), show.legend = FALSE, size = 3) +
  geom_image(aes(image = emoji_url), size = .045) +
  scale_fill_gradient(low = "blue",high = "red") +
  scale_color_gradient(low = "#2b83ba",high = "#d7191c") +
  ylab("Nombre de fois oC9 l'emoji a C)tC) utilisC)") +
  xlab("Emoji et signification") +
  labs(title = "Emojis les plus gC)nC)ralement utilisC)s",
       subtitle = "Emojis les plus utilisC)s par tous") +
  coord_flip() +
  theme_minimal() +
  theme( legend.title = element_blank(),
         legend.position = "bottom",
         plot.title = element_text(hjust = 0.5, size = 20),
         plot.subtitle = element_text(hjust = 0.5))



# Celui qui a envoy?? le plus de message!


myChat %>%
  mutate(day = date(time)) %>%
  group_by(saison)%>%
  count(author) %>%
  slice_max(n, prop = 0.1)  %>%
  ggplot(aes(x = reorder(author, n), y = n, fill=saison)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=paleta.saison) +
  ylab("Nombre total de messages") + xlab("Utilisateurs") +
  coord_flip() +
  labs(title = "Nombre total de messages par utilisateur",
       subtitle = "Qui est le plus communicatif ? Frequence par saison") +
  theme_minimal() +
  theme( legend.title = element_blank(),
         legend.position = "bottom",
         plot.title = element_text(hjust = 0.5, size = 20),
         plot.subtitle = element_text(hjust = 0.5))


# Les mots les plus utilis??s

#remover_word <- stopwords(language = "fr")

remover_word <- c("M??dias","omis","m??dia", stopwords("fr"))

myChat2 <- myChat%>%
  filter(myChat$text !="<M??dias omis>")
  

myChat %>%
  unnest_tokens(input = text, output = word) %>%
  filter(!word %in% remover_word) %>%
  count(word) %>%
  top_n(1, n) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(word,n), y = n, fill = n, color = n)) +
  geom_col(show.legend = FALSE, width = 0.1) +
  geom_point(show.legend = FALSE, size = 3) +
  scale_fill_gradient(low = "#2b83ba", high = "#d7191c") +
  scale_color_gradient(low = "#2b83ba", high = "#d7191c") +
  labs(title = "Mots les plus couramment utilisC)s dans les conversations") +
  xlab("Mots") +
  ylab("Nombre de fois que le mot a ??t?? utilis??") +
  coord_flip() +
  theme_minimal() +
  theme( legend.title = element_blank(),
         legend.position = "bottom",
         panel.spacing.x = unit(0.0, "lines"),
         plot.title = element_text(hjust = 0.5, size = 15))


# Lexical Diversity

to_remove <- c("M??dias","omis","m??dia", stopwords("fr"))

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) %>%
  ggplot(aes(x = reorder(author, lex_diversity),
             y = lex_diversity,
             fill = author)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(expand = (mult = c(0, 0, 0, 500))) +
  geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("unique words") +
  xlab("") +
  ggtitle("Lexical Diversity") +
  coord_flip()        


# output csv for using Word_cloud.py 

chat_py <- chat %>% 
  select(author,text)
write_csv(chat_py,"chat_py.csv") 


paleta.saison <- brewer.pal(8,"Accent")


myChat %>%
  mutate( num_jsem = wday(day),
          nom_jsem = weekdays(day)) %>%
  group_by(saison, num_jsem, nom_jsem) %>%
  count() %>%
  ggplot(aes(x = reorder(nom_jsem, -num_jsem), y = n, fill = saison)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = paleta.saison) +
  ylab("") + xlab("") +
  labs(title = "Nombre de messages par jour de la semaine",
       subtitle = "Fr??quence selon les saisons de l'ann??e") +
  theme_minimal() +
  theme( legend.title = element_blank(),
         legend.position = "bottom",
         plot.title = element_text(hjust = 0.5, size = 20),
         plot.subtitle = element_text(hjust = 0.5))







library(viridis)

myChat %>%
  mutate( num_jsem = wday(day),
          nom_jsem = weekdays(day)) %>%
  group_by(saison, num_jsem, nom_jsem) %>%
  count() %>%
  ggplot(aes(x = reorder(nom_jsem, -num_jsem), y = n, fill = saison)) +
  geom_bar(stat = "identity") +
  scale_color_viridis(discrete = TRUE, option = "C") +
  scale_fill_viridis(discrete = TRUE)+
  ylab("") + xlab("") +
  coord_flip() +
  labs(title = "Nombre de messages par jour de la semaine",
       subtitle = "FrC)quence selon les saisons de l'annC)e") +
  theme_minimal() +
  theme( legend.title = element_blank(),
         legend.position = "bottom",
         plot.title = element_text(hjust = 0.5, size = 20),
         plot.subtitle = element_text(hjust = 0.5))

  
  

# Total No of days in the chat
#Total Messages
  #No of smileys sent
  # No of media sent
  #No of links sent
  # No of people who left the group
  #No of delete messages
  #No of active members
  
  

  
# Message per day
# Avg words per message
  #Most active person
  #No of times group name was changed
  #Most active day of week
  
  
daysed<-c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")        

no_of_days_of_messages<- myChat %>% mutate(day=date(time)) %>% summarise(no=length(unique(day))) %>%pull(no)

most_oldest_date<-chat %>% mutate(day=date(time))%>% arrange(day) %>% slice(1) %>% select(Oldest=day) %>%pull(Oldest)

most_recent_date<-chat %>% mutate(day=date(time))%>% arrange(desc(day)) %>% slice(1) %>% select(Newest=day) %>%pull(Newest)

#total no of days
total_no_of_days<-as.numeric(most_recent_date-most_oldest_date)   

# total no of days with messages
no_of_days_of_no_messgaes<- as.numeric(total_no_of_days-no_of_days_of_messages) 

# % days without msg
percent_days_without_messages<-round(no_of_days_of_no_messgaes/total_no_of_days*100,2) 

#most active day
most_active_day<-myChat %>% mutate(date = date(time)) %>% count(date) %>% top_n(1) %>% pull(date) 

#most active day of week
most_active_day_of_week<-myChat %>% mutate(day = wday(as.Date(time),week_start = 1)) %>% count(day) %>% top_n(1) %>% pull(day)
most_active_day_of_week<-daysed[most_active_day_of_week]   

#total no of messages
total_no_of_messages <- myChat %>% count() 

# no of unique users
total_no_of_users<- n_distinct(myChat$author) 

# no of messages per day
messages_per_day<-as_tibble(total_no_of_messages/no_of_days_of_messgaes) 

# no of deleted messages  
no_of_smiley<-myChat%>% unnest(emoji) %>% count() #no of smileys
unique_smiley<-myChat %>%  unnest(emoji) %>% count(emoji, sort = TRUE) %>% count()
no_of_media <-myChat %>% filter(text == "<Media omitted>") %>% count() #no of media
no_of_links<- myChat%>% filter(str_detect(text,"www.")| str_detect(text,"http:")|str_detect(text,"https:")|str_detect(text,"youtu.be")) %>% count() #no of links



tab = data.frame(c(no_of_days_of_messages,no_of_days_of_messgaes))
tab

row.names(tab) = c("no_of_days_of_messages","no_of_days_of_messgaes")

library(formattable)
widget_formattable = formattable(tab)


#all these values can be easily combined into a table 