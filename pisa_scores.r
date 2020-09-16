library(tidyverse)
library(ggplot2)
library(gridExtra)

df.raw <- read.csv(file ='Pisa scores 2013 - 2015 Data.csv', fileEncoding="UTF-8-BOM", na.strings = '..')
str(df.raw)

df <- df.raw[1:1161, c(1, 4, 7)] #select relevant rows and cols
  %>%  spread(key=Series.Code, value=X2015..YR2015.) 
  %>% rename(Maths = LO.PISA.MAT,                        
            Maths.F = LO.PISA.MAT.FE,
            Maths.M = LO.PISA.MAT.MA,
            Reading = LO.PISA.REA,
            Reading.F = LO.PISA.REA.FE,
            Reading.M = LO.PISA.REA.MA,
            Science = LO.PISA.SCI,
            Science.F = LO.PISA.SCI.FE,
            Science.M = LO.PISA.SCI.MA
) %>%
  drop_na()

#Math Score Ranking by Countries

ggplot(data=df,aes(x=reorder(Country.Name,Maths),y=Maths)) + 
  geom_bar(stat ='identity',aes(fill=Maths))+
  coord_flip() + 
  theme_grey() + 
  scale_fill_gradient(name="Maths Score Level")+
  labs(title = 'Ranking of Countries by Maths Score',
       y='Score',x='Countries')+ 
  geom_hline(yintercept = mean(df$Maths),size = 1, color = 'blue')

# Cambio df a formto long para hacer boxplot
df2 = df[,c(1,3,4,6,7,9,10)] %>%   # select relevant columns 
  pivot_longer(c(2,3,4,5,6,7),names_to = 'Score')
view(df2) 
# Veo la performance relativa entre hombres y mujeres
ggplot(data = df2, aes(x=Score,y=value, color=Score)) + 
  geom_boxplot()+
  scale_color_brewer(palette="Dark2") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Did males perform better than females?',
       y='Scores',x='Test Type')

# Separo el genero y el topico con  strsplit
S = numeric(408)     # create an empty vector
for (i in 1:length(df2$Score)) {
  S[i] = strsplit(df2$Score[i],".",fixed = TRUE)
}

# Armo df con dos columnas especificando genero y topico
df3 = S%>%unlist() %>% matrix(ncol = 2, byrow = TRUE)%>% as.data.frame()
view(df3)

# Combino df 2 y 3 
df4 = cbind(df2,df3) 
colnames(df4) = c('Country','Score','Value','Test','Gender')
df4$Score = NULL # pporque la columna score es redundante
view(df4)


ggplot(data = df4, aes(x=Test,y=Value, fill=Test)) + 
  geom_boxplot()+
  scale_fill_brewer(palette="Dark2") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = '',
       y='Scores',x='Test')+
  facet_wrap(~Gender,nrow = 1)

# Cambio el facet wrap por el tipo de prueba
ggplot(data = df4, aes(x=Gender,y=Value, fill=Gender)) + 
  geom_boxplot()+
  scale_fill_brewer(palette="Dark2") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Did males perform better than females?',
       y='Scores',x='')+
  facet_wrap(~Test,nrow = 1)


# Con df original creo columnas para ver la diferencia entre hombres y mujeres entre topicos

df = df %>% mutate(Maths.Diff = ((Maths.M - Maths.F)/Maths.F)*100,
                   Reading.Diff = ((Reading.M - Reading.F)/Reading.F)*100,
                   Science.Diff = ((Science.M - Science.F)/Science.F)*100,
                   Total.Score = Maths + Reading + Science,
                   Avg.Diff = (Maths.Diff+Reading.Diff+Science.Diff)/3
)
view(df)

# Visualizo los resultados
ggplot(data=df, aes(x=reorder(Country.Name, Maths.Diff), y=Maths.Diff)) +
  geom_bar(stat = "identity", aes(fill=Maths.Diff)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = mean(df$Maths.Diff), size=1, color="black") +
  scale_fill_gradient(name="% Difference Level") +
  labs(title="Are Males better at math?", x="", y="% difference from female")


df5 = df[,c(1,11,12,13)]
boxplot(df5$Maths.Diff, df5$Reading.Diff, df5$Science.Diff,
        main = 'Are Males better than Females?',
        names = c('Maths','Reading','Science'),
        col = 'green'
)

df6 = df5 %>% pivot_longer(c(2,3,4))
names(df6) = c('Country','Test Diff','Result')
View(df6)

new = rep(c('Maths', 'Reading', 'Science'),68) #to create a new   column indicating 'Maths', 'Reading' and 'Science'
df6 = cbind(df6, new)
names(df6) = c('Country','Test Diff','Result', 'Test')   #change column names
df6$'Test Diff' = NULL

df6$Test = factor(df6$Test, levels = c("Maths","Science","Reading"))  # Cambio el orden del legend

# Defino el outlier

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}   # define a function to detect outliers

# Creo un nuevo dataframe para indicar si existe un outlier con una columna adicional
df6$Country = as.character(df6$Country)
df7 <- df6  %>% group_by(as.character(Test)) %>% 
  mutate(is_outlier=ifelse(is_outlier(Result), Country, as.numeric(NA)))

# Solo quiero el aparezcan los paises con outliers, el resto los dejo con NA
df7$Country[which(is.na(df7$is_outlier))] <- as.numeric(NA)

ggplot(data = df7, aes(x=Test,y=Result, fill=Test)) + 
  geom_boxplot(alpha = 0.7,
               outlier.colour='red', 
               outlier.shape=19, 
               outlier.size=3, 
               width = 0.6)+
  geom_text(aes(label = Country), na.rm = TRUE, hjust = -0.2)+         
  theme_grey() +
  labs(title = 'Did males perform better than females?',
       y='% Difference from FEMALES',x='',
       caption  = 'Positive % Difference means Males performed \n better than Females and vice versa',
       subtitle = 'Based on PISA Score 2015') + 
  theme(axis.text=element_text(size=20),
        legend.text = element_text(size = 16), 
        legend.title = element_text(size = 16),
        
        legend.position = 'right', aspect.ratio = 1.4,
        plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(color = "Red", face = "italic", size = 13)
  )


plot1 = ggplot(data=df, aes(x=reorder(Country.Name, Maths.Diff), y=Maths.Diff)) +
  geom_bar(stat = "identity", aes(fill=Maths.Diff)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = mean(df$Maths.Diff), size=1, color="black") +
  labs(x="", y="Maths")+
  scale_fill_gradient(name="% Difference Level", low = "red", high = "green")+
  theme(legend.position = "none")
plot2 = ggplot(data=df, aes(x=reorder(Country.Name, Reading.Diff), y=Reading.Diff)) +
  geom_bar(stat = "identity", aes(fill=Reading.Diff)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = mean(df$Reading.Diff), size=1, color="black") +
  labs(x="", y="Reading")+
  scale_fill_gradient(name="% Difference Level", low = "red", high = "green") +
  theme(legend.position = "none")
plot3 = ggplot(data=df, aes(x=reorder(Country.Name, Science.Diff), y=Science.Diff)) +
  geom_bar(stat = "identity", aes(fill=Science.Diff)) +
  coord_flip() +
  theme_light() +
  geom_hline(yintercept = mean(df$Science.Diff), size=1, color="black") +
  labs(x="", y="Science")+
  scale_fill_gradient(name="% Difference", low = "red", high = "green") +
  theme(legend.position = "none")


grid.arrange(plot1, plot2,plot3, nrow = 1,
             top = 'Are Males better than Females?',
             bottom = '% Difference from Females'
)

# Los graficos los muestro en una misma fila (nrow =1)


