library(tidyverse)
library(ggplot2)

df.raw <- read.csv(file ='Pisa scores 2013 - 2015 Data.csv', fileEncoding="UTF-8-BOM", na.strings = '..')
str(df.raw)

df <- df.raw[1:1161, c(1, 4, 7)] #select relevant rows and cols
  %>%  spread(key=Series.Code, value=X2015..YR2015.) 
  %>%  rename(Maths = LO.PISA.MAT,                        
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


