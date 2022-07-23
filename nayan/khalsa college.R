library(tidyverse)
khalsa_college=
  read_csv("C:\\Users\\student\\Desktop\\college.csv")

khalsa_college %>%
  view()

khalsa_college = khalsa_college %>%
  mutate(region=as.factor(region))
khalsa_college

ggplot()
ggplot(data = khalsa_college) + 
  geom_line(mapping = aes(x=tuition,y=sat_avg,color = region))

ggplot()
ggplot(data = khalsa_college) + 
  geom_line(mapping = aes(x=tuition,y=sat_avg,color = control))

ggplot()
ggplot(data = khalsa_college) + 
  geom_point(mapping = aes(x=tuition,y=sat_avg,color = gender))


