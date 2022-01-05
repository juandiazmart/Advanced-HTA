library(meta)


df <- read.csv("R/meta.csv")

metabin <- metabin(event.e = obi,n.e=n.obi,event.c = ritu,
                   n.c=n.ritu,studlab = ID,data=df,sm="RR")

summary(metabin)

forest(metabin)

?forest
