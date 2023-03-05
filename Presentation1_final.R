rm(list=ls())
library(corrplot)
library(cluster)
library(psych)
#### understanding and cleaning our data ####
setwd("D:/Programming/Regression Analysis/Datasets")
df <- read.csv("spotify.csv")
tmp = head(df)
ncol(df)
nrow(df)
names(df)
sapply(df, class)
classes = unique(sapply(df, class))
summary(df)

genre_table <- table(df$playlist_genre)
genre_table
subgenre_table <- table(df$playlist_subgenre)
subgenre_table

length(unique(df$playlist_name))
length(unique(df$track_artist))
# collecting numeric columns from our data
spotify_num = df[sapply(df, is.numeric)]
spotify_num
spotify_num <- spotify_num[-c(1,6)]
spotify_num
#calculating dispersion matrix
disp = cov(spotify_num)
disp
dcorr = cor(spotify_num)
dcorr
#generating a heat map to visualize correlation between columns 
corrplot(dcorr, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))

#scaling our data for pca 
spotify_scale <- sapply(spotify_num, scale)

#### FA and PCA ####
#scree plot analysis for fa and pca
fa.parallel(spotify_scale, fa="both", n.iter=100,
            show.legend=TRUE, main="Scree plot with parallel analysis")

fa <- fa(spotify_scale, nfactors=5, rotate="none", fm="pa")
fa
rfa <- fa(spotify_scale, nfactors=5, rotate="varimax", fm="pa")
rfa
pc <- principal(spotify_scale, nfactors=7,rotate="none")
pc
rpc <- principal(spotify_scale, nfactors=7,rotate="varimax")
rpc
summary(pc)
pcs <- prcomp(spotify_scale)
pcs
summary(pcs)

#### CLUSTERING ####
#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 50
data <- spotify_scale
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


kmeans.re <- kmeans(spotify_scale, centers = 449, nstart = 50)
kmeans.re
head(kmeans.re)
summary(kmeans.re)

#table(df$playlist_genre[which(kmeans.re$cluster == 1)])


