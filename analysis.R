df <- readMat("Test_2_run_2.mat")

df2 <- readMat("Test_1_run_2.mat")

df3 <- readMat("Test_19_run_2.mat")

setwd("C:/Users/Dell/Desktop/BigRed")

df1<-read.delim("lp1.data",dec="\t",header = F)
df2<-read.delim("lp2.data",dec="\t",header = F)
df3<-read.delim("lp3.data",dec="\t",header = F)
df4<-read.delim("lp4.data",dec="\t",header = F)
df5<-read.delim("lp5.data",dec="\t",header = F)


# THIS IS HOW YOU ELIMINATE NULL COLS: normalDFy <- normalDFy[,colSums(is.na(normalDFy))<nrow(normalDFy)]

i=10
normalDFy <- as.data.frame(df1$V2[2:16])
normalDFy$i <- df1$V2[18:32]

normalDFx <- data.frame()
normalDFy <- data.frame()
normalDFz <- data.frame(ncols=1,nrow=15)
normalDFtx <- data.frame()
normalDFty <- data.frame()
normalDFtz <- data.frame()

dim(normalDFy$X1)
dimnames(normalDFy$X1)

head(normalDF)

nodata <- as.data.frame(setNames(normal))


head(normalDFy)

as.data.frame(df1$V2[2:16])

###READ NORMAL READINGS FROM D1

#Create data frame
normalDFx <- data.frame(matrix(ncol=1,nrow = 15))
normalDFy <- data.frame(matrix(ncol = 1, nrow = 15))
normalDFz <- data.frame(matrix(ncol = 1, nrow = 15))
normalDFtx <- data.frame(matrix(ncol = 1, nrow = 15))
normalDFty <- data.frame(matrix(ncol = 1, nrow = 15))
normalDFtz <- data.frame(matrix(ncol = 1, nrow = 15))

#Add data to data frames
for(i in 1:1408){
  a = i+1
  e = i + 15
  s = toString(a)
  if (df1$V1[i] == 'normal') {
    # want something that looks like df1$V2[i+1:i+15]
    normalDFx[,s] <- df1$V2[a:e]
    normalDFy[,s] <- df1$V3[a:e]
    normalDFz[,s] <- df1$V4[a:e]
    normalDFtx[,s] <- df1$V5[a:e]
    normalDFty[,s] <- df1$V6[a:e]
    normalDFtz[,s] <- df1$V7[a:e]
  }
}

#Remove null columns
normalDFx <- normalDFx[,colSums(is.na(normalDFx))<nrow(normalDFx)]
normalDFy <- normalDFy[,colSums(is.na(normalDFy))<nrow(normalDFy)]
normalDFz <- normalDFz[,colSums(is.na(normalDFz))<nrow(normalDFz)]
normalDFtx <- normalDFtx[,colSums(is.na(normalDFtx))<nrow(normalDFtx)]
normalDFty <- normalDFty[,colSums(is.na(normalDFty))<nrow(normalDFty)]
normalDFtz <- normalDFtz[,colSums(is.na(normalDFtz))<nrow(normalDFtz)]


#Average columns
normalDFx$mean <- rowMeans(normalDFx, na.rm=TRUE)
normalDFy$mean <- rowMeans(normalDFy, na.rm=TRUE)
normalDFz$mean <- rowMeans(normalDFz, na.rm=TRUE)
normalDFtx$mean <- rowMeans(normalDFtx, na.rm=TRUE)
normalDFty$mean <- rowMeans(normalDFty, na.rm=TRUE)
normalDFtz$mean <- rowMeans(normalDFtz, na.rm=TRUE)

x <- c(seq(from=1,to=15,by=1))

df <- data.frame(x,normalDFx$mean,normalDFy$mean,normalDFz$mean,normalDFtx$mean,normalDFty$mean,normalDFtz$mean)

ggplot(df, aes(df$x, y=value)) + labs(x="Time after collision", y="Mean") + ggtitle("Comparing Different Failures")+ geom_point(aes(y=normalDFx$mean, col="X force"), size=2) + geom_point(aes(y=normalDFy$mean,col="Y Force"), size=2) + geom_point(aes(y=normalDFz$mean, col="Z force"), size=2) + geom_point(aes(y=normalDFtx$mean,col="X Torque"), size=2) + geom_point(aes(y=normalDFty$mean,col="Y Torque"), size=2) + geom_point(aes(y=normalDFtz$mean,col="Z Torque"), size=2) + geom_line(aes(x = df$x, y = df$normalDFx.mean), data=df, size=0) + geom_line(aes(x = df$x, y = df$normalDFy.mean), data=df, size=0) + geom_line(aes(x = df$x, y = df$normalDFz.mean), data=df, size=0) + geom_line(aes(x = df$x, y = df$normalDFtx.mean), data=df, size=0) + geom_line(aes(x = df$x, y = df$normalDFty.mean), data = df, size = 0) + geom_line(aes(x = df$x, y = df$normalDFtz.mean),  data = df, size = 0)

###NOW WE WANT TO LOOK AT COLLISION

#Create data frame
collisionDFx <- data.frame(matrix(ncol=1,nrow = 15))
collisionDFy <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFz <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFtx <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFty <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFtz <- data.frame(matrix(ncol = 1, nrow = 15))


#Add data to data frames
for(i in 1:1408){
  a = i+1
  e = i + 15
  s = toString(a)
  if (df1$V1[i] == 'collision') {
    # want something that looks like df1$V2[i+1:i+15]
    collisionDFx[,s] <- df1$V2[a:e]
    collisionDFy[,s] <- df1$V3[a:e]
    collisionDFz[,s] <- df1$V4[a:e]
    collisionDFtx[,s] <- df1$V5[a:e]
    collisionDFty[,s] <- df1$V6[a:e]
    collisionDFtz[,s] <- df1$V7[a:e]
  }
}


#Remove null columns
collisionDFx <- collisionDFx[,colSums(is.na(collisionDFx))<nrow(collisionDFx)]
collisionDFy <- collisionDFy[,colSums(is.na(collisionDFy))<nrow(collisionDFy)]
collisionDFz <- collisionDFz[,colSums(is.na(collisionDFz))<nrow(collisionDFz)]
collisionDFtx <- collisionDFtx[,colSums(is.na(collisionDFtx))<nrow(collisionDFtx)]
collisionDFty <- collisionDFty[,colSums(is.na(collisionDFty))<nrow(collisionDFty)]
collisionDFtz <- collisionDFtz[,colSums(is.na(collisionDFtz))<nrow(collisionDFtz)]

#Average columns
collisionDFx$mean <- rowMeans(collisionDFx, na.rm=TRUE)
collisionDFy$mean <- rowMeans(collisionDFy, na.rm=TRUE)
collisionDFz$mean <- rowMeans(collisionDFz, na.rm=TRUE)
collisionDFtx$mean <- rowMeans(collisionDFtx, na.rm=TRUE)
collisionDFty$mean <- rowMeans(collisionDFty, na.rm=TRUE)
collisionDFtz$mean <- rowMeans(collisionDFtz, na.rm=TRUE)

x <- c(seq(from=1,to=15,by=1))

df <- data.frame(x,collisionDFx$mean,collisionDFy$mean,collisionDFz$mean,collisionDFtx$mean,collisionDFty$mean,collisionDFtz$mean)

# Visualize
ggplot(df, aes(df$x, y=value)) + labs(x="Time after collision", y="Mean") + ggtitle("Comparing Different Failures for Collisions")+ geom_point(aes(y=collisionDFx$mean, col="X force"), size=2) + geom_point(aes(y=collisionDFy$mean,col="Y Force"), size=2) + geom_point(aes(y=collisionDFz$mean, col="Z force"), size=2) + geom_point(aes(y=collisionDFtx$mean,col="X Torque"), size=2) + geom_point(aes(y=collisionDFty$mean,col="Y Torque"), size=2) + geom_point(aes(y=collisionDFtz$mean,col="Z Torque"), size=2) + geom_line(aes(x = df$x, y = df$collisionDFx.mean), data=df, size=0) + geom_line(aes(x = df$x, y = df$collisionDFy.mean), data=df, size=0) + geom_line(aes(x = df$x, y = df$collisionDFz.mean), data=df, size=0) + geom_line(aes(x = df$x, y = df$collisionDFtx.mean), data=df, size=0) + geom_line(aes(x = df$x, y = df$collisionDFty.mean), data = df, size = 0) + geom_line(aes(x = df$x, y = df$collisionDFtz.mean),  data = df, size = 0)






