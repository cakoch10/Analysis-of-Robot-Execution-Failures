df2<-read.delim("lp3.data",dec="\t",header = F)

###NOW WE WANT TO LOOK AT normal

#Create data frame
collisionDFx <- data.frame(matrix(ncol=1,nrow = 15))
collisionDFy <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFz <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFtx <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFty <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFtz <- data.frame(matrix(ncol = 1, nrow = 15))


#Add data to data frames
for(i in 1:752){
  a = i+1
  e = i + 15
  s = toString(a)
  if (df2$V1[i] == 'ok') {
    # want something that looks like df1$V2[i+1:i+15]
    collisionDFx[,s] <- df2$V2[a:e]
    collisionDFy[,s] <- df2$V3[a:e]
    collisionDFz[,s] <- df2$V4[a:e]
    collisionDFtx[,s] <- df2$V5[a:e]
    collisionDFty[,s] <- df2$V6[a:e]
    collisionDFtz[,s] <- df2$V7[a:e]
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

###NOW WE WANT TO LOOK AT MOVED

#Create data frame
collisionDFx <- data.frame(matrix(ncol=1,nrow = 15))
collisionDFy <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFz <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFtx <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFty <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFtz <- data.frame(matrix(ncol = 1, nrow = 15))


#Add data to data frames
for(i in 1:752){
  a = i+1
  e = i + 15
  s = toString(a)
  if (df2$V1[i] == 'moved') {
    # want something that looks like df1$V2[i+1:i+15]
    collisionDFx[,s] <- df2$V2[a:e]
    collisionDFy[,s] <- df2$V3[a:e]
    collisionDFz[,s] <- df2$V4[a:e]
    collisionDFtx[,s] <- df2$V5[a:e]
    collisionDFty[,s] <- df2$V6[a:e]
    collisionDFtz[,s] <- df2$V7[a:e]
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

###NOW WE WANT TO LOOK AT SLIGHTLY_NORMAL

#Create data frame
collisionDFx <- data.frame(matrix(ncol=1,nrow = 15))
collisionDFy <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFz <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFtx <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFty <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFtz <- data.frame(matrix(ncol = 1, nrow = 15))


#Add data to data frames
for(i in 1:752){
  a = i+1
  e = i + 15
  s = toString(a)
  if (df2$V1[i] == 'slightly_moved') {
    # want something that looks like df1$V2[i+1:i+15]
    collisionDFx[,s] <- df2$V2[a:e]
    collisionDFy[,s] <- df2$V3[a:e]
    collisionDFz[,s] <- df2$V4[a:e]
    collisionDFtx[,s] <- df2$V5[a:e]
    collisionDFty[,s] <- df2$V6[a:e]
    collisionDFtz[,s] <- df2$V7[a:e]
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


###NOW WE WANT TO LOOK AT LOST

#Create data frame
collisionDFx <- data.frame(matrix(ncol=1,nrow = 15))
collisionDFy <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFz <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFtx <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFty <- data.frame(matrix(ncol = 1, nrow = 15))
collisionDFtz <- data.frame(matrix(ncol = 1, nrow = 15))


#Add data to data frames
for(i in 1:752){
  a = i+1
  e = i + 15
  s = toString(a)
  if (df2$V1[i] == 'lost') {
    # want something that looks like df1$V2[i+1:i+15]
    collisionDFx[,s] <- df2$V2[a:e]
    collisionDFy[,s] <- df2$V3[a:e]
    collisionDFz[,s] <- df2$V4[a:e]
    collisionDFtx[,s] <- df2$V5[a:e]
    collisionDFty[,s] <- df2$V6[a:e]
    collisionDFtz[,s] <- df2$V7[a:e]
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




