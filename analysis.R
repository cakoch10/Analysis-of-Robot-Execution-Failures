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


###NOW WE WANT TO LOOK AT obstruction

#Create data frame
obstructionDFx <- data.frame(matrix(ncol=1,nrow = 15))
obstructionDFy <- data.frame(matrix(ncol = 1, nrow = 15))
obstructionDFz <- data.frame(matrix(ncol = 1, nrow = 15))
obstructionDFtx <- data.frame(matrix(ncol = 1, nrow = 15))
obstructionDFty <- data.frame(matrix(ncol = 1, nrow = 15))
obstructionDFtz <- data.frame(matrix(ncol = 1, nrow = 15))


#Add data to data frames
for(i in 1:1408){
  a = i+1
  e = i + 15
  s = toString(a)
  if (df1$V1[i] == 'obstruction') {
    # want something that looks like df1$V2[i+1:i+15]
    obstructionDFx[,s] <- df1$V2[a:e]
    obstructionDFy[,s] <- df1$V3[a:e]
    obstructionDFz[,s] <- df1$V4[a:e]
    obstructionDFtx[,s] <- df1$V5[a:e]
    obstructionDFty[,s] <- df1$V6[a:e]
    obstructionDFtz[,s] <- df1$V7[a:e]
  }
}


#Remove null columns
obstructionDFx <- obstructionDFx[,colSums(is.na(obstructionDFx))<nrow(obstructionDFx)]
obstructionDFy <- obstructionDFy[,colSums(is.na(obstructionDFy))<nrow(obstructionDFy)]
obstructionDFz <- obstructionDFz[,colSums(is.na(obstructionDFz))<nrow(obstructionDFz)]
obstructionDFtx <- obstructionDFtx[,colSums(is.na(obstructionDFtx))<nrow(obstructionDFtx)]
obstructionDFty <- obstructionDFty[,colSums(is.na(obstructionDFty))<nrow(obstructionDFty)]
obstructionDFtz <- obstructionDFtz[,colSums(is.na(obstructionDFtz))<nrow(obstructionDFtz)]

#Average columns
obstructionDFx$mean <- rowMeans(obstructionDFx, na.rm=TRUE)
obstructionDFy$mean <- rowMeans(obstructionDFy, na.rm=TRUE)
obstructionDFz$mean <- rowMeans(obstructionDFz, na.rm=TRUE)
obstructionDFtx$mean <- rowMeans(obstructionDFtx, na.rm=TRUE)
obstructionDFty$mean <- rowMeans(obstructionDFty, na.rm=TRUE)
obstructionDFtz$mean <- rowMeans(obstructionDFtz, na.rm=TRUE)

x <- c(seq(from=1,to=15,by=1))

df <- data.frame(x,obstructionDFx$mean,obstructionDFy$mean,obstructionDFz$mean,obstructionDFtx$mean,obstructionDFty$mean,obstructionDFtz$mean)

# Visualize
ggplot(df, aes(df$x, y=value)) + labs(x="Time after obstruction", y="Mean") + ggtitle("Comparing Different Failures for obstructions")+ geom_point(aes(y=obstructionDFx$mean, col="X force"), size=2) + geom_point(aes(y=obstructionDFy$mean,col="Y Force"), size=2) + geom_point(aes(y=obstructionDFz$mean, col="Z force"), size=2) + geom_point(aes(y=obstructionDFtx$mean,col="X Torque"), size=2) + geom_point(aes(y=obstructionDFty$mean,col="Y Torque"), size=2) + geom_point(aes(y=obstructionDFtz$mean,col="Z Torque"), size=2) + geom_line(aes(x = df$x, y = df$obstructionDFx.mean), data=df, size=0) + geom_line(aes(x = df$x, y = df$obstructionDFy.mean), data=df, size=0) + geom_line(aes(x = df$x, y = df$obstructionDFz.mean), data=df, size=0) + geom_line(aes(x = df$x, y = df$obstructionDFtx.mean), data=df, size=0) + geom_line(aes(x = df$x, y = df$obstructionDFty.mean), data = df, size = 0) + geom_line(aes(x = df$x, y = df$obstructionDFtz.mean),  data = df, size = 0)

###NOW WE WANT TO LOOK AT fr_collision

#Create data frame
fr_collisionDFx <- data.frame(matrix(ncol=1,nrow = 15))
fr_collisionDFy <- data.frame(matrix(ncol = 1, nrow = 15))
fr_collisionDFz <- data.frame(matrix(ncol = 1, nrow = 15))
fr_collisionDFtx <- data.frame(matrix(ncol = 1, nrow = 15))
fr_collisionDFty <- data.frame(matrix(ncol = 1, nrow = 15))
fr_collisionDFtz <- data.frame(matrix(ncol = 1, nrow = 15))


#Add data to data frames
for(i in 1:1408){
  a = i+1
  e = i + 15
  s = toString(a)
  if (df1$V1[i] == 'fr_collision') {
    # want something that looks like df1$V2[i+1:i+15]
    fr_collisionDFx[,s] <- df1$V2[a:e]
    fr_collisionDFy[,s] <- df1$V3[a:e]
    fr_collisionDFz[,s] <- df1$V4[a:e]
    fr_collisionDFtx[,s] <- df1$V5[a:e]
    fr_collisionDFty[,s] <- df1$V6[a:e]
    fr_collisionDFtz[,s] <- df1$V7[a:e]
  }
}


#Remove null columns
fr_collisionDFx <- fr_collisionDFx[,colSums(is.na(fr_collisionDFx))<nrow(fr_collisionDFx)]
fr_collisionDFy <- fr_collisionDFy[,colSums(is.na(fr_collisionDFy))<nrow(fr_collisionDFy)]
fr_collisionDFz <- fr_collisionDFz[,colSums(is.na(fr_collisionDFz))<nrow(fr_collisionDFz)]
fr_collisionDFtx <- fr_collisionDFtx[,colSums(is.na(fr_collisionDFtx))<nrow(fr_collisionDFtx)]
fr_collisionDFty <- fr_collisionDFty[,colSums(is.na(fr_collisionDFty))<nrow(fr_collisionDFty)]
fr_collisionDFtz <- fr_collisionDFtz[,colSums(is.na(fr_collisionDFtz))<nrow(fr_collisionDFtz)]

#Average columns
fr_collisionDFx$mean <- rowMeans(fr_collisionDFx, na.rm=TRUE)
fr_collisionDFy$mean <- rowMeans(fr_collisionDFy, na.rm=TRUE)
fr_collisionDFz$mean <- rowMeans(fr_collisionDFz, na.rm=TRUE)
fr_collisionDFtx$mean <- rowMeans(fr_collisionDFtx, na.rm=TRUE)
fr_collisionDFty$mean <- rowMeans(fr_collisionDFty, na.rm=TRUE)
fr_collisionDFtz$mean <- rowMeans(fr_collisionDFtz, na.rm=TRUE)

x <- c(seq(from=1,to=15,by=1))

df <- data.frame(x,fr_collisionDFx$mean,fr_collisionDFy$mean,fr_collisionDFz$mean,fr_collisionDFtx$mean,fr_collisionDFty$mean,fr_collisionDFtz$mean)

# Visualize
ggplot(df, aes(df$x, y=value)) + labs(x="Time after fr_collision", y="Mean") + ggtitle("Comparing Different Failures for fr_collisions")+ geom_point(aes(y=fr_collisionDFx$mean, col="X force"), size=2) + geom_point(aes(y=fr_collisionDFy$mean,col="Y Force"), size=2) + geom_point(aes(y=fr_collisionDFz$mean, col="Z force"), size=2) + geom_point(aes(y=fr_collisionDFtx$mean,col="X Torque"), size=2) + geom_point(aes(y=fr_collisionDFty$mean,col="Y Torque"), size=2) + geom_point(aes(y=fr_collisionDFtz$mean,col="Z Torque"), size=2) + geom_line(aes(x = df$x, y = df$fr_collisionDFx.mean), data=df, size=0) + geom_line(aes(x = df$x, y = df$fr_collisionDFy.mean), data=df, size=0) + geom_line(aes(x = df$x, y = df$fr_collisionDFz.mean), data=df, size=0) + geom_line(aes(x = df$x, y = df$fr_collisionDFtx.mean), data=df, size=0) + geom_line(aes(x = df$x, y = df$fr_collisionDFty.mean), data = df, size = 0) + geom_line(aes(x = df$x, y = df$fr_collisionDFtz.mean),  data = df, size = 0)






##----------------------------------------------------------
##now we analyze the lp2 data


###READ NORMAL READINGS FROM D1

#Create data frame
normalDFx <- data.frame(matrix(ncol=1,nrow = 15))
normalDFy <- data.frame(matrix(ncol = 1, nrow = 15))
normalDFz <- data.frame(matrix(ncol = 1, nrow = 15))
normalDFtx <- data.frame(matrix(ncol = 1, nrow = 15))
normalDFty <- data.frame(matrix(ncol = 1, nrow = 15))
normalDFtz <- data.frame(matrix(ncol = 1, nrow = 15))

#Add data to data frames
for(i in 1:752){
  a = i+1
  e = i + 15
  s = toString(a)
  if (df2$V1[i] == 'normal') {
    # want something that looks like df1$V2[i+1:i+15]
    normalDFx[,s] <- df2$V2[a:e]
    normalDFy[,s] <- df2$V3[a:e]
    normalDFz[,s] <- df2$V4[a:e]
    normalDFtx[,s] <- df2$V5[a:e]
    normalDFty[,s] <- df2$V6[a:e]
    normalDFtz[,s] <- df2$V7[a:e]
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


###NOW WE WANT TO LOOK AT BACK_COL

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
  if (df2$V1[i] == 'back_col') {
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


