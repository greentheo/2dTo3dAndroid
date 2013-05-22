library(EBImage)
library(rgl)
library(Matrix)
picSet = 'firehydrant'

files = list.files(path=paste('picsets/', picSet, sep='',collapse=''),full.names=T)
img = vector("list", length(files))
for(f in 1:length(files)){
  img[[f]][["img"]] = readImage(files[f])
  
  #correct for the rotation (pitch of the picture)
  sensor=strsplit(gsub('.jpg','',files[f]), '_')[[1]]
  
  img[[f]][["sensor"]] = sensor[c(4,6,8)]
  names(img[[f]][["sensor"]]) = sensor[c(3,5,7)]
  
}
  
#plot pictures and picture diffs
plotPic = function(img){
  bw = imageData(flip((img)))
  bw = (bw[,,1]+bw[,,2]+bw[,,3])/3
  image(bw, col=seq(0,1, length.out=2))
}
plotPicDiff = function(img1, img2){
  bw1 = imageData(flip(img1))
  bw1 = (bw1[,,1]+bw1[,,2]+bw1[,,3])/3
  
  bw2 = imageData(flip(img2))
  bw2 = (bw2[,,1]+bw2[,,2]+bw2[,,3])/3
  
  image(bw1-bw2, col=heat.colors(10))
  
#   browser()
#   surface3d(x=1:nrow(bw1), y=1:ncol(bw1), z=bw1-bw2)
}

findMatches(img1, img2, gps = 30, df=60, skip=5){
  #gps = grid filter size (the size of the grid to find points on)
  #df = difference Filter so you don't have to compare each grid to all grids.. .just ones close by
  
  #threshold for easier analysis (at first)
  bw1 = imageData(flip(resize(img1,w=dim(img1)[1]/4)))
  bw1 = (bw1[,,1]+bw1[,,2]+bw1[,,3])/3
  bw1 = ifelse(bw1>mean(bw1), 0, 1)
  
  bw2 = imageData(flip(resize(img2, w=dim(img2)[1]/4)))
  bw2 = (bw2[,,1]+bw2[,,2]+bw2[,,3])/3
  bw2 = ifelse(bw2>mean(bw2), 0, 1)
  
  #divide up the first image into points with features
  colInd = seq.int((1),(nrow(bw1)-gps+1), by=skip)
  rowInd = seq.int((1),(ncol(bw1)-gps+1), by=skip)
  feats1 = array(0, c(length(colInd)*length(rowInd), gps^2+2))
  feats2 = feats1
  count=1
  for(j in colInd){
    for(i in rowInd){
      feats1[count,] = c(i,j, as.numeric(bw1[j:(j+gps-1),i:(i+gps-1)]))  
      feats2[count,] = c(i,j, as.numeric(bw2[j:(j+gps-1), i:(i+gps-1)]))
      count = count+1
    }
  }
  
  #calculate a similarity score for all feature rows 
  featCor = Matrix(0,nrow=nrow(feats1), ncol=nrow(feats2))
  progessBar = txtProgressBar(min=0, max=nrow(feats1))
  for(i in 1:nrow(feats1)){
    setTxtProgressBar(progessBar, i)
    indFilt = which(feats2[,1]<(feats1[i,1]+df) & 
                      feats2[,1]>(feats1[i,1]-df) &
                      feats2[,2]<(feats1[i,2]+df) &
                      feats2[,2]>(feats1[i,2]-df) )
    
    cors = sapply(indFilt, function(x){
      cor(feats2[x, 3:ncol(feats1)], feats1[i,3:ncol(feats1)])
    })    
    cors[is.na(cors)] = -1
      featCor[i,indFilt] = cors 
#       featCor[i,indFilt] = rowMeans(exp(abs(matrix(feats1[i,3:ncol(feats1)], nrow=length(indFilt), ncol=ncol(feats1)-2, byrow=T)
#                                     -feats2[indFilt,3:ncol(feats1)])) )
  } 
  
  #now take the best match for each point
  #filter out anything less than the .3 correlation and only use those as comparison points
  #sigInd = which(featCor<.3)
  matches = max.col(featCor)
  
  #now we can only allow a unique match... so select best match value
  
  featCor[is.na(featCor)]=-1
  matchInd = which(diag(featCor[,matches])>.3)
  match1Y = feats1[c(1:nrow(feats1))[matchInd], 1]
  match1X = feats1[c(1:nrow(feats1))[matchInd], 2]
  match2Y = feats2[matches[matchInd], 1]
  match2X = feats2[matches[matchInd], 2]
  
  image(1:nrow(bw1), 1:ncol(bw1), bw1,col=gray(seq(0,1, by=.05)))
  Yflip =max(match1Y)-match1Y+1
  points((match1X), Yflip, pch=1:length(matchInd), col="blue")
  rect(xleft=match1X,xright=match1X+gps-1, ytop=Yflip, ybottom=Yflip-gps+1, border="red")
  
  image(1:nrow(bw2), 1:ncol(bw2),bw2, col=gray(seq(0,1, by=.05)))
  Yflip = max(match2Y)-match2Y+1
  points(match2X, Yflip, pch=1:length(matchInd), col="blue")
  rect(xleft=match2X,xright=match2X+gps-1, ytop=Yflip, ybottom=Yflip-gps+1, border="red")
  
  #plot a bunch of examples of matches
 pdf('results/featCompare.pdf')
  for(i in 1:length(matchInd)){
    bwSub1 = bw1[match1X[i]:(match1X[i]+gps-1), match1Y[i]:(match1Y[i]+gps-1)]
    image(1:nrow(bwSub1), 1:ncol(bwSub1), bwSub1,col=gray(seq(0,1, by=.05)), main="img1")
   
    bwSub2 = bw1[match2X[i]:(match2X[i]+gps-1), match2Y[i]:(match2Y[i]+gps-1)]
    image(1:nrow(bwSub2), 1:ncol(bwSub2), bwSub2,col=gray(seq(0,1, by=.05)), main="img2")
    
    image(1:nrow(bwSub2), 1:ncol(bwSub2), bwSub2-bwSub1,col=gray(seq(0,1, by=.05)), main="subtract")
    
  }
  dev.off()
}

plotPic(img[[1]]$img)
plotPic(img[[2]]$img)
plotPicDiff(img[[1]]$img, img[[2]]$img)
plotPicDiff(img[[2]]$img, img[[3]]$img)
plotPicDiff(img[[3]]$img, img[[4]]$img)

(lapply(img, function(x)return(x$sensor)))
