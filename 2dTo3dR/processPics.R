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
  bw = imageData(flip((img))
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

findMatches(img1, img2, gps = 30, df=60){
  #gps = grid filter size (the size of the grid to find points on)
  #df = difference Filter so you don't have to compare each grid to all grids.. .just ones close by
  
  bw1 = imageData(flip(resize(img1,w=dim(img1)[1]/4)))
  bw1 = (bw1[,,1]+bw1[,,2]+bw1[,,3])/3
  
  bw2 = imageData(flip(resize(img2, w=dim(img2)[1]/4)))
  bw2 = (bw2[,,1]+bw2[,,2]+bw2[,,3])/3
  
  #divide up the first image into points with features
  feats1 = array(0, c((nrow(bw1)-gps)*(ncol(bw1)-gps), gps^2+2))
  feats2 = feats1
  count=1
  for(i in (1):(nrow(bw1)-gps)){
    for(j in (1):(ncol(bw1)-gps)){
      feats1[count,] = c(i,j, as.numeric(bw1[i:(i+gps-1), j:(j+gps-1)]))  
      feats2[count,] = c(i,j, as.numeric(bw2[i:(i+gps-1), j:(j+gps-1)]))
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
    
      featCor[i,indFilt] = rowMeans((matrix(feats1[i,3:ncol(feats1)], nrow=length(indFilt), ncol=ncol(feats1)-2, byrow=T)
                                    -feats2[indFilt,3:ncol(feats1)])^2 )
    
  } 
    
  
}

plotPic(img[[1]]$img)
plotPic(img[[2]]$img)
plotPicDiff(img[[1]]$img, img[[2]]$img)
plotPicDiff(img[[2]]$img, img[[3]]$img)
plotPicDiff(img[[3]]$img, img[[4]]$img)

(lapply(img, function(x)return(x$sensor)))
