library(EBImage)
library(rgl)

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
  bw = imageData(flip(img))
  bw = (bw[,,1]+bw[,,2]+bw[,,3])/3
  image(bw, col=heat.colors(10))
}
plotPicDiff = function(img1, img2){
  bw1 = imageData(flip(img1))
  bw1 = (bw1[,,1]+bw1[,,2]+bw1[,,3])/3
  
  bw2 = imageData(flip(img2))
  bw2 = (bw2[,,1]+bw2[,,2]+bw2[,,3])/3
  
  image(bw1-bw2, col=heat.colors(10))
  
  browser()
  plot3d()
}

plotPic(img[[1]]$img)
plotPic(img[[2]]$img)
plotPicDiff(img[[1]]$img, img[[2]]$img)
plotPicDiff(img[[2]]$img, img[[3]]$img)
plotPicDiff(img[[3]]$img, img[[4]]$img)

(lapply(img, function(x)return(x$sensor)))
