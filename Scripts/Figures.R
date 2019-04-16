############################################
## tables and figures? #####################
###########################################

# SCRIPT TO MAKE FIGURES FOR POSTERS/PAPER?

library(expss)
library(tables)
library(grDevices) # add nice colors
require(graphics)
library(ggthemes)
library(ggmap)
library(gridExtra)
library(ape)

# phylogeny
ColsBasic <- c("#D55E00","#56B4E9")
Phylo <- read.tree(file = "Analysis_Scripts/Chapter3/Data/Pruned/BBPruned.tre")
Phylo$tip.label <- str_replace_all(Phylo$tip.label, "_", " ")
Cols <- as.character(Class1$Strict[match(Phylo$tip.label, Class1$Species)])
Cols[which(Cols == "A")] <- ColsBasic[2]
Cols[which(Cols != ColsBasic[2])] <- ColsBasic[1]
pdf("PhyloColored.pdf", width = 3, height = 21)
par(mfrow = c(1,1), mar = c(0,1,1,0))
plot.phylo(Phylo, tip.color = Cols, cex = 0.3)
dev.off()

# NOTE I DIDNT GET THESE TO WORK, I JUST MADE A PDF FROM PPT FROM MAPS I MADE

# load the maxent predictions
ArbC5 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/Final_mapAC5.grd")
TerrC5 <- raster("./Analysis_Scripts/Chapter3/ENM/Prediction/Final_mapTC5.grd")

# load the polygons
ArbPoly <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Polygons/ArbPolyAll/chull.shp")
TerrPoly <- rgdal::readOGR("./Analysis_Scripts/Chapter3/Polygons/TerrPolyAll/chull.shp")

# set margins
par(mfrow = c(1,1), mar = c(1,1,1,1))
plot(ArbC5,col=jet.colors)

#
show_col(colorblind_pal()(8))
jet.colors <-
  colorRampPalette(c("white","#D55E00","#56B4E9","#56B4E9"))( 80 )
#
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-150,0), ylim=c(10,20), axes=F, col="light gray") 

#
pdf("Sample4panelplot.pdf", width = 7, height = 5)
par(mar = c(1,.001,.05,.01), mfrow=c(3,3))
plot.new()
plot.new()
text(x=.5,y=.15,paste("Arboreal\n", 
                     "Niche Model"), cex = 1.6, col = "black")
plot.new()
text(x=.5,y=.15,paste("Terrestrial\n", 
                     "Niche Model"), cex = 1.6, col = "black")
plot.new()
text(x=.75,y=.5,paste("Arboreal\n", 
                     "Distribution"), cex = 1.6, col = "black",srt=90)

par(mfrow = c(1,1), mar = c(1,1,1,1))
plot(ArbC5, col=jet.colors, xaxt = "n", yaxt = "n", xlim = c(-150,-10), 
     ylim = c(-20,55), legend=F)
plot(ArbPoly, add = T,lwd=2)
#text("59.3% Overlap", x=-110, y=0,cex = 1)

par(mfrow = c(1,1), mar = c(1,1,1,1))
plot(TerrC5, col=jet.colors, xaxt = "n", yaxt = "n",xlim = c(-150,-10), 
             ylim = c(-20,55),legend=F)
plot(ArbPoly, add = T,lwd=2)

text("4.2% Overlap", x=-110, y=0,cex = 1)
plot.new()
text(x=.85,y=.5,paste("Terrestrial\n", 
                     "Distribution"), cex = 1.6, col = "black",srt=90)

par(mfrow = c(1,1), mar = c(1,1,1,1))
plot(ArbC5, col=jet.colors, xaxt = "n", yaxt = "n", xlim = c(-150,-10), ylim = c(-20,55),legend=F)
plot(TerrPoly, add = T,lwd=2)

text("35.0% Overlap", x=-110, y=0,cex = 1)

par(mfrow = c(1,1), mar = c(1,1,1,1))
plot(TerrC5, col=jet.colors, xaxt = "n", yaxt = "n", xlim = c(-150,-10), ylim = c(-20,55),legend=F)
plot(TerrPoly, add = T,lwd=2)
text("84.4% Overlap", x=-110, y=0,cex = 1)
dev.off()


# USING LAYOUT

pdf("l.pdf", width=7, height=5)
mat <- matrix(c(0,1,2,3,4,5,6,7,8), 3,byrow=T)
layout(mat = mat,c(1,5,5),c(2,5,5))
#layout(mat = mat, c(2,5,5), c(2,5,5))
layout.show(n=8)

par(mar=c(1, 1, 1, 1))
plot.new()
text(x=.5,y=.5,paste("Arboreal\n", 
                     "Niche Model"), cex = 1.5, col = "black")
par(mar=c(1, 1, 1, 1))
plot.new()
text(x=.5,y=.5,paste("Terrestrial\n", 
                      "Niche Model"), cex = 1.5, col = "black")
par(mar=c(1, 1, 1, 1))
plot.new()
text(x=.5,y=.5,paste("Arboreal\n", 
                     "Distribution"), cex = 1.5, col = "black",srt=90)

par(mar=c(2, .1, 2, .1))
plot(ArbC5, col=jet.colors, xaxt = "n", yaxt = "n", xlim = c(-150,-10), 
     ylim = c(-20,55), legend=F, add = F, frame.plot = F)
layout(mat = mat,c(1,5,5),c(2,5,5))
plot(ArbPoly, add = T)
text("59.3% Overlap", x=-110, y=0,cex = .5)
par(mar=c(2, .1, 2, .1))
plot(TerrC5, col=jet.colors, xaxt = "n", yaxt = "n",xlim = c(-150,-10), 
     ylim = c(-20,55),legend=F)
plot(ArbPoly, add = T)
text("4.2% Overlap", x=-110, y=0,cex =.5)
par(mar=c(1, 1, 1, 1))
plot.new()
text(x=.5,y=.5,paste("Terrestrial\n", 
                      "Distribution"), cex = 1.5, col = "black",srt=90)
par(mar=c(2, .1, 2, .1))
plot(ArbC5, col=jet.colors, xaxt = "n", yaxt = "n", xlim = c(-150,-10), ylim = c(-20,55),legend=F)
plot(TerrPoly, add = T)
text("35.0% Overlap", x=-110, y=0,cex = .5)
par(mar=c(2, .1, 2, .1))
plot(TerrC5, col=jet.colors, xaxt = "n", yaxt = "n", xlim = c(-150,-10), ylim = c(-20,55),legend=F)
plot(TerrPoly, add = T)
text("84.4% Overlap", x=-110, y=0,cex = .5)
dev.off()





