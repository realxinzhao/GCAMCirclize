
library(dplyr)
library(circlize)

# FAOSTAT access date 2/2/2022  
# unit: tonne
read.csv("data/FAOSTAT_TM_Wheat_2020.csv") -> Wheat_TM_2020 


Wheat_TM_2020 %>% 
  mutate(value = value / 1000000) %>%  #Mt
  filter(value >= 0.5) %>% 
  mutate(area = replace(area, area == "United States of America", "USA")) %>% 
  mutate(area = replace(area, area == "United Arab Emirates", "UAE")) %>% 
  mutate(area = replace(area, area == "Democratic Republic of the Congo", "DR Congo")) %>% 
  mutate(area = gsub("Federation| of Great Britain and Northern Ireland| Province of| \\(Bolivarian Republic of\\)|United Republic of | \\(Islamic Republic of\\)", "", area)) %>% 
  mutate(source = replace(source, source == "United States of America", "USA")) %>% 
  mutate(source = replace(source, source == "United Arab Emirates", "UAE")) %>% 
  mutate(source = replace(source, source == "Democratic Republic of the Congo", "DR Congo")) %>% 
  mutate(source = gsub("Federation| of Great Britain and Northern Ireland| Province of| \\(Bolivarian Republic of\\)|United Republic of | \\(Islamic Republic of\\)", "", source)) %>% 
  select(REG_ex = source, REG_im = area, flow = value) ->
dat_circular
  





#save figure to default wd
png(file = "Wheat_trade_2020.png", width = 7000, height = 7000, res = 600)

#base plot
chordDiagram(as.data.frame(dat_circular), 
             transparency = 0.5, 
             directional = 1,
             direction.type = c("diffHeight", "arrows"), 
             diffHeight = -uh(2, "mm")
             ,link.arr.type = "big.arrow"
             ,annotationTrack = c("grid")
             ,preAllocateTracks = list(list(track.height = c(0.3))
                                       ,list(track.height = c(0.035))
             ))

#title(main = "test figure")

circos.track(track.index = 3, panel.fun = function(x, y) {
  circos.axis(h = 1, labels.cex = 0.8)   
}, bg.border = NA)

circos.track(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
  #make text label vertical when space is too small; cex to adjust font size
  
  if(abs(xplot[2] - xplot[1]) < 20 | abs(xplot[2] - xplot[1]) > 340) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5), col = "black", 
                cex = 1)
  } else {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "inside", 
                niceFacing = TRUE, adj = c(0.5, 0), col= "black", 
                cex = 1)
  }  }, bg.border = NA)

dev.off() #dump

