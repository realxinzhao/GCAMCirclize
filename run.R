rm(list=ls())
require(tidyr)
library(dplyr)
library(stringr)
library(circlize)
library(RColorBrewer)
`%notin%` = Negate(`%in%`) #create not in

#Read and process queried data
#region mapping
Regmap <- read.csv("./data/Regmapping.csv", header=TRUE, sep=",",comment.char = "#")

Rawdatapath= "./Queriedcsv/"

quary = "Agprod"; file = paste0(Rawdatapath, quary, ".csv")
assign(quary,  tidyr::gather(read.csv(textConnection(readLines(file[1])[-1]), header = TRUE, stringsAsFactors = FALSE),
                     "year", "value", -c(scenario, region, sector, subsector, output, technology, Units, X)))
Agprod1 <- get(quary) %>%
  within(rm(X, scenario)) %>% mutate(year = as.numeric(substr(year,2,5))) %>% rename(landleaf = technology, unit = Units) %>%
  group_by(region, crop = output, year, unit) %>% summarise(value = sum(value)) %>% ungroup() %>%
  mutate(crop = tolower(crop))

quary = "Agdemand"; file = paste0(Rawdatapath, quary, ".csv")
assign(quary,  tidyr::gather(read.csv(textConnection(readLines(file[1])[-1]), header = TRUE, stringsAsFactors = FALSE),
                             "year", "value", -c(scenario, region, input, sector, Units, X)))
Agdemand1 <- get(quary) %>%
  within(rm(X, scenario)) %>% mutate(year = as.numeric(substr(year,2,5))) %>% rename(unit = Units, crop = input) %>%
  mutate(crop = if_else(str_detect(crop, "regional"),tolower(substr(crop,nchar("regional")+2, nchar(crop))), tolower(crop))) %>%
  group_by(region, crop, year, unit) %>% summarise(value = sum(value)) %>% ungroup()

quary = "RegAgsource"; file = paste0(Rawdatapath, quary, ".csv")
assign(quary,  tidyr::gather(read.csv(textConnection(readLines(file[1])[-1]), header = TRUE, stringsAsFactors = FALSE),
                             "year", "value", -c(scenario, region, sector, subsector, input, Units, X)))
RegAgsource1 <- RegAgsource %>%
  within(rm(X, scenario)) %>% mutate(year = as.numeric(substr(year,2,5))) %>% rename(unit = Units, crop = sector) %>%
  mutate(crop = if_else(str_detect(crop, "regional"),tolower(substr(crop,nchar("regional")+2, nchar(crop))), tolower(crop))) %>%
  mutate(source = if_else(str_detect(subsector, "domestic"), "domestic", "imported")) %>%
  dplyr::select(region, crop, year, unit, source, value)

#plot stars here
#test for corn and 2100

cc ="corn"
yy = 2100
trade <- Agdemand1 %>% filter(crop == cc) %>% rename(consume = value)  %>% 
  left_join(Agprod1 %>% filter(crop == cc) %>% rename(prod= value)) %>% 
  left_join(RegAgsource1 %>% filter(crop == cc,  source == "imported")%>% rename(imported = value)) %>%
  mutate(export = prod + imported - consume, domestic = prod - export) %>% 
  filter(year == yy) %>% left_join(Regmap)

#note that nn is the number of the regions presented after the automated aggregation (based on flow size)
nn = 10  #nn<31 regions

gridcol = rand_color(nn+2) #given random colors
  
    dat_circular0 <- trade %>% mutate(world = "World", RegID = as.character(RegID)) %>%
       mutate(RegNO = substr(RegID,2,nchar(RegID))) %>% arrange(as.numeric(RegNO)) 
    
    dd <- trade %>% mutate(world = "world", RegID = as.character(RegID)) %>%
      mutate(RegNO = substr(RegID,2,nchar(RegID))) %>% arrange(as.numeric(RegNO))  %>%
      mutate(topplayer = abs(imported) + abs(export)) %>% 
      arrange(desc(topplayer)) %>% top_n(nn) %>% 
      dplyr::select(RegID1) %>% add_row(RegID1 = "World") %>% 
      rename(RegID2 = RegID1) %>% mutate(RegID2 = as.character(RegID2))
    
    dd1 <- unique(dd$RegID2)
    
    dat_circular <- dat_circular0 %>% select(REG_ex = RegID1, REG_im = world, flow = export) %>% 
      bind_rows(dat_circular0 %>% mutate(REG_ex = "World") %>% select(REG_ex, REG_im = RegID1, flow = imported)) %>%
      mutate(REG_ex = if_else(REG_ex %notin% dd$RegID2, "Other regions", REG_ex)) %>%
      mutate(REG_im = if_else(REG_im %notin% dd$RegID2, "Other regions", REG_im)) %>% 
      group_by(REG_ex, REG_im) %>% summarise(flow = sum(flow)) #%>% arrange(flow)
    
    #save figure to default wd
      png(file = "test.png", width = 5000, height = 5000, res = 500)
      
      #circos.par(start.degree = -5)
      #circos.par(gap.after = c(5,5,5,5,5,20,20))
      #circos.par(RESET = T)
      
      #base plot
      chordDiagram(as.data.frame(dat_circular), 
                   transparency = 0.5, 
                   directional = 1,
                   direction.type = c("diffHeight", "arrows"), 
                   diffHeight = -uh(2, "mm")
                   ,link.arr.type = "big.arrow"
                   #, col = linkcol
                   , grid.col = gridcol
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
        
        if(abs(xplot[2] - xplot[1]) < 20) {
          circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                      niceFacing = TRUE, adj = c(0, 0.5), col = "black", 
                      cex = 1)
        } else {
          circos.text(mean(xlim), ylim[1], sector.name, facing = "inside", 
                      niceFacing = TRUE, adj = c(0.5, 0), col= "black", 
                      cex = 1)
        }  }, bg.border = NA)
      
      dev.off() #dump
      



