#try to plot map with names of sites... 

library(openair)
library(ggmap)
library(maps)
library(mapdata)
library(RColorBrewer)


#make colours 

# Classic palette BuPu, with 4 colors
coul = brewer.pal(11, "Paired") 
# I can add more tones to this palette :
coul = colorRampPalette(coul)(18)
# Plot it
pie(rep(1, length(coul)), col = coul , main="") 

mycols <- c("") 
mycols[c(1:2)] <- c("black", "darkgrey")
mycols[c(3:20)] <- coul
mycols[c(21:28)] <- brewer.pal(8, "Dark2")
mycols[28] <- "red"
mycols[29] <- "lightgrey"

site.col <- c("MUMBA", "UOW", "Bargo", "Bringelly", "Macarthur","Oakdale", "Richmond","St_Marys", "Vineyard", "Chullora", "Earlwood", "Lindfield", "Liverpool", "Prospect", "Randwick", "Rozelle", "Westmead", "Wollongong", "Kembla Grange", "Albion_Park_Sth", "Bellambi", "Badgerys_Creek", "Bankstown_Airport", "Camden_Airport", "Richmond_RAAF", "Sydney_Airport", "Williamtown_RAAF", "Wollongong_Airport", "Warrawong")
col.df <- data.frame(site = site.col, col = mycols)


#then, merge colours with site_info 

site_map_info <- merge(site_info, col.df, by = "site") 

#for CASANZ slide 
site.col.oeh <- c("Bargo", "Bringelly", "Macarthur","Oakdale", "Richmond","St_Marys", "Vineyard", "Chullora", "Earlwood", "Lindfield", "Liverpool", "Prospect", "Randwick", "Rozelle", "Westmead", "Wollongong", "Kembla Grange", "Albion_Park_Sth")
col.oeh <- rep("purple", 18)
site.col.bom <- c("Bellambi", "Badgerys_Creek", "Bankstown_Airport", "Camden_Airport", "Richmond_RAAF", "Sydney_Airport", "Williamtown_RAAF", "Wollongong_Airport")
col.bom <- rep("red",8)
col.df.oeh <- data.frame(site = site.col.oeh, col = col.oeh)
col.df.bom <- data.frame(site = site.col.bom, col = col.bom)

col.df.all <- rbind(col.df.oeh,col.df.bom)

site_map_info <- merge(site_info, col.df.all, by = "site") 

#try to map this 

map <- get_map(location = c(150.7, -33.5), maptype = "satellite", source = "google", zoom = 8)

col_custom <- site_map_info$col

(ggmap(map) + 
    geom_point(data = site_map_info, 
               mapping = aes(y = site_lat, x =site_lon, fill = site), color = col_custom) )


ggmap(zoom_map) + 
    geom_point(data = site_map_info, 
               mapping = aes(y = site_lat, x =site_lon, fill = site), color = col_custom) 


zoom_map <- get_map(location = c(150.8, -34), maptype = "satellite", source = "google", zoom = 9)
ggmap(zoom_map)

Box <- data.frame(lat = c(-32.2458213055, -32.2458213055, -34.66993595, -34.66993595),
                  lon = c(152.1507788472, 149.83520577, 149.83520577,152.1507788472))

ggmap(map) + 
    geom_point(data = site_map_info, 
               mapping = aes(y = site_lat, x =site_lon, fill = site), color = col_custom) + 
    geom_polygon(data = Box, mapping = aes(y = lat, x = lon), color = "red", fill = NA)
