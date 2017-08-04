## Load libraries
library(plyr)
library(rvest)
library(visNetwork)
library(htmlwidgets)
library(stringr)
library(dplyr)
library(dtplyr)
library(data.table)
library(tidyr)
library(circlize)
library(ggmap)
library(dplyr)
library(geosphere)
library(purrr)
library(marmap)
#library(parallel)
library(gridExtra)
library(raster)
library(data.table)
library(dplyr)
library(ggplot2)
library(magrittr)
library(png)
library(caTools)
library(ndtv)

setwd("/Users/ap186094/Documents/GradDip Data Science/FIT5147 Data Exploration and Visualisation/Assessment 4")

## Initialise variables
college_players_scrape <- NULL
urls <-NULL


## Generate URLs to scrape
for (i in 1:26){
  for (j in 1:20){
    urls_ij <- paste("http://www.footballdb.com/players/players.html?page="
                     , j
                     , "&letter="
                     , LETTERS[i]
                     , sep = "")
    urls <- rbind(urls, urls_ij)
  }
}
##urls[1]
##View(urls)


# Scrape website
for (i in 1:length(urls)){
  url <-  urls[i]
  college_players_i <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="leftcol"]/div[5]/table') %>%
    html_table()
  college_players_i_df <- data.frame(matrix(unlist(college_players_i), ncol=4, byrow=F),stringsAsFactors=FALSE)
  college_players_scrape <- rbind(college_players_scrape, college_players_i_df)
}

# Assign column names
colnames(college_players_scrape) <- c("Player", "Position", "College", "Pro_career")

# Write to csv
 write.csv(college_players_scrape, file = "college_players_scrape.csv")

#====================================





# Read from csv
college_players <- read.csv("college_players_scrape.csv")

# Add player_id index
college_players$player_id <- 1:nrow(college_players) 

# Add Path
college_players$Path <- paste(college_players$College
                              ,gsub('[[:digit:]]+', '', 
                                    gsub('[[:blank:]]+', '', 
                                         gsub('-', '', college_players$Pro_career)))
                              ,sep = ','
                              )

# Unpivot team stints
college_players_unpivotted <- college_players %>% 
  mutate(Teams_stint = strsplit(as.character(Pro_career), ', ')) %>% 
  unnest(Teams_stint)

# Flags for Ranges and NFLE
college_players_unpivotted$IsRange <- grepl("-", college_players_unpivotted$Teams_stint)
college_players_unpivotted$IsNFLE <- grepl("NFLE:", college_players_unpivotted$Teams_stint)
college_players_unpivotted$Teams_stint <- sub('NFLE: ', '', college_players_unpivotted$Teams_stint)


# Split Teams into Year element and Team element
college_players_unpivotted <- college_players_unpivotted %>% mutate(Teams_stint = strsplit(as.character(Teams_stint), ' '))
Teams_stint_temp <- as.data.frame(t(as.data.frame(lapply(college_players_unpivotted$Teams_stint,list))))
colnames(Teams_stint_temp) <- c( "Year_range", "Team")
college_players_unpivotted <- bind_cols(college_players_unpivotted,Teams_stint_temp)

college_players_unpivotted_backup <-college_players_unpivotted

#=====================


# Year_range to Year_list
# Split Year_range into stints
college_players_unpivotted <-college_players_unpivotted_backup
college_players_unpivotted <-  college_players_unpivotted %>% mutate(Year_range_continuous = strsplit(as.character(Year_range), ',')) %>% unnest(Year_range_continuous, .drop = FALSE)

# Create UDF that takes year input and converts to list of years
RangeToList <- function(x) {
  first <- str_sub(x,1,4)
  last <- str_sub(x,-4,-1)
  if(grepl('[0-9]',x)) {return(first:last)}
  else {return(NULL)}
}

# Applies UDF to each 
college_players_unpivotted$Year <- lapply(college_players_unpivotted$Year_range_continuous, RangeToList)

# One row per year
college_players_unpivotted <- college_players_unpivotted %>% unnest(Year, .drop = FALSE)

# Convert 'Teams' to character
college_players_unpivotted$Team <-paste(college_players_unpivotted$Team)

# Write to csv
#write.csv(college_players_unpivotted, file = "college_players_unpivotted")
college_players_unpivotted_backup2 <- college_players_unpivotted

# Create row per player per pro season, drop unwanted columns
pro_careers <- college_players_unpivotted[c("player_id", "Player", "Position", "Team", "Year")]
pro_careers$Level <- "Professional"

# Create row for per player for college
college_careers <- unique(college_players_unpivotted[c("player_id", "Player", "Position", "College")])
colnames(college_careers) <- c("player_id", "Player", "Position", "Team")
college_careers$Year <- -1
college_careers$Level <- "College"

# Merge college and pro sets
careers <- rbind(college_careers, pro_careers)

# Assign college year
careers <- careers[order(careers$player_id),]
row.names(careers) <- 1:nrow(careers)
for (i in 1:(dim(careers)[1]))
{
  careers$Year[i] <- if(careers$Year[i] == -1) {careers$Year[i+1] -1 } else {next}
}

write.csv(careers, file = "careers.csv")

#### GEOCODE ########################################

# Create lists of college teams and pro teams
#pro_teams <- rapply(list(unique(pro_careers$Team)), as.character, classes="factor", how="replace")
#college_teams <- rapply(list(unique(college_careers$Team)), as.character, classes="factor", how="replace")

# Create df of college teams and pro teams
#pro_teams <- rapply(data.frame(unique(pro_careers$Team)), as.character, classes="factor", how="replace")
#college_teams <- rapply(data.frame(unique(college_careers$Team)), as.character, classes="factor", how="replace")


# Create factor lists of college teams and pro teams
#pro_teams <- unique(pro_careers$Team)
#college_teams <- unique(college_careers$Team)




college_teams <-data.frame(sort(unique(college_careers$Team)))

# Create UDF that takes college name and appends 'University' if it does not contain 'University' or 'College'
Uni_namer <- function(x) {
  if(grepl("college|university", x, ignore.case = TRUE)) {return(x)}
  else if(str_sub(x,-1,-1) == ")") {return(gsub("\\(", "University (", x))}
  else {return(paste(x, "University"))}
}

#Prepare college names using UDF
college_teams <- lapply(college_teams, Uni_namer)

#Geocode
college_teams$LatLong <- lapply(college_teams, geocode)
college_teams_backup <- college_teams
save(college_teams, file = "college_teams")






# # Geocode second pass (some don't work in first attempt)
# college_teams_regeocode <- subset.data.frame(college_teams,is.na(Longitude))["College"]
# college_teams_regeocode$LatLong <- lapply(list(college_teams_regeocode), geocode)


#Geocode Pro teams
pro_teams_backup <- pro_teams
pro_teams <- read.csv(file = "NFL_Teams.csv")
pro_teams %>% mutate(Site = paste(City,State...Country., sep = ", ")) -> pro_teams
pro_teams <- pro_teams[c("Team", "Site")]
pro_teams$LatLong <- lapply(pro_teams$Site, geocode)
save(pro_teams, file = "pro_teams")








#### Data manipulation ###
setwd("/Users/ap186094/Documents/GradDip Data Science/FIT5147 Data Exploration and Visualisation/Assessment 4")

careers <- read.csv("careers.csv")

load("pro_teams")
pro_teams <-bind_cols(pro_teams[c("Team","Site")], rbindlist(pro_teams$LatLong))
pro_teams$level <-  "Professional"

load("college_teams")
# Not sure why this is necessary, but otherwise 'college_teams' is a list
college_teams  <- data.frame(college_teams)
college_teams <- cbind(college_teams, data.frame(sort(unique(careers$Team[careers$Level == "College"]))))
college_teams <- college_teams[,c(4,1,2,3)]
colnames(college_teams) <- c("Team", "Site", "lon", "lat")
college_teams$level <-  "College"






##### Join college and pro teams lists
Teams <- rbind(college_teams, pro_teams)
write.csv(Teams, "Teams.csv")

# List of trades and movements
#View(careers)
careers <- careers[1:7] 
careers_backup <- careers


careers <- careers_backup[1:7] 

# Self-join to previous row
careers$index <- as.integer(row.names(careers))

careers$index_with_offset <- careers$index - 1
careers$Movement_type <- "Draft"
careers <- merge(careers, careers[c("index","player_id","Team", "Level")], by.x = "index_with_offset", by.y = "index", suffixes = c("","_previous_season"), all.x = TRUE)
careers$Team_previous_season[careers$player_id != careers$player_id_previous_season] <- NA
careers$Level_previous_season[careers$player_id != careers$player_id_previous_season] <- NA
careers$Movement_type[careers$player_id != careers$player_id_previous_season] <- NA
careers$Movement_type[careers$Level == "Professional" & careers$Level_previous_season == "Professional"] <- "Trade"
careers$player_id_previous_season <- NULL
careers$index_with_offset <- NULL



careers_movements <- na.omit(careers[careers$Team != careers$Team_previous_season,])


# Summarise
movements_by_year <- ddply(careers_movements
                        , c(.(Year), .(Movement_type),. (Team_previous_season), .(Team))
                        , nrow)
write.csv(movements_by_year, "movements_by_year.csv")

movements_by_year_backup <- movements_by_year
#View(movements_by_year)











# #### Viz - early attempts (not used in final analysis) ########################################
# 
# chosen_year <- 2014
# #gif_frames <- array(data = NA)
# #for (chosen_year in 1920:2016) {
# 
# 
# #chosen_trades <- movements_by_year[, 2:4]
# chosen_trades <- movements_by_year[movements_by_year$Year == chosen_year & movements_by_year$Movement_type == "Pro" , 3:5]
# colnames(chosen_trades) <- c("start", "end", "weight")
# rownames(chosen_trades) <- NULL
# 
# library("igraph")
# trades <- data.frame(start = c("Atl","Atl","Bal","Bal","Chi","Chi"), end = c("Bal","Chi","Atl","Chi","Atl","Bal"),weight = c(20,40,20,30,50,10))
# #trades <- chosen_trades[60,]
# #trades <- chosen_trades
# Teams <- read.csv("Teams.csv")
# coords <- Teams[c("Team","lat","lon")]
# 
# # non existent coords are temporarily set to 0,0
# coords$lat[is.na(coords$lat)] <- 0
# coords$lon[is.na(coords$lon)] <- 0
# 
# 
# 
# 
# g <- graph.data.frame(trades, directed = T)
# 
# V(g)$name 
# E(g)$weight
# E(g)$from <- ends(g,  E(g), names = TRUE)[,1]
# E(g)$to <- ends(g,  E(g), names = TRUE)[,2]
# 
#     sort_var_temp <- cbind(E(g)$to, 1:length(E(g)))
#     colnames(sort_var_temp) <- c("Team","Order")
#     
#     team_colors <- data.frame(c("Mia", "Chi", "SD", "GB", "Det", "Ten", "NO", "Den", "Cle", "Ari", "Min", "KC", "Cin", "TB", "NE", "Pit", "Phi", "NYJ", "Hou", "Sea", "Car", "Atl", "SF", "Bal", "Ind", "NYG", "Dal", "Buf", "Was", "Stl", "Jax", "Oak"))
#     colnames(team_colors) <- "Team"
#     team_colors$Colour <- "gray20"
#     
#     team_colors[team_colors$Team == "Ari",][2] <- "#9B2743"
#     team_colors[team_colors$Team == "Atl",][2] <-  "#A6192D"
#     team_colors[team_colors$Team == "Bal",][2] <-  "#280353"
#     team_colors[team_colors$Team == "Buf",][2] <-  "#00338D"
#     
#     team_colors[team_colors$Team == "Car",][2] <- "#0088CE"
#     team_colors[team_colors$Team == "Chi",][2] <-  "#03202F"
#     team_colors[team_colors$Team == "Cin",][2] <-  "#FB4F14"
#     team_colors[team_colors$Team == "Cle",][2] <-  "#eb3300"
#     
#     team_colors[team_colors$Team == "Dal",][2] <- "#0D254C"
#     team_colors[team_colors$Team == "Den",][2] <-  "#FB4F14"
#     team_colors[team_colors$Team == "Det",][2] <-  "#006DB0"
#     team_colors[team_colors$Team == "GB",][2] <-  "#203731"
#     
#     team_colors[team_colors$Team == "Hou",][2] <-  "#02253A"
#     team_colors[team_colors$Team == "Ind",][2] <- "#003B7B"
#     team_colors[team_colors$Team == "Jax",][2] <-  "#9F792C"
#     team_colors[team_colors$Team == "KC",][2] <-  "#B20032"
#     
#     team_colors[team_colors$Team == "Mia",][2] <-  "#008D97"
#     team_colors[team_colors$Team == "Min",][2] <- "#582C81"
#     team_colors[team_colors$Team == "NE",][2] <-  "#0D254C"
#     team_colors[team_colors$Team == "NO",][2] <-  "#D2B887"
#     
#     team_colors[team_colors$Team == "NYG",][2] <-  "#192F6B"
#     team_colors[team_colors$Team == "NYJ",][2] <- "#0C371D"
#     team_colors[team_colors$Team == "Oak",][2] <-  "#000000"
#     team_colors[team_colors$Team == "Phi",][2] <-  "#003B48"
#     
#     team_colors[team_colors$Team == "Pit",][2] <-  "#FFB612"
#     team_colors[team_colors$Team == "SD",][2] <- "#0C2340"
#     team_colors[team_colors$Team == "Sea",][2] <-  "#69BE28"
#     team_colors[team_colors$Team == "SF",][2] <-  "#AF1E2C"
#     
#     team_colors[team_colors$Team == "Stl",][2] <- "#13264B"
#     team_colors[team_colors$Team == "TB",][2] <-  "#D60A0B"
#     team_colors[team_colors$Team == "Ten",][2] <-  "#648FCC"
#     team_colors[team_colors$Team == "Was",][2] <-  "#773141"
#     
#     
#     sort_var_temp2 <- merge(sort_var_temp, team_colors, sort = FALSE)
#     sort_var_temp2$Order <- as.integer(lapply(sort_var_temp2$Order,as.character))
#     sort_var_temp2[order(sort_var_temp2$Order), ][,3]
# 
# E(g)$color <- sort_var_temp2[order(sort_var_temp2$Order), ][,3]
# 
# ideg <- degree(g, mode = "in", loops = F)
# 
# col= "gray20" #rainbow(12) # For edge colors
# 
# 
# ## Node Layout
# nodes <- data.frame(V(g)$name)
# colnames(nodes) <- "Team"
# 
# l_df <- merge(nodes, coords, sort = FALSE)[c("lon", "lat")]
# l <- as.matrix(l_df)
# 
# 
# 
# png(paste(c("img/",chosen_year,'.png'), sep = "", collapse = ""))
# plot.igraph(g, 
#             vertex.label = V(g)$label, 
#             vertex.label.color = "gray20",
#             vertex.size = 1, #ideg*25 + 40
#             vertex.size2 = 1,
#             vertex.color = "gray90", vertex.frame.color = "gray20",
#             vertex.shape = "circle",
#             edge.arrow.size= 0, 
#             edge.color=E(g)$color, 
#             edge.width = E(g)$weight / 5,
#             edge.curved = T, 
#             main = "NFL Trades",
#             sub = as.character(chosen_year),
#             layout = l)
# dev.off()
# 
# gif_frames[chosen_year] <-readPNG(paste(c("img/",chosen_year,'.png'), sep = "", collapse = ""))
# #}
# 
# ######visIgraph#####
# toVisNetworkData(g)
# visIgraph(g)
# 
# 
# movements_by_year
# 
# 
# 
# ######### StatNet###########
# 
# 
# library(ndtv)
# 
# #test dataset
# movements_by_year <- movements_by_year_backup
# movements_by_year <- movements_by_year[movements_by_year$Movement_type == 'Trade',]
# 
# movements_by_year$onset <- as.numeric(movements_by_year$Year) -1
# movements_by_year$terminus <- as.numeric(movements_by_year$Year)
# 
# levels <- with(movements_by_year, unique(c(as.character(Team_previous_season), as.character(Team))))
# movements_by_year$tail <-  as.numeric(factor(movements_by_year$Team_previous_season, levels))
# movements_by_year$head <-  as.numeric(factor(movements_by_year$Team, levels))
# movements_by_year$onset.censored <- TRUE
# movements_by_year$terminus.censored <- TRUE
# movements_by_year$duration <- 1
# movements_by_year$edge.id <- as.numeric(as.factor(paste(movements_by_year$Team_previous_season, movements_by_year$Team)))
# 
# movements_by_year <- movements_by_year[6:13]
# 
# select_years <- movements_by_year[movements_by_year$terminus>1970,]
# 
# # Create network object
# nd <- networkDynamic(edge.spells = select_years)
# 
# 
# #Set static coordinates
# lat_long_coord_ND <- merge(levels,  Teams, by.x = "x", by.y = "Team", all.x=TRUE, sort = FALSE)
# 
# staticCoords <- lat_long_coord_ND[,c("lon", "lat")]
# staticCoords$lon[is.na(staticCoords["lon"])] <- 0
# staticCoords$lat[is.na(staticCoords["lat"])] <- 0
# 
# 
# activate.vertex.attribute(nd,'x',staticCoords[,1],onset=-Inf,terminus=Inf)
# activate.vertex.attribute(nd,'y',staticCoords[,2],onset=-Inf,terminus=Inf)
# 
# 
# activate.vertex.attribute(nd,'vertex.names',levels,onset=-Inf,terminus=Inf)
# activate.vertex.attribute(nd,'ColPro',lat_long_coord_ND$level,onset=-Inf,terminus=Inf)
# 
# activate.edge.attribute(nd,'weight',100,onset=-Inf,terminus=Inf)
# 
# 
# reconcile.edge.activity(nd, mode="reduce.to.vertices")
# get.vertex.activity(nd,as.spellList=TRUE)
# reconcile.vertex.activity(nd,"match.to.edges", edge.active.default = TRUE)
# 
# compute.animation(nd,animation.mode='useAttribute')
# 
# #render
# 
# render.d3movie(nd,animation.mode='useAttribute',output.mode = 'htmlWidget', displaylabels=FALSE,vertex.ColPro="Professional")
# 
# ########## CALCULATE NODE PERSISTENCE: FOR WHICH YEARS IS EACH NODE ACTIVE
# 
