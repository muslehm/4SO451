library(statnet)
library(GGally)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

options(scipen = 999) 
wd <- getwd()
setwd(wd)



df1 <- read.csv('../Data/tal3at_Sep25Oct7(2).csv', header = TRUE)

##Replace date of account creation with Boolean TRUE if account created before the announcement of the movement
##The first tweet on their official account####
time<- df1$account_created_at
g_date = as.Date("2019-09-07")
z=1
tlist = list()
for (i in time){
  istna <- i
  istna <- as.numeric(istna)
  if (is.na(istna))
  {
    
    istna <- as.Date(i, origin="1970-01-01") 
    
  }
  else{
    istna <- as.Date(as.POSIXct(as.numeric(i), origin="1970-01-01"))
  }
  if (istna > g_date)
  {
    tlist[[z]] = "FALSE"
  }
  else
  {
    tlist[[z]]= "TRUE"
  }
  z= z+1
}

tal3at_C <- unlist(tlist, use.names=FALSE)
df1$account_created_at <- tal3at_C

##### Create Edgelist #####

#Create edge list for replies
r_edgelist <-  df1 %>% 
  filter(!is.na(reply_to_user_id)) %>% #only choose replies
  select(user_id, reply_to_user_id)  %>% #select only these two columns
  filter(user_id != reply_to_user_id) #remove loop
  
colnames(r_edgelist) <- c("node1", "node2")

#Create edge list for quoted
q_edgelist <-  df1 %>% #remove duplicate already added in mention
  filter(!is.na(quoted_user_id)) %>% # choose only quotes
  select(user_id, quoted_user_id) %>% #select only these two columns
  filter(user_id != quoted_user_id) #remove loop

colnames(q_edgelist) <- c("node1", "node2")

#  filter(!is.na(mentions_user_id)) %>% #remove isolates
m_edgelist <-  df1 %>% 
  select(user_id, mentions_user_id) 
m_edgelist$user_id <- as.character(m_edgelist$user_id)
m_edgelist$mentions_user_id <- gsub("c\\(\"", "", m_edgelist$mentions_user_id)
m_edgelist$mentions_user_id <- gsub("\\)", "", m_edgelist$mentions_user_id)
m_edgelist$mentions_user_id <- gsub("\"", "", m_edgelist$mentions_user_id)

dlist = list()
z = 0
is.na(m_edgelist[2,]$mentions_user_id)
for(i in 1:nrow(m_edgelist)) {
  
  mentions <- str_split(m_edgelist[i,]$mentions_user_id, ', ')
  for (p in mentions[[1]]){
    z = z + 1
    conn<-data.frame(user = m_edgelist[i,]$user_id, mention = p)
    dlist[[z]] <- conn
  }
  
}

all_data <- do.call(rbind, dlist)
colnames(all_data) <- c("node1", "node2")

all_data$node2[is.na(all_data$node2)]<- "0"
all_data_ <- all_data %>% #Remove self-loop
  filter(node2 != node1)
all_data_ <- all_data_ %>% #Remove mention none
  filter(all_data_$node2 != '0')

#Create edge list for retweets
rt_edgelist <-  df1 %>% 
  filter(!is.na(retweet_user_id)) %>% #filter to retweets only
  select(user_id, retweet_user_id) %>% #select only these two columns
  filter(user_id != retweet_user_id) #remove loop

colnames(rt_edgelist) <- c("node1", "node2")

##Combine all interactions
interactions <- rbind(all_data_, r_edgelist, rt_edgelist, q_edgelist)

########
##Remove Duplicates####
interactions$node2[interactions$node2=="876928828530151425"]  <- "876928828530151424"
interactions$node2[interactions$node2=="1171674661421236225"]  <- "1171674661421236224"
interactions$node2[interactions$node2=="836647049688977410"]  <- "836647049688977408"
interactions$node2[interactions$node2=="1165964924079804417"]  <- "1165964924079804416"
interactions$node2[interactions$node2=="1105210664522575873"]  <- "1105210664522575872"
interactions$node2[interactions$node2=="1155788381697323014"]  <- "1155788381697323008"
interactions$node2[interactions$node2=="1178378952911523843"]  <- "1178378952911523840"
interactions$node2[interactions$node2=="698953991091163140"]  <- "698953991091163136"
interactions$node2[interactions$node2=="748685721431642116"]  <- "748685721431642112"
interactions$node2[interactions$node2=="1156959607912312833"]  <- "1156959607912312832"
interactions$node2[interactions$node2=="1018294379138699265"]  <- "1018294379138699264"
interactions$node2[interactions$node2=="1142553789213433858"]  <- "1142553789213433856"
interactions$node2[interactions$node2=="816691630644334593"]  <- "816691630644334592"
interactions$node2[interactions$node2=="945078488457011201"]  <- "945078488457011200"
interactions$node2[interactions$node2=="984388954022727682"]  <- "984388954022727680"
interactions$node2[interactions$node2=="984362710681911298"]  <- "984362710681911296"
interactions$node2[interactions$node2=="1187770945898995712"]  <- "1187770945899000064"
interactions$node2[interactions$node2=="1116796290342490112"]  <- "1116796290342490113"
interactions$node2[interactions$node2=="823336729620967426"]  <- "823336729620967424"
interactions$node2[interactions$node2=="1095789390809497601"]  <- "1095789390809497600"
interactions$node2[interactions$node2=="1116807358867542021"]  <- "1116807358867542016"
interactions$node2[interactions$node2=="1172899027312140289"]  <- "1172899027312140288"
interactions$node2[interactions$node2=="1010377508976459777"]  <- "1010377508976459776"
interactions$node2[interactions$node2=="1018925143995928576"]  <- "1018925143995928577"
interactions$node2[interactions$node2=="1021814674977968135"]  <- "1021814674977968128"
interactions$node2[interactions$node2=="1045354576105353218"]  <- "1045354576105353216"
interactions$node2[interactions$node2=="1048556592990437376"]  <- "1048556592990437377"
interactions$node2[interactions$node2=="1057321402968301569"]  <- "1057321402968301568"
interactions$node2[interactions$node2=="1083513491133206528"]  <- "1083513491133206531"
interactions$node2[interactions$node2=="1101407363762921473"]  <- "1101407363762921472"
interactions$node2[interactions$node2=="1102570260610326528"]  <- "1102570260610326529"
interactions$node2[interactions$node2=="1127482903833628672"]  <- "1127482903833628673"
interactions$node2[interactions$node2=="895894784749056000"]  <- "895894784749056001"
interactions$node2[interactions$node2=="1141908386382000000"]  <- "1141908386382000128"
interactions$node2[interactions$node2=="989274514767872000"]  <- "989274514767872001"
interactions$node2[interactions$node2=='913062226596000000']  <- '913062226595999744'
interactions$node2[interactions$node2=='1171142578496000000']  <- '1171142578495995904'
interactions$node2[interactions$node2=='1140524883862000000']  <- '1140524883861999616'
interactions$node2[interactions$node2=='1151552483443000064']  <- '1151552483442995200'
interactions$node2[interactions$node2=='1134253041186684930']  <- '1134253041186684928'
interactions$node2[interactions$node2=='1134833981327912962']  <- '1134833981327912960'
interactions$node2[interactions$node2=='1152646439958384643']  <- '1152646439958384640'
interactions$node2[interactions$node2=='1152670934295285761']  <- '1152670934295285760'
interactions$node2[interactions$node2=='1155234138535993345']  <- '1155234138535993344'
interactions$node2[interactions$node2=='1159812112287490053']  <- '1159812112287490048'
interactions$node2[interactions$node2=='1170328334435323914']  <- '1170328334435323904'
interactions$node2[interactions$node2=='1171942541815623686']  <- '1171942541815623680'
interactions$node2[interactions$node2=='1175059145030025221']  <- '1175059145030025216'
interactions$node2[interactions$node2=='1195272479738155009']  <- '1195272479738155008'
interactions$node2[interactions$node2=='720146404602195969']  <- '720146404602195968'
interactions$node2[interactions$node2=='798074384036597760']  <- '798074384036597761'
interactions$node2[interactions$node2=='815123072474902529']  <- '815123072474902528'
interactions$node2[interactions$node2=='815627662043529217']  <- '815627662043529216'
interactions$node2[interactions$node2=='816893187319013377']  <- '816893187319013376'
interactions$node2[interactions$node2=='830981265398566916']  <- '830981265398566912'
interactions$node2[interactions$node2=='853306464269725697']  <- '853306464269725696'
interactions$node2[interactions$node2=='880729084661293057']  <- '880729084661293056'
interactions$node2[interactions$node2=='885762745748115457']  <- '885762745748115456'
interactions$node2[interactions$node2=='899374827114557441']  <- '899374827114557440'
interactions$node2[interactions$node2=='924358188559892481']  <- '924358188559892480'
interactions$node2[interactions$node2=='940994087511117825']  <- '940994087511117824'
interactions$node2[interactions$node2=='954349703726092289']  <- '954349703726092288'
interactions$node2[interactions$node2=='954775706747187201']  <- '954775706747187200'
interactions$node2[interactions$node2=='940994087511117825']  <- '940994087511117824'
interactions$node2[interactions$node2=='954349703726092289']  <- '954349703726092288'
interactions$node2[interactions$node2=='1063190502432999936']  <- '1063190502433001472'

interactions$node1[interactions$node1=='1063190502432999936']  <- "1063190502433001472"
interactions$node1[interactions$node1=='954775706747187201']  <- "954775706747187200"
interactions$node1[interactions$node1=='954349703726092289']  <- "954349703726092288"
interactions$node1[interactions$node1=='940994087511117825']  <- "940994087511117824"
interactions$node1[interactions$node1=='924358188559892481']  <- "924358188559892480"
interactions$node1[interactions$node1=='899374827114557441']  <- "899374827114557440"
interactions$node1[interactions$node1=='885762745748115457']  <- "885762745748115456"
interactions$node1[interactions$node1=='880729084661293057']  <- "880729084661293056"
interactions$node1[interactions$node1=='853306464269725697']  <- "853306464269725696"
interactions$node1[interactions$node1=='830981265398566916']  <- "830981265398566912"
interactions$node1[interactions$node1=='816893187319013377']  <- "816893187319013376"
interactions$node1[interactions$node1=='815627662043529217']  <- "815627662043529216"
interactions$node1[interactions$node1=='815123072474902529']  <- "815123072474902528"
interactions$node1[interactions$node1=='798074384036597760']  <- "798074384036597761"
interactions$node1[interactions$node1=='720146404602195969']  <- "720146404602195968"
interactions$node1[interactions$node1=='1195272479738155009']  <- "1195272479738155008"
interactions$node1[interactions$node1=='1175059145030025221']  <- "1175059145030025216"
interactions$node1[interactions$node1=='1171942541815623686']  <- "1171942541815623680"
interactions$node1[interactions$node1=='1170328334435323914']  <- "1170328334435323904"
interactions$node1[interactions$node1=='1159812112287490053']  <- "1159812112287490048"
interactions$node1[interactions$node1=='1155234138535993345']  <- "1155234138535993344"
interactions$node1[interactions$node1=='1152670934295285761']  <- "1152670934295285760"
interactions$node1[interactions$node1=='1152646439958384643']  <- "1152646439958384640"
interactions$node1[interactions$node1=='1134833981327912962']  <- "1134833981327912960"
interactions$node1[interactions$node1=='1134253041186684930']  <- "1134253041186684928"
interactions$node1[interactions$node1=='1151552483443000064']  <- "1151552483442995200"
interactions$node1[interactions$node1=='1140524883862000000']  <- "1140524883861999616"
interactions$node1[interactions$node1=='1171142578496000000']  <- "1171142578495995904"
interactions$node1[interactions$node1=='913062226596000000']  <- "913062226595999744"
interactions$node1[interactions$node1=="989274514767872000"]  <- "989274514767872001"
interactions$node1[interactions$node1=="1141908386382000000"]  <- "1141908386382000128"
interactions$node1[interactions$node1=="895894784749056000"]  <- "895894784749056001"
interactions$node1[interactions$node1=="1127482903833628672"]  <- "1127482903833628673"
interactions$node1[interactions$node1=="1102570260610326528"]  <- "1102570260610326529"
interactions$node1[interactions$node1=="1101407363762921473"]  <- "1101407363762921472"
interactions$node1[interactions$node1=="1083513491133206528"]  <- "1083513491133206531"
interactions$node1[interactions$node1=="1057321402968301569"]  <- "1057321402968301568"
interactions$node1[interactions$node1=="1048556592990437376"]  <- "1048556592990437377"
interactions$node1[interactions$node1=="1045354576105353218"]  <- "1045354576105353216"
interactions$node1[interactions$node1=="1021814674977968135"]  <- "1021814674977968128"
interactions$node1[interactions$node1=="1018925143995928576"]  <- "1018925143995928577"
interactions$node1[interactions$node1=="1010377508976459777"]  <- "11010377508976459776"
interactions$node1[interactions$node1=="1172899027312140289"]  <- "1172899027312140288"
interactions$node1[interactions$node1=="1116807358867542021"]  <- "1116807358867542016"
interactions$node1[interactions$node1=="1095789390809497601"]  <- "1095789390809497600"
interactions$node1[interactions$node1=="823336729620967426"]  <- "823336729620967424"
interactions$node1[interactions$node1=="1116796290342490112"]  <- "1116796290342490113"
interactions$node1[interactions$node1=="1187770945898995712"]  <- "1187770945899000064"
interactions$node1[interactions$node1=="984362710681911298"]  <- "984362710681911296"
interactions$node1[interactions$node1=="984388954022727682"]  <- "984388954022727680"
interactions$node1[interactions$node1=="945078488457011201"]  <- "945078488457011200"
interactions$node1[interactions$node1=="816691630644334593"]  <- "816691630644334592"
interactions$node1[interactions$node1=="1142553789213433858"]  <- "1142553789213433856"
interactions$node1[interactions$node1=="1018294379138699265"]  <- "1018294379138699264"
interactions$node1[interactions$node1=="1156959607912312833"]  <- "1156959607912312832"
interactions$node1[interactions$node1=="748685721431642116"]  <- "748685721431642112"
interactions$node1[interactions$node1=="698953991091163140"]  <- "698953991091163136"
interactions$node1[interactions$node1=="1178378952911523843"]  <- "1178378952911523840"
interactions$node1[interactions$node1=="1155788381697323014"]  <- "1155788381697323008"
interactions$node1[interactions$node1=="1105210664522575873"]  <- "1105210664522575872"
interactions$node1[interactions$node1=="1165964924079804417"]  <- "1165964924079804416"
interactions$node1[interactions$node1=="876928828530151425"]  <- "876928828530151424"
interactions$node1[interactions$node1=="1171674661421236225"]  <- "1171674661421236224"
interactions$node1[interactions$node1=="836647049688977410"]  <- "836647049688977408"
###########
interactions_ <- unique(interactions)
interactions_ <- interactions_ %>%
  filter(interactions_$node2!='c(NA')

edge_list_u <- interactions_[order(interactions_$node1),]

notusers <- c('10228272', '823905', '1156462412045926400','1168499151669276673', '1169339451467808768',
              '750874439110230016', "1133091555286364160", '701765847270957056')
edge_list_u <- edge_list_u %>%
  filter(!(node1 %in% notusers))%>%
  filter(!(node2 %in% notusers))

#filter unique users in network and create data frame for unique nodes and extract features
users <- c(edge_list_u$node1, edge_list_u$node2)
users <- unique(users)
users <- data.frame(users)
##destruction Zone: destroying variable that we don't need for efficiency####
rm(time, tal3at_C, g_date, tlist, q_edgelist, r_edgelist, rt_edgelist,
   mentions, interactions, dlist, conn, all_data, i, istna, p, z, m_edgelist)

#####
newdf <- df1[df1$user_id %in% users$users,]
newdf <- newdf %>%
  select(user_id, screen_name, location, followers_count, lang, verified, account_created_at) 
newdf_ <-  newdf %>% 
  distinct(newdf$user_id, .keep_all = TRUE)
drops <- c('newdf$user_id')
newdf_ <- newdf_[ , !(names(newdf_) %in% drops)]
colnames(newdf_)
userpost <- data.frame(unique(newdf_$user_id))
usernot <- data.frame(users[!(users$users %in% userpost$unique.newdf_.user_id.),])
colnames(usernot) <- c('user_id')
usernot$screen_name <- NA
usernot$location <- NA
usernot$followers_count <- NA
usernot$lang <- NA
usernot$verified <- NA
usernot$account_created_at <- NA
df_netnodes <- rbind(newdf_, usernot)
checkus <- unique(df_netnodes$user_id)

######Flter Locations####

df_netnodes <- df_netnodes[order(df_netnodes$user_id),]
df_netnodes <- df_netnodes[!(df_netnodes$user_id %in% notusers),]
df_netnodes$gender <- NA
df_netnodes$group <- NA
df_netnodes$known <- NA
#write.csv(df_netnodes,'check.csv', row.names = FALSE, )
df_netnodes_ <- read.csv('../Data/checked.csv', header = TRUE)
df_netnodes_$user_id <- as.character(df_netnodes_$user_id)
df_netnodes_ <-df_netnodes_[order(df_netnodes_$user_id),]
unique(df_netnodes_$location)
##Location Grouping ####
df_netnodes_$location[df_netnodes_$location== "Kuwait"] <- "Gulf"
df_netnodes_$location[df_netnodes_$location== "Qatar"] <- "Gulf"
df_netnodes_$location[df_netnodes_$location== "UAE"] <- "Gulf"
df_netnodes_$location[df_netnodes_$location== "Oman"] <- "Gulf"
#df_netnodes_$location[df_netnodes_$location== "KSA"] <- "Gulf"
df_netnodes_$location[df_netnodes_$location== "Bahrain"] <- "Gulf"

arab <- c( "Morocco", "Lebanon", "Egypt", "Jordan", "Gulf", "Tunisia", "Sudan", "Iraq","Syria","Yemen",
          "Algeria", "Mauritania", "Libya", "Somaliland", "Western Sahara")
region <- c("Turkey", "Iran")
europe <- c("UK", "France", "Sweden", "Germany", "Portugal", "Italy", "Greece", "Spain", "Poland","Ireland",
            "Norway", "Finland", "Czech", "Hungary", "Romania", "Bulgaria","Switzerland", "Austria",
            "Denmark", "Iceland", "Holland", "Belgium")
namerica <- c("USA", "Canada")
samerica <- c("Brazil", "Chile", "Ecuador", "Venezuela", "South America")
asia <- c('Japan', 'Australia', 'Asia', 'Pakistan', 'India', 'Korea', "New Zealand")
africa <- c("Ghana", "South Africa", "Kenya", "Senegal", "Ethiopia", "Mauritius")
df_netnodes_$location[df_netnodes_$location %in% arab ] <- "Arab"
df_netnodes_$location[df_netnodes_$location %in% europe] <- "Europe"
df_netnodes_$location[df_netnodes_$location %in% namerica] <- "North America"
df_netnodes_$location[df_netnodes_$location %in% region] <- "A-A-A-SA"
df_netnodes_$location[df_netnodes_$location %in% asia] <- "A-A-A-SA"
df_netnodes_$location[df_netnodes_$location %in% africa] <- "A-A-A-SA"
df_netnodes_$location[df_netnodes_$location %in% samerica] <- "A-A-A-SA"

#####

location <- df_netnodes_$location
summary(location)

#####Convert Function####
convert_l <- function(x){
  if (as.numeric(x) >10000000) { return ("More than 10M")
  } else if (as.numeric(x) >1000000 & as.numeric(x) < 10000001) {return( "More than 1M") 
  } else if (as.numeric(x) >100000 & as.numeric(x) < 1000001) {return( "More than 100K") 
  } else if (as.numeric(x) >10000 & as.numeric(x) < 100001) { return( "More than 10K")
  } else if (as.numeric(x) >1000 & as.numeric(x) < 10001) {return( "More than 1K") 
  } else if (as.numeric(x) >100 & as.numeric(x) < 1001) {return( "More than 100") 
  } else if (as.numeric(x) >10 & as.numeric(x) < 101) { return( "More than 10") 
  }else if (as.numeric(x) < 11){ return( "10 or less") }
}
#######
gender <- df_netnodes_$gender
summary(gender)
group <- df_netnodes_$group
summary(group)
known <- df_netnodes_$known
summary(known)

fo_count <- df_netnodes_$followers_count
max(fo_count) #7625181
fo_count_1 = list()
for (i in 1:length(fo_count)){
  fo_count_1[[i]] <- convert_l(fo_count[i])}
unique(fo_count_1)

language <- df_netnodes_$lang
summary(language)

verified <- df_netnodes_$verified
summary(verified)
tal3at_date <- df_netnodes_$account_created_at
summary(tal3at_date)
sname <- df_netnodes_$screen_name

##setcolors#####
unique(tal3at_date)
bol_colours <- c('#a6cee3', '#e31a1c')
names(bol_colours) <-  (unique (tal3at_date))

unique(language)
lang_colours <- c('#edea2b',  '#b2df8a',  '#fb9a99', '#1f78b4', '#bebada',  '#e31a1c', '#ffff99', '#b15928', '#8dd3c7', '#ffffb3')
names(lang_colours) <-  (unique (language))

l_colours <- c('#80b1d3', '#edea2b',  '#fb9a99', '#1f78b4', '#bebada',  '#e31a1c', '#ffff99', '#b15928', '#8dd3c7', '#33a02c', '#ffffb3')
names(l_colours) <-  (unique(location))

colours <- c('#a6cee3',  '#e31a1c',  '#b2df8a', '#33a02c',  '#fb9a99',  '#1f78b4',  '#fdbf6f', '#ff7f00') 
#'#cab2d6', '#6a3d9a', '#ffff99', '#b15928', '#8dd3c7', '#ffffb3', '#bebada', '#fb8072', '#80b1d3', '#fdb462', '#b3de69', '#fccde5', '#d9d9d9')
names(colours) <-  (unique(fo_count_1))

unique(gender)
g_colours <- c('#a6cee3',  '#e31a1c',   '#fdbf6f', '#ff7f00', '#8dd3c7') 
names(g_colours) <-  (unique(gender))

unique(group)
gr_colours <- c('#a6cee3',  '#e31a1c',   '#fdbf6f', '#ff7f00', '#8dd3c7') 
names(gr_colours) <-  (unique(group))

#####
#Create Network with Statenet#####
tweets_int <- network (edge_list_u, directed = T, matrix.type = "edgelist")

network::set.vertex.attribute (tweets_int, 'Location', as.character(location))
network::set.vertex.attribute (tweets_int, 'Group', as.character(group))
network::set.vertex.attribute (tweets_int, 'Gender', as.character(gender))
network::set.vertex.attribute (tweets_int, 'Known', as.logical(known))

network::set.vertex.attribute (tweets_int, 'Followers_count', as.numeric(fo_count))
network::set.vertex.attribute (tweets_int, 'Followers_Category', as.character(unlist(fo_count_1)))

network::set.vertex.attribute (tweets_int, 'Language', as.character(language))
network::set.vertex.attribute (tweets_int, 'Acc_Before_Tal3at', as.logical(tal3at_date))
network::set.vertex.attribute (tweets_int, 'Verified', as.logical(verified))
network::set.vertex.attribute (tweets_int, 'Screen_name', as.character(sname))
network:: delete.vertex.attribute(tweets_int, 'na')
network::list.vertex.attributes(tweets_int)

##### Second Variable Destruction station#####
rm(all_data_, df_netnodes, edge_list_u, fo_count_1, fo_count, interactions_, newdf, newdf_,
   usernot, userpost, users, arab, checkus, drops, europe, fo_count, gender, group, i, known,
   language, loc_coun, loc_coun_, namerica, notusers, protected, region, sname, tal3at_date,
   verified, convert_l)
###Tukey's Five Point Summar #####
network.size(tweets_int)
network.density (tweets_int)
summary(tweets_int,print.adj=FALSE)

components(tweets_int, connected = "weak")
components(tweets_int, connected = "strong")
largest_tweetnet <- component.largest(tweets_int, result = "graph")
diameter <- geodist(largest_tweetnet)
max (diameter$gdist)
gtrans(tweets_int)
########
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, node.color = "#000080", node.alpha = 0.75,
        mode = "fruchtermanreingold", label = FALSE,node.size = 1) +
  ggtitle("Basic Sociogram of the 'Tal3at' twitter network") +
  theme(plot.title=element_text(family='', face='bold', colour='black', size=18))


sn_largest <- df_netnodes_$screen_name[df_netnodes_$user_id %in% row.names(largest_tweetnet)]
ggnet2(largest_tweetnet, label = sn_largest, label.size = 3, node.color = "#99d8c9", arrow.size = 3, arrow.gap = 0.03, 
       mode = "fruchtermanreingold", node.size = 8) +
  ggtitle("Largest Component of the Network") +
  theme(plot.title=element_text(family='', face='bold', colour='black', size=18))

indeg <- degree (tweets_int, cmode= 'indegree')
network::set.vertex.attribute (tweets_int, 'Indegree', as.numeric(indeg))

outdeg <- degree (tweets_int, cmode = 'outdegree')
network::set.vertex.attribute (tweets_int, 'Outdegree', as.numeric(outdeg))

total_degree <- degree(tweets_int)
network::set.vertex.attribute (tweets_int, 'Tdegree', as.numeric(total_degree))

max(total_degree)
max(outdeg)
max(indeg)

which.max(total_degree)
df_netnodes_[666,]
which.max(indeg)
df_netnodes_[666,] #tal3at_sept26
which.max(outdeg)
df_netnodes_[678,] #Slow_Leafy

degreedist <- degreedist(tweets_int)

gtrans (tweets_int, measure="weak", use.adjacency = F)
gtrans (tweets_int, measure="weakcensus", use.adjacency = F)

grecip (tweets_int, measure = "edgewise")
mutuality (tweets_int)

#1001 VERIFIED 
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, label = FALSE, 
        node.color = 'Verified', legend.size = 12,
        color.palette = bol_colours, node.size = indeg/5, 
        max_size = 12) + guides (size = F)

#1002 Before or after Tal3at started, TRUE if account created before Tal3at's
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, label = FALSE, 
        node.color = 'Acc_Before_Tal3at',legend.size = 12, node.alpha = 0.5,
        color.palette = bol_colours, node.size = indeg/5,
        max_size = 12) + guides (size = F)


#1003 node size for indegree, color for followers
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, label = tweets_int %v% "Screen_name", 
        node.color = 'Followers_Category', legend.size = 12, label.size = 1,
        color.palette = colours, node.size = indeg/5, 
        max_size = 12) + guides (size = F)

#1004 node size for indegree, color for Gender
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, label = FALSE, 
        node.color = 'Gender', legend.size = 12,
        color.palette = g_colours, node.size = indeg/5, 
        max_size = 12) + guides (size = F)+
  ggtitle("Tal3at Network: indegree centrality measure (+Gender)") +
  theme(plot.title=element_text(family='', face='bold', colour='black', size=18))

#1005 node size for indegree, color for Known
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, label = FALSE, 
        node.color = 'Known', legend.size = 12,
        color.palette = bol_colours, node.size = indeg/5, 
        max_size = 12) + guides (size = F) +
  ggtitle("Tal3at Network: indegree centrality measure (+Identification)") +
  theme(plot.title=element_text(family='', face='bold', colour='black', size=18))

#1006 node size for indegree, color for Language
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, label = FALSE, 
        node.color = 'Language', legend.size = 12,
        color.palette = lang_colours, node.size = indeg/5, 
        max_size = 12) + guides (size = F)

#1007 node size for indegree, color for Group
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, label = FALSE, 
        node.color = 'Group', legend.size = 12,
        color.palette = gr_colours, node.size = indeg/5, 
        max_size = 12) + guides (size = F)  +
  ggtitle("Tal3at Network: indegree centrality measure (+Group)") +
  theme(plot.title=element_text(family='', face='bold', colour='black', size=18))

#1008 node size for indegree, color for Location
ggnet2 (tweets_int, arrow.size = 1, arrow.gap = 0.01, label = FALSE, 
        node.color = 'Location', legend.size = 12,
        color.palette = l_colours, node.size = indeg/5, 
        max_size = 12) + guides (size = F) +
  ggtitle("Tal3at Network: indegree centrality measure (+Location)") +
  theme(plot.title=element_text(family='', face='bold', colour='black', size=18))


#109 node size for indegree, color for location

ggnet2 (tweets_int, arrow.size = 1, label=c("1170751819464814592", "820348466207539200", "107774746",
                                            "876928828530151424", "821515375","596502422","2923535000",
                                            "868371457", "3214542081","250373836"), 
        arrow.gap = 0.01, label.size = 3, label.color = 'black', 
        node.color = 'Location', legend.size = 6, node.alpha = 0.75,
        color.palette = l_colours, node.size = indeg/5, 
        max_size = 12) + guides (size = F)+
  ggtitle("Tal3at Network: indegree centrality measure") +
  theme(plot.title=element_text(family='', face='bold', colour='black', size=18))

ggnet2 (tweets_int, arrow.size = 1, label=c("tal3at_sept26", "Shafax6", "fidaazaanin"), arrow.gap = 0.01, label = FALSE, 
        node.color = 'Group', legend.size = 6, node.alpha = 0.75,
        color.palette = gr_colours, node.size = indeg/5, 
        max_size = 12) + guides (size = F)

##Parital networ####
n1F <- get.inducedSubgraph(tweets_int,
                           which((tweets_int %v% "Followers_count" > 100)))
summary(n1F, print.adj = F)
largest_tweetnet_p <- component.largest(n1F, result = "graph")
diameter <- geodist(largest_tweetnet_p)
max (diameter$gdist)
summary(n1F,print.adj=FALSE)
#1010 node size for indegree, color for location
ggnet2 (palNetwork, arrow.size = 1, arrow.gap = 0.01, label =n1F %v% 'vertex.names',
        label.size = 1,
        node.color = 'Location', legend.size = 6, node.alpha = 0.75,
        color.palette = l_colours, node.size = 'Indegree', 
        max_size = 12) + guides (size = F)


#1010.5 node size for indegree, color for location
ggnet2 (largest_tweetnet_p, arrow.size = 1, arrow.gap = 0.01, label = TRUE,  
        label.size = 2, legend.size = 6, node.alpha = 0.75,node.size = 3,
        max_size = 12)

ids_largest_p <- df_netnodes_[df_netnodes_$user_id %in% row.names(largest_tweetnet_p),]
#####Pal Network ####

palNetwork <- get.inducedSubgraph(tweets_int,
                           which((tweets_int %v% "Group" =='P')))

ggnet2 (palNetwork, arrow.size = 1, arrow.gap = 0.01, label =n1F %v% 'vertex.names',
        label.size = 1,
        node.color = 'Location', legend.size = 6, node.alpha = 0.75,
        color.palette = l_colours, node.size = 'Indegree', 
        max_size = 12) + guides (size = F)

#network:: delete.vertex.attribute(tweets_int, 'isol')

#Centrality measurements####

#1012 betweeness
betw<- round (betweenness (tweets_int, cmode = "directed", rescale = T),2)
max (betw)
which.max (betw)
df_netnodes_[666,] #tal3at_sept26


ggnet2 (tweets_int, label=c("1170751819464814592", "21808604", "138864669",
                            "876928828530151424", "596502422","2565063066","250858058",
                            "49568059", "3214542081","574697732"),
        label.size = 3, node.color = 'Language',  color.palette = lang_colours, 
        node.alpha = 0.5, node.size = 2+(betw*2000),max_size = 15,) + guides (size = F)+
  ggtitle("Tal3at Network: betweenness centrality measure") +
  theme(plot.title=element_text(family='', face='bold', colour='black', size=18))

#1013 Eigenvector centrality
library (igraph)
library (intergraph)

tweet.graph <- asIgraph (tweets_int)
evcenter <-eigen_centrality(tweet.graph , directed = TRUE, scale = TRUE,
                 weights = NULL, options = arpack_defaults)
evcenter <- evcenter$vector
evcenter <- round(evcenter, 2)
max (evcenter)
which.max (evcenter)
df_netnodes_[666,] #tal3at_sept26
ggnet2 (tweets_int, label=c("1170751819464814592", "2493238856", "107774746",
                            "1105210664522575872", "4416151035","2565063066","301030263",
                            "1125028847509020672", "596502422","21808604"),
        label.size = 3, node.color = 'Group',  color.palette = gr_colours, 
        node.alpha = 0.6, node.size = evcenter*100,max_size = 10,mode = "fruchtermanreingold") + guides (size = F)+
  ggtitle("Tal3at Network: eigenvector centrality measure") +
  theme(plot.title=element_text(family='', face='bold', colour='black', size=18))

#Centrality table
tweetnet_cent <- data.frame (
  name = tweets_int %v% "Screen_name",
  group = tweets_int %v% "Group",
  location = tweets_int %v% "Location",
  indegree = indeg,
  outdegree = outdeg,
  Totaldegrees = total_degree,
  betweenness = betw,
  eigenvector = evcenter,
  id = tweets_int %v% "vertex.names"
  )

#clos <- data.frame (clos = clos, id = rownames (largest_tweetnet))
#tweetnet_cent <- merge (tweetnet_cent, clos, by = "id",  all = T) 

tweetnet_cent <- tweetnet_cent [order (-tweetnet_cent$indegree),]
top10<- (head (tweetnet_cent,10))
top10

tweetnet_cent <- tweetnet_cent [order (-tweetnet_cent$eigenvector),]
top10<- (head (tweetnet_cent,10))
top10

tweetnet_cent <- tweetnet_cent [order (-tweetnet_cent$betweenness),]
top10<- (head (tweetnet_cent,10))
top10

tweetnet_cent <- tweetnet_cent [order (-tweetnet_cent$outdegree),]
top10<- (head (tweetnet_cent,10))
top10

tweetnet_cent <- tweetnet_cent [order (-tweetnet_cent$Totaldegrees),]
top10<- (head (tweetnet_cent,10))
top10

#top20_tweeps <- df_netnodes_ [df_netnodes_$user_id %in% top20$id,] 
#write.csv(top20,'top20_b.csv', row.names = FALSE)

cent.deg<- round (centralization (tweets_int, FUN = degree),3)
cent.clos<- round (centralization (tweets_int, FUN = closeness),3)
cent.betw<- round (centralization (tweets_int, FUN = betweenness),3)
cent.eig<- round (centralization (tweets_int, FUN = evcent),3)

print (c (cent.deg, cent.clos, cent.betw, cent.eig))

cut <- cutpoints (tweets_int, mode = "digraph", return.indicator = T)
length (cut [cut==T])

cutpoints_tweet <-data.frame (cutpoint = cut [cut==T],  name = (tweets_int %v% 'Screen_name')[cut==T], 
                              location = (tweets_int %v% 'Location')[cut==T]) 

top10$id %in% cutpoints_tweet$name

set.vertex.attribute (tweets_int, 'cut', as.logical (cut)) 
tweetscut  <- tweets_int %s% which(tweets_int %v% "cut" == F) 

#1014 Without cut points
ggnet2 (tweetscut, label = F, node.color = 'Group',  color.palette = gr_colours,
        node.alpha = 0.7, node.size = "Indegree", max_size = 12) + guides (size = F)

network::set.vertex.attribute (tweets_int, 'vertex.names', as.character(tweets_int %v% 'Screen_name'))
tweetnet_brokerage <- brokerage (tweets_int, tweets_int %v% 'Gender')
str (tweetnet_brokerage)
tweetnet_brokerage$raw.gli
head(tweetnet_brokerage$raw.nli)

brokerage_rawscores <- as.data.frame(tweetnet_brokerage$raw.nli) 
brokerage_rawscores  <- brokerage_rawscores [order (-brokerage_rawscores$t),]
brok10 <- head(brokerage_rawscores, 10)
brok10

brokerage_rawscores  <- brokerage_rawscores [order (-brokerage_rawscores$w_I),]
coord10 <- head(brokerage_rawscores, 10)
coord10

brokerage_rawscores  <- brokerage_rawscores [order (-brokerage_rawscores$w_O),]
consultant10 <- head(brokerage_rawscores, 10)
consultant10

brokerage_rawscores  <- brokerage_rawscores [order (-brokerage_rawscores$b_IO),]
rep10 <- head(brokerage_rawscores, 10)
rep10 

brokerage_rawscores  <- brokerage_rawscores [order (-brokerage_rawscores$b_OI),]
gatekeeper10 <- head(brokerage_rawscores, 10)
gatekeeper10

brokerage_rawscores  <- brokerage_rawscores [order (-brokerage_rawscores$b_O),]
liaison10 <- head(brokerage_rawscores, 10)
liaison10
