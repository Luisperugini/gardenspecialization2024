library(bipartite)
library(iNEXT)
library(vegan)
library(ggplot2)
library(rethinking)
library(lubridate)
library(tidyr)
library(reshape2)
library(ape)
library(gridExtra)
library(readxl)
library(V.PhyloMaker2)
library(picante)
library(geiger)
library(R.utils)
library(phytools)
library(raster)
library(geosphere)
library(sp)
library(dplyr)
library(brms)
library(DHARMa)
Data.raw <- read.csv("Supplement+1_+Garden+plant-pollinator+data (4) - Sheet1 (2).csv", stringsAsFactors = T)

Data.raw.a <- Data.raw[Data.raw$Survey.type == "A",]

Data.raw.a$Plant.species <- gsub(pattern = " $", replacement = "", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "[[:space:]]", replacement = "_", 
                                 x = Data.raw.a$Plant.species)
Data.raw.a$Plant.species <- gsub(pattern = "Lavandula.angustifolia", 
                                 replacement = "Lavandula_angustifolia", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Raphanus.raphanistrum.subsp..sativus", 
                                 replacement = "Raphanus_raphanistrum_subsp._sativus", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Aurinia.saxatilis", 
                                 replacement = "Aurinia_saxatilis", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Celosia_argentea.var..cristata", 
                                 replacement = "Celosia_argentea_var._cristata", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Cota.tinctoria", 
                                 replacement = "Cota_tinctoria", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Aquilegia.sp.", 
                                 replacement = "Aquilegia_sp.", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Citrus.×.latifolia", 
                                 replacement = "Citrus_×_latifolia", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Mentha.×.piperita", 
                                 replacement = "Mentha_×_piperita", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Solanum.tuberosum", 
                                 replacement = "Solanum_tuberosum", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Silene.chalcedonica", 
                                 replacement = "Silene_chalcedonica", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Saxifraga.×.arendsii", 
                                 replacement = "Saxifraga_×_arendsii", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Rosmarinus.officinalis", 
                                 replacement = "Rosmarinus_officinalis", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Robinia.pseudoacacia", 
                                 replacement = "Robinia_pseudoacacia", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Raphanus.raphanistrum", 
                                 replacement = "Raphanus_raphanistrum", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Hebe_cf._rakaiensis", 
                                 replacement = "Hebe_sp.", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Cytisus_cf._×_boskoopii", 
                                 replacement = "Cytisus_sp.", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Aeschynomene_cf._sensitiva", 
                                 replacement = "Aeschynomene_sp.", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Thunbergia..alata", 
                                 replacement = "Thunbergia_alata", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Crateagus_sp.", 
                                 replacement = "Crataegus_sp.", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Geranium_x_magnificum", 
                                 replacement = "Geranium_×_magnificum", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Spiraea.nipponica.", 
                                 replacement = "Spiraea_nipponica", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Oxalis.debilis.var..corymbosa", 
                                 replacement = "Oxalis_debilis_var._corymbosa", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Oxalis.corniculata", 
                                 replacement = "Oxalis_corniculata", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Oxalis.corniculata", 
                                 replacement = "Oxalis_corniculata", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Lotus.angustissimus", 
                                 replacement = "Lotus_angustissimus", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Lonicera sp.", 
                                 replacement = "Lonicera_sp.", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Hibiscus.mutabilis", 
                                 replacement = "Hibiscus_mutabilis", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Hibiscus.mutabilis", 
                                 replacement = "Hibiscus_mutabilis", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Dipsacus_fallonum", 
                                 replacement = "Dipsacus_fullonum", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Digitalis_purpureum", 
                                 replacement = "Digitalis_purpurea", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Dasiphora.fruticosa", 
                                 replacement = "Dasiphora_fruticosa", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Convulvulos_tricolor", 
                                 replacement = "Convolvulos_tricolor", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Chrysanthemum.morifolium", 
                                 replacement = "Chrysanthemum_morifolium", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Chaenomeles.lagenaria", 
                                 replacement = "Chaenomeles_lagenaria", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Centauria_montana", 
                                 replacement = "Centaurea_montana", x = Data.raw.a$Plant.species)

Data.raw.a$Plant.species <- gsub(pattern = "Allium.sphaerocephalon", 
                                 replacement = "Allium_sphaerocephalon", x = Data.raw.a$Plant.species)

Data.raw.a$Flower.visitor.species <- gsub(pattern = "Aglais urticae ", 
                                 replacement = "Aglais urticae", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Andrena bicolor?", 
                                 replacement = "Andrena bicolor", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Andrena cineraria ", 
                                 replacement = "Andrena cineraria", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Andrena dorsata or flavipes", 
                                 replacement = "Andrena sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Andrena flavipes ", 
                                 replacement = "Andrena flavipes", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Andrena haemarrhoa", 
                                 replacement = "Andrena haemorrhoa", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Andrena sp. ", 
                                 replacement = "Andrena sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Andrena sp. 2 ", 
                                 replacement = "Andrena sp. 2", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Anthophora plumipes ", 
                                 replacement = "Anthophora plumipes", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Anthopora plumipes", 
                                 replacement = "Anthophora plumipes", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Antophora plumipes", 
                                 replacement = "Anthophora plumipes", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Apis cerana ", 
                                 replacement = "Apis cerana", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Apis mellifera ", 
                                 replacement = "Apis mellifera", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Bibionidae sp. 1 ", 
                                 replacement = "Bibionidae sp. 1", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Bibionidae sp. 3 ", 
                                 replacement = "Bibionidae sp. 3", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Bombus brevivillus ", 
                                 replacement = "Bombus brevivillus", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Bombus hortorum ", 
                                 replacement = "Bombus hortorum", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Bombus lapidarus", 
                                 replacement = "Bombus lapidarius", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Bombus lucuorum", 
                                 replacement = "Bombus lucorum", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Bombus luocorum", 
                                 replacement = "Bombus lucorum", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Bombus muscorum ", 
                                 replacement = "Bombus muscorum", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Bombus pascuorum ", 
                                 replacement = "Bombus pascuorum", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Bombus pascuroum", 
                                 replacement = "Bombus pascuorum", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Bombus pratorum ", 
                                 replacement = "Bombus pratorum", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Bombus sp. ", 
                                 replacement = "Bombus sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Bombus terrestris ", 
                                 replacement = "Bombus terrestris", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Bombus vestalis ", 
                                 replacement = "Bombus vestalis", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Bombyliidae sp. ", 
                                 replacement = "Bombyliidae sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Bombylius sp. ", 
                                 replacement = "Bombylius sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Byasa impediens ", 
                                 replacement = "Byasa impediens", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Camponotus aethiops ", 
                                 replacement = "Camponotus aethiops", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Ceratina curcubitina", 
                                 replacement = "Ceratina cucurbitina", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Cerceris rybyensis ", 
                                 replacement = "Cerceris rybyensis", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Cerceris rybyensis ", 
                                 replacement = "Cerceris rybyensis", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Chalcidoidea sp.?", 
                                 replacement = "Chalcidoidea sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Chrysis ignata group", 
                                 replacement = "Chrysis sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Curculionoidae sp.", 
                                 replacement = "Curculionoidea sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Diptera sp ", 
                                 replacement = "Diptera sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Diptera sp. ", 
                                 replacement = "Diptera sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Eristalis sp. ", 
                                 replacement = "Eristalis sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Eristalis tenax?", 
                                 replacement = "Eristalis tenax", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Eristalis tenax ", 
                                 replacement = "Eristalis tenax", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Halictidae sp. 1 ", 
                                 replacement = "Halictidae sp. 1", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Helophilus pendulus ", 
                                 replacement = "Helophilus pendulus", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Helophilus pendulus ", 
                                 replacement = "Helophilus pendulus", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Heriades truncorum?", 
                                 replacement = "Heriades truncorum", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Hylaeus sp. ", 
                                 replacement = "Hylaeus sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Hyleaus sp. ", 
                                 replacement = "Hylaeus sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Hyleaus sp.", 
                                 replacement = "Hylaeus sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Hymenoptera sp. ", 
                                 replacement = "Hymenoptera sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Hymentoptera sp.", 
                                 replacement = "Hymenoptera sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Ichneumoidae sp.", 
                                 replacement = "Ichneumonidae sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Lasiglossum calceatum", 
                                 replacement = "Lasioglossum calceatum", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Lasioglossum calceatum ", 
                                 replacement = "Lasioglossum calceatum", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Lasioglossum limbellum ", 
                                 replacement = "Lasioglossum limbellum", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Lasioglossum malachurum", 
                                 replacement = "Lasioglossum malacharum", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Lasioglossum morio group", 
                                 replacement = "Lasioglossum sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Lasioglossum sp. ", 
                                 replacement = "Lasioglossum sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Lasioglossum sp. 1 ", 
                                 replacement = "Lasioglossum sp. 1", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Lasioglossum sp.?", 
                                 replacement = "Lasioglossum sp. 1", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Lassioglossum sp.", 
                                 replacement = "Lasioglossum sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Lepidoptera sp. 1 ", 
                                 replacement = "Lepidoptera sp. 1", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Megachile sp. ", 
                                 replacement = "Megachile sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Melangyna cincta ", 
                                 replacement = "Melangyna cincta", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Melanstoma sp.", 
                                 replacement = "Melanostoma sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Meligethes sp.?", 
                                 replacement = "Meligethes sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Osmia bicornis ", 
                                 replacement = "Osmia bicornis", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Osmia leaiana", 
                                 replacement = "Osmia leaiana", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Pararge aegaria", 
                                 replacement = "Pararge aegeria", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Solitary bee", 
                                 replacement = "Solitary bee sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Sphaerophoria sp. ", 
                                 replacement = "Sphaerophoria sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Syrphidae sp. ", 
                                 replacement = "Syrphidae sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Syrphidea sp. 2", 
                                 replacement = "Syrphidae sp. 2", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Syrphidae sp.?", 
                                 replacement = "Syrphidae sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Syrphus sp. ", 
                                 replacement = "Syrphus sp.", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Vespidae sp. 3 ", 
                                 replacement = "Vespidae sp. 3", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Vespidae sp. 4 ", 
                                 replacement = "Vespidae sp. 4", x = Data.raw.a$Flower.visitor.species)
Data.raw.a$Flower.visitor.species <- gsub(pattern = "Vespula sp. ", 
                                 replacement = "Vespula sp.", x = Data.raw.a$Flower.visitor.species)


Data.raw.a$Flower.visitor.species <- gsub(pattern = "\\s", replacement = " ", 
                                          x = Data.raw.a$Flower.visitor.species)

############################## Data ################################
Data <- Data.raw.a [!Data.raw.a$Flower.visitor.species %in% c("0",""),]
Data$Plant.species <- as.factor(Data$Plant.species)
Data$Flower.visitor.species <- as.factor(Data$Flower.visitor.species)
Data$Five.letter.identifier <- droplevels(Data$Five.letter.identifier)


nlevels(Data$Five.letter.identifier)
Networks_old <- vector(mode = "list", length = nlevels(Data$Five.letter.identifier))
for (i in 1:length(Networks_old)){
  Web <- levels(Data$Five.letter.identifier)[i]
  Data.web <- Data[Data$Five.letter.identifier == Web,]
  Networks_old[[i]] <- table(droplevels(Data.web$Plant.species), 
                         droplevels(Data.web$Flower.visitor.species))
}
names(Networks_old) <- levels(Data$Five.letter.identifier)
levels(Data$Plant.species)
levels(Data$Five.letter.identifier)

################################ Variables dataframe #######################
varia<- read.csv("Supplement+2_+Metadata+for+surveyed+gardens - Sheet1.csv")
varia$Riq.An <- unlist(lapply(Networks_old, ncol))
varia$Riq.Pl <- unlist(lapply(Networks_old, nrow))
varia <- varia[varia$X620 %in% names(Networks_old),]
varia.geo <- varia[,c("Longitude","Latitude")]
coordinates(varia.geo) <- c("Longitude","Latitude")
summary(varia)
Climate <- getData(name = "worldclim", var = "bio", res = 5)
Clim.world <- Climate[[c(1,12)]]
Clim <- extract(Clim.world, varia.geo)
varia <- cbind(varia,Clim)

write.csv(varia_clean, "varia_clean.csv")
varia_clean <- varia[varia$Riq.An > 5 & varia$Riq.Pl > 5, ]
Networks <- Networks_old[names(Networks_old) %in% varia_clean$X620]
Data_new <- Data [Data$Five.letter.identifier %in% varia_clean$X620, ]
levels(Data_new$Plant.species)
levels(Data_new$Flower.visitor.species)
levels(Data_new$Five.letter.identifier)
polinizadores <- unique(unlist(lapply(Networks, function(x) colnames(x))))
plantas <- unique(unlist(lapply(Networks, function(x) rownames(x))))
count_interactions <- function(network) {
  sum(network != 0)
}
num_interactions_per_network <- sapply(Networks, count_interactions)

spec.list <- lapply(Networks, FUN = specieslevel, index = "d", level = "lower")
unlist(spec.list)
write.csv(df, "df.csv")
df <- do.call("rbind", spec.list)
df$SP <- substr(rownames(df), start = 7, nchar(rownames(df)))
df$NetID <- substr(rownames(df), start = 1, stop = 5)
df[,4:11] <- varia_clean[match(df$NetID, varia_clean$X620),c("bio1", "bio12","Riq.Pl", "Age.of.property..years.","Type", "Garden.size..m2.", "Elevation..masl.", "Riq.An")]
df$SP <- as.factor(gsub(x = df$SP, pattern = " ", fixed = T, replacement = "_"))
df$d.beta <- df$d
df$d.beta[df$d.beta == 0] <- 0.00001
df$d.beta[df$d.beta == 1] <- 0.99999

df$bio1.z <- scale(df$bio1)
df$bio12.z <- scale(df$bio12)
df$Riq.Pl.z <- scale(df$Riq.Pl)
df$Riq.An.z <- scale(df$Riq.An)
df$Phylo <- df$SP

levels(df$SP)

################################### MAP ####################################
library(ggplot2)
library(maps)
library(ggplot2)
library(raster)
library(sf)
library(ggspatial)
library(viridis)
library(terra)
library(geodata)
library("rnaturalearth")
library("rnaturalearthdata")

temperature <- worldclim_global(var = "bio", res = 10, path = tempdir())
mean_temp <- temperature[[1]]
mean_temp_df <- as.data.frame(mean_temp, xy = TRUE, na.rm = TRUE)
colnames(mean_temp_df) <- c("Longitude", "Latitude", "Mean_Temperature")

world <- ne_countries(scale = "medium", returnclass = "sf")
p<-ggplot(data = world) +
  geom_sf() +
  coord_sf(expand = F, xlim = c(-180, 180), ylim = c(-60, 80))+
  geom_tile(data = mean_temp_df, aes(x = Longitude, y = Latitude, fill = Mean_Temperature), alpha = 0.6) +
  scale_fill_viridis(option = "cividis", name = "Mean Temperature (°C)") +
  geom_point(data = varia_clean, aes(x = Longitude, y = Latitude),
             color = "lightpink", shape = 21, fill = "red", size = 3, stroke = 1.5, alpha = 0.5) +
  labs(x = "Longitude",
       y = "Latitude") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),       
    axis.text = element_text(size = 12),
    legend.position = "bottom")


ggsave("meu_grafico.svg", plot = p, width = 8, height = 5, dpi = 300, units = "in")
ggsave("meu_grafico.png", plot = p, width = 8, height = 5, dpi = 600, units = "in")

############################# Europe ##########################
q <- p + coord_sf(xlim = c(-15, 25), ylim = c(33, 65), expand = F) + 
  guides(fill="none", x = "none", y = "none") + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
ggsave("europa.svg", plot = q, width = 5, height = 5, dpi = 300, units = "in")
ggsave("europa.png", plot = q, width = 5, height = 5, dpi = 600, units = "in")
################################# Phylogeny #################################

Plants_phylo <- Data_new[, c("Plant.species", "Plant.genus", "Plant.Family")]
Plants_phylo <- Plants_phylo[!duplicated(Plants_phylo),]
Plants_phylo$Plant.species <- droplevels(Plants_phylo$Plant.species)
Plants_phylo$Plant.genus <- droplevels(Plants_phylo$Plant.genus)
Plants_phylo$Plant.Family <- droplevels(Plants_phylo$Plant.Family)

Filo.Regional <- phylo.maker(sp.list=Plants_phylo)
Filo.Regional <- Filo.Regional$scenario.3
setdiff(levels(df$SP), Filo.Regional$tip.label)
setdiff(Filo.Regional$tip.label, levels(df$SP))
Filo.vcv <- ape::vcv(Filo.Regional)

######################################### data description ##################

count_interactions <- function(network) {
  interactions_plants <- colSums(network != 0)
  interactions_pollinators <- rowSums(network != 0)
  list(plants = interactions_pollinators, pollinators = interactions_plants)
}

interactions_per_network <- lapply(Networks, count_interactions)
combine_interactions <- function(interactions_list) {
  total_plants <- numeric()
  total_pollinators <- numeric()
  
  for (interactions in interactions_list) {
    total_plants <- c(total_plants, interactions$plants)
    total_pollinators <- c(total_pollinators, interactions$pollinators)
  }
  
  plants_df <- data.frame(
    Especie = names(total_plants),
    Ocorrencias = total_plants
  )
  
  pollinators_df <- data.frame(
    Especie = names(total_pollinators),
    Ocorrencias = total_pollinators
  )
  
  list(plants = plants_df, pollinators = pollinators_df)
}

combined_interactions <- combine_interactions(interactions_per_network)

plants_df <- combined_interactions$plants
pollinators_df <- combined_interactions$pollinators

total_interactions_plants <- plants_df %>%
  group_by(Especie) %>%
  summarise(Ocorrencias = sum(Ocorrencias, na.rm = TRUE))

total_interactions_pollinators <- pollinators_df %>%
  group_by(Especie) %>%
  summarise(Ocorrencias = sum(Ocorrencias, na.rm = TRUE))

sorted_plants <- total_interactions_plants %>%
  arrange(desc(Ocorrencias))

sorted_pollinators <- total_interactions_pollinators %>%
  arrange(desc(Ocorrencias))

print(sorted_plants)
print(sorted_pollinators)

total_interactions_in_network <- function(network) {
  sum(network != 0)
}

total_interactions_per_network <- sapply(Networks, total_interactions_in_network)

total_interactions_df <- data.frame(
  Rede = seq_along(total_interactions_per_network),
  Total_Interacoes = total_interactions_per_network
)

print(total_interactions_df)
total_interactions_all_networks <- sum(total_interactions_per_network)

num_interactions_per_network <- total_interactions_per_network

mean_interactions <- mean(num_interactions_per_network)
sd_interactions <- sd(num_interactions_per_network)

plant_richness_per_network <- sapply(Networks, function(x) nrow(x))
pollinator_richness_per_network <- sapply(Networks, function(x) ncol(x))

mean_plant_richness <- mean(plant_richness_per_network)
sd_plant_richness <- sd(plant_richness_per_network)

mean_pollinator_richness <- mean(pollinator_richness_per_network)
sd_pollinator_richness <- sd(pollinator_richness_per_network)

print(paste("Média do número de interações por rede:", mean_interactions))
print(paste("Desvio padrão do número de interações por rede:", sd_interactions))
print(paste("Média da riqueza de plantas por rede:", mean_plant_richness))
print(paste("Desvio padrão da riqueza de plantas por rede:", sd_plant_richness))
print(paste("Média da riqueza de polinizadores por rede:", mean_pollinator_richness))
print(paste("Desvio padrão da riqueza de polinizadores por rede:", sd_pollinator_richness))

plants_per_network <- list()
animals_per_network <- list()

for (i in 1:length(Networks)) {
  plants_per_network[[i]] <- rownames(Networks[[i]])
  animals_per_network[[i]] <- colnames(Networks[[i]])
}

all_plants <- unlist(plants_per_network)
all_animals <- unlist(animals_per_network)

plants_shared <- table(all_plants)
animals_shared <- table(all_animals)

plants_shared_df <- as.data.frame(plants_shared)
animals_shared_df <- as.data.frame(animals_shared)

colnames(plants_shared_df) <- c("Especie", "Ocorrencias")
colnames(animals_shared_df) <- c("Especie", "Ocorrencias")

shared_plants_df <- plants_shared_df %>%
  filter(Ocorrencias > 1) %>%
  arrange(desc(Ocorrencias))

shared_animals_df <- animals_shared_df %>%
  filter(Ocorrencias > 1) %>%
  arrange(desc(Ocorrencias))

print("Plantas mais comuns:")
print(shared_plants_df)

print("Animais mais comuns:")
print(shared_animals_df)

visitors__plant <- sapply(Networks, function(network) {
  rowSums(network > 0)
})
mean_visitors_per_plant <- mean(unlist(visitors__plant))

plants__visitor <- sapply(Networks, function(network) {
  colSums(network > 0)
})
mean_plants_per_visitor <- mean(unlist(plants__visitor))

print(paste("Média de visitantes por planta:", mean_visitors_per_plant))
print(paste("Média de plantas visitadas por polinizadores:", mean_plants_per_visitor))

quantidade_d_1 <- sum(df$d == 1.0000000)
print(quantidade_d_1)

quantidade_d_0 <- sum(df$d == 0.00000000)
print(quantidade_d_0)

media_d_por_netid <- aggregate(d ~ NetID, data = df, FUN = mean)
print(media_d_por_netid)

media_d_geral <- mean(df$d, na.rm = TRUE)
print(media_d_geral)


varia.d<-valores_por_netid <- aggregate(d ~ NetID, data = df, 
                               FUN = function(x) c(max = max(x, na.rm = TRUE), min = min(x, na.rm = TRUE)))
print(varia.d)

percent_interactions_per_plant <- sapply(Networks, function(network) {
  apply(network > 0, 1, sum) / ncol(network) * 100
})
mean_percent_interactions <- mean(unlist(percent_interactions_per_plant))

print(paste("Em média, as plantas interagiram com", round(mean_percent_interactions, 2), "% da comunidade de polinizadores."))

mean_d_across_networks <- mean(df$d, na.rm = TRUE)
sd(df$d, na.rm = TRUE)
print(paste("O d' médio across todas as redes é:", round(mean_d_across_networks, 2)))

variability_by_network <- aggregate(d ~ NetID, data = df, FUN = sd)
print(variability_by_network)

mean_percent_per_network <- sapply(percent_interactions_per_plant, mean, na.rm = TRUE)


########################## Data Analysis ###########################
# Beta distribution ###############

options(mc.cores = parallel::detectCores())


Beta_Phyl <- brm(d.beta ~ (1|SP) + (1|gr(Phylo, cov = Filo)), family = "beta", data = df,
                 data2 = list(Filo = Filo.vcv),
                 chains = 8, iter = 2250, warmup = 1000)
plot(Beta_Phyl)


Beta_RichEnvSP <- brm(d.beta ~ Riq.Pl.z + Riq.An.z + bio1.z + bio12.z + (1|SP) + (1|NetID), 
                      family = "beta", data = df, chains = 8, iter = 2250, warmup = 1000)

plot(Beta_RichEnvSP)

################################### Plant rich #####################################
library(ggridges)
library(dplyr)
library(tidyr)
library(forcats)
library(viridis)
library(circlize)
library(tidyr)
library(emmeans)
library(ggplot2)
library(car)
library(forcats)
library(dplyr)
library(ggdist)
Blevz <- lm(Riq.Pl ~  bio1 + bio12 + Age.of.property..years. + Type + Garden.size..m2., data = varia_clean)
summary(Blevz)
Plant_drop1<-drop1(Blevz, test = "F")
anova(Blevz)
emmeans_plant <- emmeans(Blevz, pairwise ~ Type)

###################################### Plot plants ##################################

emmeans_df <- as.data.frame(emmeans_plant$emmeans)

Significance <- data.frame(Type = c("rural", "urban", "suburban"),
                           Text = c("a", "ab", "b"))

plot1 <- ggplot(varia_clean, aes(x = Type, y = Riq.Pl)) +
  geom_swarm() +
  geom_point(data = emmeans_df, aes(x = Type, y = emmean), 
             inherit.aes = FALSE, shape = 21, size = 3, 
             fill = "red", color = "red") +                        
  geom_errorbar(data = emmeans_df, aes(x = Type, ymin = emmean - SE, ymax = emmean + SE), inherit.aes = FALSE, width = 0.2, color = "black") +  
  geom_text(data = Significance, y = 100, mapping = aes(label = Text)) +
  labs(x = "Garden Type", y = "Plant Species Richness") +
  theme_classic() +
  theme(legend.position = "none", 
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 11))
ggsave("Fig3Richhness.svg", plot = plot1, width = 4, height = 3)

################## Pollinators rich ##################

Blevz2 <- lm(Riq.An ~ Riq.Pl + bio1 + bio12 + Age.of.property..years. + Type + Garden.size..m2., data = varia_clean)
summary(Blevz2)
pol_drop1<-drop1(Blevz2, test = "F")

Blevz2.PlvsPol <- lm(Riq.An ~ Riq.Pl, data = varia_clean)
summary(Blevz2.PlvsPol)
drop1(Blevz2.PlvsPol, test = "F")


############################ plot pollinators  ##################################

plot.2<-ggplot(varia_clean, aes(x = Riq.Pl, y = Riq.An, color = bio12)) +
  geom_point(alpha = 0.7, size = 3) + 
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "black") + 
  labs(x = "Plant Species Richness", y = "Pollinator Species Richness", color = "Precipitation (mm)") +
  theme_classic() +
  scale_color_gradient(low = "blue", high = "yellow") +
  scale_x_log10() +
  scale_y_log10()
ggsave("Fig4.svg", plot = plot.2, width = 6, height = 4)
ggsave("Fig4.png", plot = plot.2, width = 6, height = 4)
write.xlsx(pol_drop1, "pol_drop1_resultados.xlsx")
write.xlsx(Plant_drop1, "Plant_drop1_resultados.xlsx")
