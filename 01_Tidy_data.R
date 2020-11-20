###---
# Tidy data
###---

# In this study we only focus on diploid (2x) A.t.tridentata

###~~~
#Get data from large-scale GxE experiment
###~~~
GE <- read.csv("~/Google Drive/*Buerki_Lab/Greenhouse/HF_GxE_experiment/HF_GxE_experiment_full_data_Feb162020.csv")

###~~~
#DB1: Data on populations, parental lines and cytotypes
###~~~
DB1 <- read.csv("~/Google Drive/*Buerki_Lab/Greenhouse/F1_Raw_data/DB1.csv")

###~~~
#Scored phenotypes: Shoot length, root length
###~~~
OUT <- read.csv("~/Google Drive/*Buerki_Lab/Greenhouse/HF_GxE_experiment/Root_2_Shoot/Harvested_seedlings_images_GxE - Harvested_seedlings_images_GxE.csv")

#Add attributes: Subsp, Family, genotype, treatment
OUT$Parent <- as.vector(GE$Parent[match(as.vector(OUT$SeedlingID), GE$SeedlingID)])
OUT$Subsp <- as.vector(GE$Subsp[match(as.vector(OUT$SeedlingID), GE$SeedlingID)])
OUT$Genotype <- as.vector(GE$Genotype[match(as.vector(OUT$SeedlingID), GE$SeedlingID)])
OUT$Treat <- as.vector(GE$Treatment[match(as.vector(OUT$SeedlingID), GE$SeedlingID)])
OUT$Treat[which(OUT$Date == "3/16/2020" & OUT$Treat == "T1")] <- "T3"

###~~~
# Convert Treatment columns: T1 = control (well watered), T2 = drought, T3 = re-watered (drought recovery)
###~~~
OUT$Treatment <- rep("NA", nrow(OUT))
OUT$Treatment[which(OUT$Treat == "T3")] <- "T3"
OUT$Treatment[which(OUT$Treat == "T2")] <- "T1"
OUT$Treatment[which(OUT$Treat == "T1")] <- "T2"


###~~~
#Infer log(Root:shoot variable,10)
###~~~
OUT$Root2shoot <- log(OUT$Root_length/OUT$Leaves_H,10)

###~~~
#Subset to only include tridentata (2x)
###~~~
#This is the dataset that we will use for the phenotyping project
tri <- subset(OUT, OUT$Subsp == unique(OUT$Subsp)[1])

###~~~
#Export data
###~~~
#Save raw data in folder 01_Raw_data
write.csv(tri, file = "01_Raw_data/Phenotype_shoot_root_preliminary.csv", quote=F, row.names = F)
