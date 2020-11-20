###---
#Preliminary analyses: Are phenotypes in 2 pop under genetic control?
###---

#AIM: Use a quantitative genetics approach to test whether three phenotypic traits generally associated to drought tolerance (shoot length, root length and R:S ratio) 
#     are differentiating individuals within populations independently to treatment.
#     This is done by applying a statistical approach comparing means (t.tests).
#     We are comparing means because we know that several individuals in pop might exhibit different phenotypes.
#     If yes, then trait is canalized and therefore genetically controlled.
#     If no, then trait is under phenotypic plasticity and is not suitable to identify phenotypes (associated to genotype).

# If you want to apply a machine learning approach to ID phenotypes you need to identify individuals of known phenotypes to
# train and validate model. If t.tests are significant (with p-value <= 0.05) then we will take individuals close to mean as "pure".

#Before conducting t.tests, data have to be:
# i) normal and ii) homogeneity of variance across groups (here groups are genotypes/populations)

###~~~
#Load packages
###~~~
library(car)
library(ggpubr)
library(ggplot2)
library(grid)

###~~~
# Load data
###~~~
#See 01_Tidy_data.R for more details on dataset
tri <- read.csv("01_Raw_data/Phenotype_shoot_root_preliminary.csv")

###~~~
# Test for normality (p-value of 0.05)
###~~~
# The null hypothesis for this test is that the data are normally distributed.
# If alpha is 0.05 and the p-value is less than 0.05, 
# then the null hypothesis that the data are normally distributed is rejected.

RSnorm <- shapiro.test(tri$Root2shoot)
Shootnorm <- shapiro.test(tri$Leaves_H)
Rootnorm <- shapiro.test(tri$Root_length)

###~~~
# Test for homogeneity of variance between group (p-value of 0.05)
###~~~
# It tests the null hypothesis that the population variances are equal (same rational as normality test)
RSvarlev <- leveneTest(Root2shoot ~ Genotype, data = tri)
#RSvarflig <- fligner.test(Root2shoot ~ Genotype, data = tri) # could also use this test
Shootvarlev <- leveneTest(Leaves_H ~ Genotype, data = tri)
Rootvarlev <- leveneTest(Root_length ~ Genotype, data = tri)

###~~~
#t.tests
###~~~

## All dataset -- Between populations for each trait
ToTest <-c("Root2shoot", "Leaves_H", "Root_length")

#Prepare matrix
TTestAll <- matrix(ncol = 1, nrow = length(ToTest))
colnames(TTestAll) <- "All_data"
rownames(TTestAll) <- ToTest

#Perform ttests
ttestAll <- lapply(tri[,], function(x) t.test(x ~ tri$Genotype, var.equal = TRUE))
TTestAll[1,1] <- ttestAll$Root2shoot$p.value
TTestAll[2,1] <- ttestAll$Leaves_H$p.value
TTestAll[3,1] <- ttestAll$Root_length$p.value

## Within treatment -- Between populations for each trait
Treat <- sort(unique(tri$Treatment))

TTestTreat <- matrix(ncol = length(Treat), nrow = length(ToTest))
colnames(TTestTreat) <- Treat
rownames(TTestTreat) <- ToTest

for(i in 1:length(Treat)){
  tmp <- subset(tri, tri$Treatment == Treat[i])
  ttestAll <- lapply(tmp[,c("Root2shoot", "Leaves_H", "Root_length")], function(x) t.test(x ~ tmp$Genotype, var.equal = TRUE))
  TTestTreat[1,i] <- ttestAll$Root2shoot$p.value
  TTestTreat[2,i] <- ttestAll$Leaves_H$p.value
  TTestTreat[3,i] <- ttestAll$Root_length$p.value
}

###~~~
# Draw plots
###~~~

#First set at dataset level
RSAll <- ggplot(tri, aes(x = Genotype, y = Root2shoot, fill = Genotype)) +
  scale_fill_manual(values=c("red", "blue")) + # Change colors 
  scale_x_discrete(labels=c(paste("IDT3 (n: ", table(tri$Genotype)[1], ")", sep=''), paste("UTT2 (n: ", table(tri$Genotype)[2], ")", sep=''))) + # Provide pop names
  geom_boxplot(notch = FALSE) +
  labs(x = 'Population', y = 'log(R:S) ratio') +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = "none", panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(x=1.5, y= 1.05, aes(label= "***", fontface = "plain"), cex = 5)
RLAll <- ggplot(tri, aes(x = Genotype, y = Root_length, fill = Genotype)) +
  scale_fill_manual(values=c("red", "blue")) +
  scale_x_discrete(labels=c(paste("IDT3 (n: ", table(tri$Genotype)[1], ")", sep=''), paste("UTT2 (n: ", table(tri$Genotype)[2], ")", sep=''))) + # Provide pop names
  geom_boxplot(notch = FALSE) +
  labs(x = '', y = 'Root length (cm)') +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = "none", panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
#geom_text(x=1.5, y= 40, aes(label= "***", fontface = "plain"))
SLAll <- ggplot(tri, aes(x = Genotype, y = Leaves_H, fill = Genotype)) +
  scale_fill_manual(values=c("red", "blue")) +
  scale_x_discrete(labels=c(paste("IDT3 (n: ", table(tri$Genotype)[1], ")", sep=''), paste("UTT2 (n: ", table(tri$Genotype)[2], ")", sep=''))) + # Provide pop names
  geom_boxplot(notch = FALSE) +
  labs(x = '', y = 'Shoot length (cm)') +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = "none", panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(x=1.5, y= 10, aes(label= "***", fontface = "plain"), cex = 5)

# Second set at treatment level
RStreat <- ggplot(tri, aes(x = Treatment, y = Root2shoot, fill = Genotype)) +
  scale_fill_manual(values=c("red", "blue")) +
  scale_x_discrete(labels=c(paste("T1 (n: ", table(tri$Treatment)[1], ")", sep=''), paste("T2 (n: ", table(tri$Treatment)[2], ")", sep=''), paste("T3 (n: ", table(tri$Treatment)[3], ")", sep=''))) + # Provide pop names
  geom_boxplot(notch = FALSE) +
  labs(x = 'Treatment', y = '') +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = "none", panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(x=1, y= 1.05, aes(label= "***", fontface = "plain"), cex=6) +
  geom_text(x=2, y= 1.05, aes(label= "***", fontface = "plain"), cex=6) +
  geom_text(x=3, y= 1.05, aes(label= "***", fontface = "plain"), cex=6)

SLtreat <- ggplot(tri, aes(x = Treatment, y = Leaves_H, fill = Genotype)) +
  scale_fill_manual(values=c("red", "blue")) +
  scale_x_discrete(labels=c(paste("T1 (n: ", table(tri$Treatment)[1], ")", sep=''), paste("T2 (n: ", table(tri$Treatment)[2], ")", sep=''), paste("T3 (n: ", table(tri$Treatment)[3], ")", sep=''))) + # Provide pop names
  geom_boxplot(notch = FALSE) +
  labs(x = '', y = '') +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = "none", panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(x=1, y= 10, aes(label= "***", fontface = "plain"), cex=6) +
  geom_text(x=2, y= 10, aes(label= "***", fontface = "plain"), cex=6) +
  geom_text(x=3, y= 10, aes(label= "***", fontface = "plain"), cex=6)

RLtreat <- ggplot(tri, aes(x = Treatment, y = Root_length, fill = Genotype)) +
  scale_fill_manual(values=c("red", "blue")) +
  scale_x_discrete(labels=c(paste("T1 (n: ", table(tri$Treatment)[1], ")", sep=''), paste("T2 (n: ", table(tri$Treatment)[2], ")", sep=''), paste("T3 (n: ", table(tri$Treatment)[3], ")", sep=''))) + # Provide pop name
  geom_boxplot(notch = FALSE) +
  labs(x = '', y = '') +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), legend.position = "none", panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
###~~~
# Adjust layout and create figure
###~~~

#Save figure in 03_Figures

pdf("03_Figures/Phenotypes_pop.pdf")
# Move to a new page
grid.newpage()
# Create layout : nrow = 3, ncol = 3
pushViewport(viewport(layout = grid.layout(nrow = 3, ncol = 3)))
# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

# Arrange plots
print(RLAll, vp = define_region(row = 1, col = 1))
print(SLAll, vp = define_region(row = 2, col = 1))
print(RSAll, vp = define_region(row = 3, col = 1))   

print(RLtreat, vp = define_region(row = 1, col = 2:3))
print(SLtreat, vp = define_region(row = 2, col = 2:3))
print(RStreat, vp = define_region(row = 3, col = 2:3))   
dev.off()

###
#Results
###

#These analyses demonstrated that:
# 1. Shoot length and R:S ratio are significantly different independently
#    of treatment. This means that these traits are canalized or under genetic control
#    and represent local adaptations at basis of genotypes.
# 2. Root lengths is not significantly different, which suggests some phenotypic plasticity 
#    or a trait fixed at taxon level.
# 3. Overall, predict that machine learning will better perform with shoot, habitus and root images (in order of importance).
