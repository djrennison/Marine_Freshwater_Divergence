library(dplyr)
library(reshape2)
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggcorrplot")
library(ggcorrplot)
library(RColorBrewer)
library(NatParksPalettes)
library(car)
library(stats)
library(lme4)
library(factoextra)
library(ggbiplot)
library(stringr)

##########
setwd("/Users/dianarennison/desktop")
#dropped bowen lake due to geneflow issue  a <- read.csv("Size_corrected_values_fixed_final_no_bowen.csv")
head(a)
str(a)
unique(a$SiteType)
a$SiteType <- factor(a$SiteType, levels = c("Freshwater", "Marine"))
#subset ecotypes for certain analyses 
a1 <- subset(a,a$SiteType == "Freshwater")
a2 <- subset(a,a$SiteType == "Marine")


library(ggplot2)
library(car)

#plot patternns of variation by ecotype for a few example traits
#Body Depth
p <- ggplot(a, aes(SiteType, Adj_body_depth, color = factor(Site)))
p + geom_boxplot() + theme_classic() 
#test for homogeneity of variance 
z <- leveneTest(a$Adj_pelvic_spine, group = a$SiteType)
z
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
#group   1   0.014 0.9059
#      527        
var(a1$Adj_body_depth)
#0.2604985
var(a2$Adj_body_depth, na.rm=TRUE)          
 #0.2582302         
          
#Pelvic Spine 
p <- ggplot(a, aes(SiteType, Adj_pelvic_spine, color = factor(Site)))
p + geom_boxplot() + theme_classic() 

z <- leveneTest(a$Adj_pelvic_spine, group = a$SiteType)
z

#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value    Pr(>F)    
#group   1  41.032 3.316e-10 ***
#      528    
var(a1$Adj_pelvic_spine)
#1.3301
var(a2$Adj_pelvic_spine, na.rm=TRUE)          
 #0.2582302   


#Gill Rakers
p <- ggplot(a, aes(SiteType, Gill_Raker_Number_Avg, color = factor(Site)))
p + geom_boxplot() + theme_classic() 
z <- leveneTest(a$Gill_Raker_Number_Avg, group = a$SiteType)
z
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value   Pr(>F)   
#group   1  7.6103 0.006005 **
#      527                    
var(a1$Gill_Raker_Number_Avg, na.rm=TRUE)
#10.63532
var(a2$Gill_Raker_Number_Avg, na.rm=TRUE)          
 # 6.916592
  

#Adj_pelvic_girdle
z <- leveneTest(a$Adj_pelvic_girdle, group = a$SiteType)
z
 #      Df F value    Pr(>F)    
#group   1  46.543 2.468e-11 ***
      527 
var(a1$Adj_pelvic_girdle, na.rm=TRUE)
#1.756678
var(a2$Adj_pelvic_girdle, na.rm=TRUE)          
 # 0.6075627
 
 
#Adj_first_dorsal_spine
z <- leveneTest(a$Adj_first_dorsal_spine, group = a$SiteType)
z
#group   1  6.5414 0.01082 *
#      527   
var(a1$Adj_first_dorsal_spine, na.rm=TRUE)
#0.5081301
var(a2$Adj_first_dorsal_spine, na.rm=TRUE)          
 #0.6075627
 
#Adj_second_dorsal_spine
z <- leveneTest(a$Adj_second_dorsal_spine, group = a$SiteType)
z
#group   1  15.712 8.396e-05 ***
#      525  
 
var(a1$Adj_second_dorsal_spine, na.rm=TRUE)
#0.5505655
var(a2$Adj_second_dorsal_spine, na.rm=TRUE)          
 #0.3215446

#Adj_mouth_width
z <- leveneTest(a$Adj_mouth_width, group = a$SiteType)
z
#    Df F value Pr(>F)
#group   1  2.0507 0.1527
var(a1$Adj_mouth_width, na.rm=TRUE)
#0.08723438
var(a2$Adj_mouth_width, na.rm=TRUE)          
 #0.1065632


#Lateral_plate_number_Avg
z <- leveneTest(a$Lateral_plate_number_Avg, group = a$SiteType)
z
 #      Df F value    Pr(>F)    
#group   1  298.62 < 2.2e-16 ***
  #    528    
var(a1$Lateral_plate_number_Avg, na.rm=TRUE)
#1.022791
var(a2$Lateral_plate_number_Avg, na.rm=TRUE)          
 #0.01691669
#Adj_eye_diameter
z <- leveneTest(a$Adj_eye_diameter, group = a$SiteType)
z
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)  
#group   1  3.7831 0.0523 .
#      528    
 #      527   
var(a1$Adj_eye_diameter, na.rm=TRUE)
#0.08269693
var(a2$Adj_eye_diameter, na.rm=TRUE)          
 #0.06150196
 
#Adj_caudal_peduncle_length  
z <- leveneTest(a$Adj_caudal_peduncle_length, group = a$SiteType)
z
#Levene's Test for Homogeneity of Variance (center = median)
#       Df F value    Pr(>F)    
#group   1  13.775 0.0002279 ***
#      528      
  
  #      527   
var(a1$Adj_caudal_peduncle_length, na.rm=TRUE)
#0.01386864
var(a2$Adj_caudal_peduncle_length, na.rm=TRUE)          
 #0.02448047
 
 
#look at marine trait variance 
a2 <- subset(a,a$SiteType == "Marine")
variances <- apply(a2, 2, function(x) var(x, na.rm = TRUE))
 
 #PCA morphological analysis 
 #subset to size corrected data and remove missing data 
 b <- na.omit(a)
 dim(b)
 head(b)
 morph_pca <- prcomp(b[,15:24], center = TRUE, scale = TRUE)
 summary(morph_pca)

#Plot a simple biplot to get a sense of loadings biplot(morph_pca)

#Plot PC1 and pC2, color by population or marine vs. freshpca_data <- as.data.frame(morph_pca$x)
pca_data <- cbind(pca_data,b[,c(1:3)])
head(pca_data)

p <- ggplot(pca_data, aes(x=PC1, y =PC2, color = SiteType)) 
p + geom_point() + labs(x = "PC1", y = "PC2") + theme_classic() + stat_ellipse(geom = "polygon", type = "norm", level = 0.95, alpha = 0.2) + labs(x = "PC1", y = "PC2", color = pca_data $SiteType) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey") + geom_vline(xintercept = 0, linetype = "dashed", color = "grey") + scale_color_manual(values = c("Marine" = "black", "Freshwater" = "dark grey"))

mar_pca <- subset(pca_data, pca_data$SiteType == "Marine")
head(mar_pca)
p <- ggplot(mar_pca, aes(x=PC1, y =PC2, color = Site)) 
p + geom_point() + labs(x = "PC1", y = "PC2") + theme_classic() + stat_ellipse(geom = "polygon", type = "norm", level = 0.95, alpha = 0.2) + labs(x = "PC1", y = "PC2", color = pca_data $SiteType) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey") + geom_vline(xintercept = 0, linetype = "dashed", color = "grey") 

fresh_pca <- subset(pca_data, pca_data$SiteType == "Freshwater")
p <- ggplot(fresh_pca, aes(x=PC1, y =PC2, color = Site)) 
p + geom_point() + labs(x = "PC1", y = "PC2") + theme_classic() + stat_ellipse(geom = "polygon", type = "norm", level = 0.95, alpha = 0.2) + labs(x = "PC1", y = "PC2", color = pca_data $SiteType) + geom_hline(yintercept = 0, linetype = "dashed", color = "grey") + geom_vline(xintercept = 0, linetype = "dashed", color = "grey") 

 #look at marine trait variance by population 
variance_by_group <- tapply(mar_pca$PC1, mar_pca$Site, function(x) var(x, na.rm = TRUE))
var(mar_pca$PC1)
var(fresh_pca$PC1)

#create matrix of absolute difference in PC1 
group1 <- mar_pca[,1]
group2 <- fresh_pca[,1]

#distance function
abs_diff <- function(x,y) {
	abs(x-y)
}

#apply function
abs_diff_matrix <- as.data.frame(outer(group1, group2, abs_diff))
head(abs_diff_matrix)
#reformat 
row.names(abs_diff_matrix) <- mar_pca[,11]
colnames(abs_diff_matrix) <- fresh_pca[,11]
abs_diff_matrix$marine_sample <- rownames(abs_diff_matrix)
rownames(abs_diff_matrix) <- NULL

library(tidyr)
difference_long <- gather(abs_diff_matrix, fresh_sample, Distance, -marine_sample)
head(difference_long)

difference_long$Fresh_Site <- b$Site[match(difference_long$fresh_sample, b$Sample_ID)]
difference_long$Marine_Site <- b$Site[match(difference_long$marine_sample, b$Sample_ID)]
difference_long$pairwise <- paste(difference_long$Marine_Site, difference_long$Fresh_Site, sep = "/") 

average_differences <- aggregate(Distance ~ pairwise, data = difference_long, FUN = mean)
average_differences <- separate(average_differences, pairwise, into = c("Marine","Fresh"), sep = "/")
head(average_differences)

marine_average <- aggregate(Distance ~ Marine, data = average_differences, FUN = mean)

#look at overall pattern 
min(marine_average$Distance)
max(marine_average$Distance)

#plot 
p <- ggplot(average_differences, aes(Marine, Distance, color = factor(Marine)))
p + geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p <- ggplot(average_differences, aes(Fresh, Distance, color = factor(Marine)))
p + geom_point() + theme_classic() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#test whether marine reference matters
library(lmerTest)
marine_ref_effect <- lmer(Distance ~ Marine + (1 | Fresh), data = average_differences)
anova(marine_ref_effect)

##Environmental and Distance analyses 

#combine geodist data with PCA data 
GeoDist <- read.csv("GeoDist_Matrix.csv")
clim <- read.csv("Climate.csv")
combined_pc_data <- average_differences 

#formatting
GeoDist <- GeoDist[,-1]
#Rename columns and rows after the site names -- use site names from Vancouver Island datacolnames(GeoDist) <- rownames(GeoDist) <- clim[['Site']]
head(GeoDist)
#reshape to long
library(reshape2) 
GeoDist <- as.matrix(GeoDist)geo_melt <- melt(GeoDist)[melt(upper.tri(GeoDist))$value,]head(geo_melt)
#Rename a few sites that were named inconsistentlygeo_melt$Var1 <- gsub("Sayward_Estuary", "Sayward_Estuary_1st", geo_melt$Var1)geo_melt$Var2 <- gsub("Sayward_Estuary", "Sayward_Estuary_1st", geo_melt$Var2)geo_melt$Var1 <- gsub("Sooke_River_Campground", "Sooke_River", geo_melt$Var1)geo_melt$Var2 <- gsub("Sooke_River_Campground", "Sooke_River", geo_melt$Var2)
#Add a new column that combines the paired site names into one factorgeo_melt$Comparison <- paste(geo_melt$Var1, geo_melt$Var2, sep = "-")#Duplicate dfgeo_melt2 <- geo_melt
#reassign the sites in Var1 and Var2 as freshwater or marinegeo_melt2$Var1 <- gsub("Bamfield_Inlet", "Marine", geo_melt2$Var1)geo_melt2$Var2 <- gsub("Bamfield_Inlet", "Marine", geo_melt2$Var2)geo_melt2$Var1 <- gsub("Courtenay_River_Estuary", "Marine", geo_melt2$Var1)geo_melt2$Var2 <- gsub("Courtenay_River_Estuary", "Marine", geo_melt2$Var2)geo_melt2$Var1 <- gsub("Englishman_River_Estuary", "Marine", geo_melt2$Var1)geo_melt2$Var2 <- gsub("Englishman_River_Estuary", "Marine", geo_melt2$Var2)geo_melt2$Var1 <- gsub("Port_Hardy_Estuary", "Marine", geo_melt2$Var1)geo_melt2$Var2 <- gsub("Port_Hardy_Estuary", "Marine", geo_melt2$Var2)geo_melt2$Var1 <- gsub("Kanaka_Creek", "Marine", geo_melt2$Var1)geo_melt2$Var2 <- gsub("Kanaka_Creek", "Marine", geo_melt2$Var2)geo_melt2$Var1 <- gsub("Bowen_Marine", "Marine", geo_melt2$Var1)geo_melt2$Var2 <- gsub("Bowen_Marine", "Marine", geo_melt2$Var2)geo_melt2$Var1 <- gsub("Belcara_Marine", "Marine", geo_melt2$Var1)geo_melt2$Var2 <- gsub("Belcara_Marine", "Marine", geo_melt2$Var2)geo_melt2$Var1 <- gsub("Canton_Lagoon", "Marine", geo_melt2$Var1)geo_melt2$Var2 <- gsub("Canton_Lagoon", "Marine", geo_melt2$Var2)geo_melt2$Var1 <- gsub("Sayward_Estuary_1st", "Marine", geo_melt2$Var1)geo_melt2$Var2 <- gsub("Sayward_Estuary_1st", "Marine", geo_melt2$Var2)geo_melt2$Var1 <- gsub("Sooke_River", "Marine", geo_melt2$Var1)geo_melt2$Var2 <- gsub("Sooke_River", "Marine", geo_melt2$Var2)geo_melt2$Var1 <- gsub("Comox_Lake", "Freshwater", geo_melt2$Var1)geo_melt2$Var2 <- gsub("Comox_Lake", "Freshwater", geo_melt2$Var2)geo_melt2$Var1 <- gsub("Bowen_Lake", "Freshwater", geo_melt2$Var1)geo_melt2$Var1 <- gsub("Mohun_Lake", "Freshwater", geo_melt2$Var1)geo_melt2$Var2 <- gsub("Bowen_Lake", "Freshwater", geo_melt2$Var2)geo_melt2$Var2 <- gsub("Mohun_Lake", "Freshwater", geo_melt2$Var2)geo_melt2$Var1 <- gsub("Muchalat_Lake", "Freshwater", geo_melt2$Var1)geo_melt2$Var2 <- gsub("Muchalat_Lake", "Freshwater", geo_melt2$Var2)geo_melt2$Var1 <- gsub("LilMud_Lake", "Freshwater", geo_melt2$Var1)geo_melt2$Var2 <- gsub("LilMud_Lake", "Freshwater", geo_melt2$Var2)geo_melt2$Var1 <- gsub("LowCamp_Lake", "Freshwater", geo_melt2$Var1)geo_melt2$Var2 <- gsub("LowCamp_Lake", "Freshwater", geo_melt2$Var2)geo_melt2$Var1 <- gsub("Mccreight_Lake", "Freshwater", geo_melt2$Var1)geo_melt2$Var2 <- gsub("Mccreight_Lake", "Freshwater", geo_melt2$Var2)
head(geo_melt2)

#Add a new column to label freshwater and marine comparisonsgeo_melt2$Freshwater.vs.Marine <- paste(geo_melt2$Var1, geo_melt2$Var2, sep = "_")#Create consistency across labelsgeo_melt2$Freshwater.vs.Marine <- gsub("Marine_Freshwater", "Freshwater_Marine", geo_melt2$Freshwater.vs.Marine)#Add new columns to include the individual site names for each pairgeo_melt2$Var1Site <- geo_melt$Var1geo_melt2$Var2Site <- geo_melt$Var2

#Subset df to include only freshwater vs. marine comparisons, as these are the only of interest geo_melt_freshmarine <- subset(geo_melt2, geo_melt2$Freshwater.vs.Marine == "Freshwater_Marine")

#make site names consistentgeo_melt_freshmarine[41:70, c(6,7)] <- geo_melt_freshmarine[41:70, c(7,6)]head(geo_melt_freshmarine)

#make site comparisons consistent between geodist data and combined datageo_melt_freshmarine$Comparison <- paste(geo_melt_freshmarine$Var1Site, geo_melt_freshmarine$Var2Site, sep = "-")
combined_pc_data$Comparison <- paste(combined_pc_data$Fresh, combined_pc_data$Marine, sep = "-")

#consolidate geodistances to the combined datacombined_pc_data$Geographical_Distance <- geo_melt_freshmarine$value[match(combined_pc_data$Comparison, geo_melt_freshmarine$Comparison)]

#Climate 
#read in climate dataclim <- read.csv("Climate.csv")
#get correlation matrixcorr <- round(cor(clim[, c(8:10,15,19,22)]), 1)

#visualize correlations#defaultggcorrplot(corr)
#hierarchicalggcorrplot(corr, hc.order = TRUE, outline.col = "white")#one-sided hierarchical with labelsggcorrplot(corr, hc.order = TRUE, type = "upper",     outline.col = "white", lab = TRUE)

#take out highly correlated variables, only look at BIO3, 6, 8, 11, 12-19corr2 <- round(cor(clim[c(9,12,14, 17:25)]), 1)#visualize correlations: hierarchal with labelsggcorrplot(corr2, hc.order = TRUE, type = "upper",     outline.col = "white", lab = TRUE)#Rerun with all climate data#only look at BIO 1, 2, 3, 8, 12, 15corr4 <- round(cor(clim[c(8:10, 15, 19, 22)]), 1)#visualize correlations: hierarchal with labelsggcorrplot(corr4, hc.order = TRUE, type = "upper",     outline.col = "white", lab = TRUE)

## Climate PCA
#Run PCA on BIO 1, 2, 3, 8, 12, 15eco_pca <- prcomp(clim[,c(8,10,15,19,22,9)], center = TRUE, scale = TRUE)summary(eco_pca)
#PC1-PC3 have eigenvalues >1 and explain 90% of the variation in the dataset

#Plot a simple biplot to get a sense of loadings and overly correlated traits biplot(eco_pca)
eco_pca_plot_mf <- fviz_pca_ind(eco_pca, geom.ind = "point", pointshape = 21, pointsize = 5, fill.ind = clim$Type, col.ind = "black", repel = TRUE, legend.title = "Site Type", addEllipses = TRUE) + theme_classic()eco_pca_plot_mf

#Predict PC scores eco_pca_scores <- head(predict(eco_pca))eco_pca_scores

#To find what contributes most to PC1 and PC2eco_pca$rotation

#extract PCA results for the sitesind.coord.eco <- as.data.frame(eco_pca$x)

#Bind columns in your original data frame to the new data frame with PC valueseco_pca_combined <- cbind(clim[,1:5], ind.coord.eco)

#Find minimum values in PC1 and add that positive constant to a new transformed column min(eco_pca_combined$PC1)eco_pca_combined$PC1_Transformed <- eco_pca_combined$PC1 + 3#Find minimum values in PC2 and add that positive constant to a new transformed column min(eco_pca_combined$PC2)eco_pca_combined$PC2_Transformed <- eco_pca_combined$PC2 + 3#Find minimum values in PC3 and add that positive constant to a new transformed column min(eco_pca_combined$PC3)eco_pca_combined$PC3_Transformed <- eco_pca_combined$PC3 + 3

#Create distance matrix of absolute difference between all sites for each PCabs_diff_eco_pc1 <- as.matrix(dist(eco_pca_combined$PC1_Transformed))abs_diff_eco_pc2 <- as.matrix(dist(eco_pca_combined$PC2_Transformed))abs_diff_eco_pc3 <- as.matrix(dist(eco_pca_combined$PC3_Transformed))

#rename columns and rows after the site namescolnames(abs_diff_eco_pc1) <- rownames(abs_diff_eco_pc1) <- eco_pca_combined[['Site']]colnames(abs_diff_eco_pc2) <- rownames(abs_diff_eco_pc2) <- eco_pca_combined[['Site']]colnames(abs_diff_eco_pc3) <- rownames(abs_diff_eco_pc3) <- eco_pca_combined[['Site']]#Reshape the matrix into a long formatabs.diff.melt.eco <- melt(abs_diff_eco_pc1)[melt(upper.tri(abs_diff_eco_pc1))$value,]abs.diff.melt.eco.pc2 <- melt(abs_diff_eco_pc2)[melt(upper.tri(abs_diff_eco_pc2))$value,]abs.diff.melt.eco.pc3 <- melt(abs_diff_eco_pc3)[melt(upper.tri(abs_diff_eco_pc3))$value,]
#consolidate pc valuesnames(abs.diff.melt.eco)[names(abs.diff.melt.eco) == "value"] <- "Eco_PC1"abs.diff.melt.eco$Eco_PC2 <- abs.diff.melt.eco.pc2$valueabs.diff.melt.eco$Eco_PC3 <- abs.diff.melt.eco.pc3$value

#Rename a few sites that were named inconsistentlyabs.diff.melt.eco$Var1 <- gsub("Sayward_Estuary", "Sayward_Estuary_1st", abs.diff.melt.eco$Var1)abs.diff.melt.eco$Var2 <- gsub("Sayward_Estuary", "Sayward_Estuary_1st", abs.diff.melt.eco$Var2)abs.diff.melt.eco$Var1 <- gsub("Sooke_River_Campground", "Sooke_River", abs.diff.melt.eco$Var1)abs.diff.melt.eco$Var2 <- gsub("Sooke_River_Campground", "Sooke_River", abs.diff.melt.eco$Var2)

#Add a new column that combines the paired site names into one factorabs.diff.melt.eco$Comparison <- paste(abs.diff.melt.eco$Var1, abs.diff.melt.eco$Var2, sep = "-")#Duplicate dfabs.diff.melt.eco2 <- abs.diff.melt.eco

#reassign the sites in Var1 and Var2 as freshwater or marineabs.diff.melt.eco2$Var1 <- gsub("Bamfield_Inlet", "Marine", abs.diff.melt.eco2$Var1)abs.diff.melt.eco2$Var2 <- gsub("Bamfield_Inlet", "Marine", abs.diff.melt.eco2$Var2)abs.diff.melt.eco2$Var1 <- gsub("Courtenay_River_Estuary", "Marine", abs.diff.melt.eco2$Var1)abs.diff.melt.eco2$Var2 <- gsub("Courtenay_River_Estuary", "Marine", abs.diff.melt.eco2$Var2)abs.diff.melt.eco2$Var1 <- gsub("Englishman_River_Estuary", "Marine", abs.diff.melt.eco2$Var1)abs.diff.melt.eco2$Var2 <- gsub("Englishman_River_Estuary", "Marine", abs.diff.melt.eco2$Var2)abs.diff.melt.eco2$Var1 <- gsub("Port_Hardy_Estuary", "Marine", abs.diff.melt.eco2$Var1)abs.diff.melt.eco2$Var2 <- gsub("Port_Hardy_Estuary", "Marine", abs.diff.melt.eco2$Var2)abs.diff.melt.eco2$Var1 <- gsub("Kanaka_Creek", "Marine", abs.diff.melt.eco2$Var1)abs.diff.melt.eco2$Var2 <- gsub("Kanaka_Creek", "Marine", abs.diff.melt.eco2$Var2)abs.diff.melt.eco2$Var1 <- gsub("Bowen_Marine", "Marine", abs.diff.melt.eco2$Var1)abs.diff.melt.eco2$Var2 <- gsub("Bowen_Marine", "Marine", abs.diff.melt.eco2$Var2)abs.diff.melt.eco2$Var1 <- gsub("Belcara_Marine", "Marine", abs.diff.melt.eco2$Var1)abs.diff.melt.eco2$Var2 <- gsub("Belcara_Marine", "Marine", abs.diff.melt.eco2$Var2)abs.diff.melt.eco2$Var1 <- gsub("Canton_Lagoon", "Marine", abs.diff.melt.eco2$Var1)abs.diff.melt.eco2$Var2 <- gsub("Canton_Lagoon", "Marine", abs.diff.melt.eco2$Var2)abs.diff.melt.eco2$Var1 <- gsub("Sayward_Estuary_1st", "Marine", abs.diff.melt.eco2$Var1)abs.diff.melt.eco2$Var2 <- gsub("Sayward_Estuary_1st", "Marine", abs.diff.melt.eco2$Var2)abs.diff.melt.eco2$Var1 <- gsub("Sooke_River", "Marine", abs.diff.melt.eco2$Var1)abs.diff.melt.eco2$Var2 <- gsub("Sooke_River", "Marine", abs.diff.melt.eco2$Var2)abs.diff.melt.eco2$Var1 <- gsub("Comox_Lake", "Freshwater", abs.diff.melt.eco2$Var1)abs.diff.melt.eco2$Var2 <- gsub("Comox_Lake", "Freshwater", abs.diff.melt.eco2$Var2)abs.diff.melt.eco2$Var1 <- gsub("Bowen_Lake", "Freshwater", abs.diff.melt.eco2$Var1)abs.diff.melt.eco2$Var2 <- gsub("Bowen_Lake", "Freshwater", abs.diff.melt.eco2$Var2)abs.diff.melt.eco2$Var1 <- gsub("Mohun_Lake", "Freshwater", abs.diff.melt.eco2$Var1)abs.diff.melt.eco2$Var2 <- gsub("Mohun_Lake", "Freshwater", abs.diff.melt.eco2$Var2)abs.diff.melt.eco2$Var1 <- gsub("Muchalat_Lake", "Freshwater", abs.diff.melt.eco2$Var1)abs.diff.melt.eco2$Var2 <- gsub("Muchalat_Lake", "Freshwater", abs.diff.melt.eco2$Var2)abs.diff.melt.eco2$Var1 <- gsub("LilMud_Lake", "Freshwater", abs.diff.melt.eco2$Var1)abs.diff.melt.eco2$Var2 <- gsub("LilMud_Lake", "Freshwater", abs.diff.melt.eco2$Var2)abs.diff.melt.eco2$Var1 <- gsub("LowCamp_Lake", "Freshwater", abs.diff.melt.eco2$Var1)abs.diff.melt.eco2$Var2 <- gsub("LowCamp_Lake", "Freshwater", abs.diff.melt.eco2$Var2)abs.diff.melt.eco2$Var1 <- gsub("Mccreight_Lake", "Freshwater", abs.diff.melt.eco2$Var1)abs.diff.melt.eco2$Var2 <- gsub("Mccreight_Lake", "Freshwater", abs.diff.melt.eco2$Var2)#Add a new column to label freshwater and marine comparisonsabs.diff.melt.eco2$Freshwater.vs.Marine <- paste(abs.diff.melt.eco2$Var1, abs.diff.melt.eco2$Var2, sep = "_")#Create consistency across labelsabs.diff.melt.eco2$Freshwater.vs.Marine <- gsub("Marine_Freshwater", "Freshwater_Marine", abs.diff.melt.eco2$Freshwater.vs.Marine)#Add new columns to include the individual site names for each pair, refer to the duplicated dfabs.diff.melt.eco2$Var1Site <- abs.diff.melt.eco$Var1abs.diff.melt.eco2$Var2Site <- abs.diff.melt.eco$Var2#Subset df to include only freshwater vs. marine comparisonsabs.diff.eco.freshmarine <- subset(abs.diff.melt.eco2, abs.diff.melt.eco2$Freshwater.vs.Marine == "Freshwater_Marine")

#switch all marine-freshwater labels to be freshwater-marine to be consistentabs.diff.eco.freshmarine2 <- abs.diff.eco.freshmarineabs.diff.eco.freshmarine2[41:70, c(1,2)] <- abs.diff.eco.freshmarine2[41:70, c(2,1)]abs.diff.eco.freshmarine2[41:70, c(8,9)] <- abs.diff.eco.freshmarine2[41:70, c(9,8)]

#rename comparisons before consolidatingabs.diff.eco.freshmarine2$Comparison2 <- paste(abs.diff.eco.freshmarine2$Var1Site, abs.diff.eco.freshmarine2$Var2Site, sep = "-")#consolidate eco PC differences onto the combined PC datacombined_pc_data$Eco_PC1_diff <- abs.diff.eco.freshmarine2$Eco_PC1[match(combined_pc_data$Comparison, abs.diff.eco.freshmarine2$Comparison2)]

combined_pc_data$Eco_PC2_diff <- abs.diff.eco.freshmarine2$Eco_PC2[match(combined_pc_data$Comparison, abs.diff.eco.freshmarine2$Comparison2)]combined_pc_data$Eco_PC3_diff <- abs.diff.eco.freshmarine2$Eco_PC3[match(combined_pc_data$Comparison, abs.diff.eco.freshmarine2$Comparison2)]
head(combined_pc_data)
#export data#write.csv(combined_pc_data, "Combined_distances_final.csv")

# Linear Models ## GLMM### Setup

#Check distribution of variable of interest combined_pc_data$Distance + 1qqp(combined_pc_data$Distance, "norm")#create freshwater site numberscombined_pc_data$Freshwater_Site_Num <- combined_pc_data$Freshcombined_pc_data$Freshwater_Site_Num <- gsub("Comox_Lake", "1", combined_pc_data$Freshwater_Site_Num)combined_pc_data$Freshwater_Site_Num <- gsub("Mohun_Lake", "2", combined_pc_data$Freshwater_Site_Num)combined_pc_data$Freshwater_Site_Num <- gsub("Muchalat_Lake", "3", combined_pc_data$Freshwater_Site_Num)combined_pc_data$Freshwater_Site_Num <- gsub("LilMud_Lake", "4", combined_pc_data$Freshwater_Site_Num)combined_pc_data$Freshwater_Site_Num <- gsub("LowCamp_Lake", "5", combined_pc_data$Freshwater_Site_Num)combined_pc_data$Freshwater_Site_Num <- gsub("Mccreight_Lake", "6", combined_pc_data$Freshwater_Site_Num)combined_pc_data$Freshwater_Site_Num <- as.numeric(combined_pc_data$Freshwater_Site_Num)

### GLMM with geography #Pc1 all traits sig with geodist

#Run mixed effects model and account for 1 random effect#All traitsout <- lmer(Distance ~ Geographical_Distance + (1 | Freshwater_Site_Num), data = combined_pc_data, REML = FALSE)summary(out) Anova(out, type = "II")

### GLMM with ecology#PC1 all traits sig with eco 2#PC2 all traits sig with eco 1 and eco 2

#run mixed effects model with eco pca data#All traitseco <- lmer(Distance ~ Eco_PC1_diff + Eco_PC2_diff + Eco_PC3_diff + (1 | Freshwater_Site_Num), data = combined_pc_data, REML = FALSE)summary(eco) Anova(eco, type = "II")#Geodist with eco differenceeco_geodist <- lmer(Geographical_Distance ~ Eco_PC1_diff  + Eco_PC2_diff + Eco_PC3_diff + (1 | Freshwater_Site_Num), data = combined_pc_data)summary(eco_geodist) Anova(eco_geodist, type = "II")

### Plot with Geography 
combined_pc_data$Geographical_Distance_km <- combined_pc_data$Geographical_Distance/1000
head(combined_pc_data)

#PC1 with geodistgraph_pc1_overall_geodist <- ggplot(combined_pc_data, aes(x = Geographical_Distance_km, y = Distance)) + theme_classic(25) +   geom_point(size = 4, aes(color = Fresh))  +  labs(x = "Geographic Distance (km)", y = "PC1 Difference") +   stat_smooth(method = "lm",    aes(color = Fresh),    se = FALSE,    size = 0.8)graph_pc1_overall_geodist


### Plots with Ecology
#PC1 with Eco2graph_pc_overall_eco2 <- ggplot(combined_pc_data, aes(x = Eco_PC2_diff, y = Distance)) + theme_classic(25) +   geom_point(size = 4, aes(color = Fresh)) +    labs(x = "Ecological PC2 Difference", y = "PC1 Difference") +   stat_smooth(method = "lm",    aes(color = Fresh),    se = FALSE,    size = 0.8)graph_pc_overall_eco2 



##########
#pairwise ecological differences for marine populations from above
#subset out just marine to marine comparisons
marine_marine <- subset(abs.diff.melt.eco2, abs.diff.melt.eco2$Freshwater.vs.Marine == "Marine_Marine")
head(marine_marine)

#subset out pairwise morphological differences for marine to marine populations from pca analysis above 
mar_pca <- subset(pca_data, pca_data$SiteType == "Marine")
head(mar_pca)
#estimate morphological group means for each site  
mean_by_group <- aggregate(PC1 ~ Site, data = mar_pca, FUN = mean)

#estimate marine-marine distance 
marine_morpho_distance <- as.matrix(dist(mean_by_group[,2], method = "manhattan"))
head(marine_morpho_distance)

#reformat 
row.names(marine_morpho_distance) <- mean_by_group[,1]
colnames(marine_morpho_distance) <- mean_by_group[,1]
marine_morpho_distance2 <- melt(marine_morpho_distance)[melt(upper.tri(marine_morpho_distance))$value,]
marine_morpho_distance2$comparison1 <-  paste(marine_morpho_distance2$Var1, marine_morpho_distance2$Var2, sep = "-") 
marine_morpho_distance2$comparison2 <-  paste(marine_morpho_distance2$Var2, marine_morpho_distance2$Var1, sep = "-") 
  head(marine_morpho_distance2)
  
 #merge in environmental and geographic distance data 
marine_morpho_distance2$Eco_PC1_1 <- marine_marine$Eco_PC1[match(marine_morpho_distance2$comparison1, marine_marine$Comparison)]
marine_morpho_distance2$Eco_PC1_2 <- marine_marine$Eco_PC1[match(marine_morpho_distance2$comparison2, marine_marine$Comparison)]
marine_morpho_distance2$Eco_PC2_1 <- marine_marine$Eco_PC2[match(marine_morpho_distance2$comparison1, marine_marine$Comparison)]
marine_morpho_distance2$Eco_PC2_2 <- marine_marine$Eco_PC2[match(marine_morpho_distance2$comparison2, marine_marine$Comparison)]
marine_morpho_distance2$geo1 <- geo_melt2$value[match(marine_morpho_distance2$comparison1, geo_melt2$Comparison)]
marine_morpho_distance2$geo2 <- geo_melt2$value[match(marine_morpho_distance2$comparison2, geo_melt2$Comparison)]

head(marine_morpho_distance2)

library(dplyr)

marine_morpho_distance2$merged_Eco_PC1 <- coalesce(marine_morpho_distance2$Eco_PC1_1,marine_morpho_distance2$Eco_PC1_2)

marine_morpho_distance2$merged_Eco_PC2 <- coalesce(marine_morpho_distance2$Eco_PC2_1,marine_morpho_distance2$Eco_PC2_2)

marine_morpho_distance2$geo <- coalesce(marine_morpho_distance2$geo1,marine_morpho_distance2$geo2)
marine_morpho_distance2$geo_km <- marine_morpho_distance2$geo/1000

p <- ggplot(marine_morpho_distance2, aes(x= value, y = merged_Eco_PC2)) 
p + geom_point() + labs(x = "morpho", y = "eco") + theme_classic()  + geom_smooth(method = lm) 


#--------------------------
#test for relationship between geographic distance and ecologcial distance PC1
y <- lm(log(marine_morpho_distance2$merged_Eco_PC1) ~ log((marine_morpho_distance2$geo_km)))
summary(y)
plot( log(marine_morpho_distance2$geo_km), log(marine_morpho_distance2$merged_Eco_PC1))
abline(y)
#Call:
#lm(formula = log(marine_morpho_distance2$merged_Eco_PC1) ~ log((marine_morpho_distance2$geo_km)))
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-3.9995 -0.4958  0.0178  0.7470  1.9261 
#
#Coefficients:
#                                      Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                            -6.0019     1.4702  -4.082 0.000190 ***
#log((marine_morpho_distance2$geo_km))   1.2243     0.2887   4.241 0.000116 ***

#Residual standard error: 1.158 on 43 degrees of freedom
#Multiple R-squared:  0.2949,	Adjusted R-squared:  0.2785 
#F-statistic: 17.99 on 1 and 43 DF,  p-value: 0.0001159


#significant correlation between eco PC1 and distance 



#-----------------------------
#test for relationship between geographic distance and ecologcial distance PC2
y <- lm(log(marine_morpho_distance2$merged_Eco_PC2) ~ log((marine_morpho_distance2$geo_km)))
summary(y)
plot( log(marine_morpho_distance2$geo_km), log(marine_morpho_distance2$merged_Eco_PC2))
abline(y)
#Call:
#lm(formula = log(marine_morpho_distance2$merged_Eco_PC2) ~ log((marine_morpho_distance2$geo_km)))
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-3.3593 -0.7672  0.2705  0.7189  1.1329 
#
#Coefficients:
#                                      Estimate Std. Error t value Pr(>|t|)
#(Intercept)                            -1.6445     1.2115  -1.357    0.182
#log((marine_morpho_distance2$geo_km))   0.3014     0.2379   1.267    0.212
#
#Residual standard error: 0.9546 on 43 degrees of freedom
#Multiple R-squared:  0.03599,	Adjusted R-squared:  0.01358 
#F-statistic: 1.606 on 1 and 43 DF,  p-value: 0.2119

#non-significant positive for eco PC2 and distance 


#-----------------------------
#test for relationship between morphological distance and geographic distance 
y <- lm(log(marine_morpho_distance2$value) ~ log((marine_morpho_distance2$geo_km)))
summary(y)
plot( log(marine_morpho_distance2$geo_km), log(marine_morpho_distance2$value))
abline(y)

#Call:
#lm(formula = log(marine_morpho_distance2$value) ~ log((marine_morpho_distance2$geo_km)))
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-4.8905 -0.2232  0.5261  0.8191  1.5259 
#
#Coefficients:
#                                      Estimate Std. Error t value Pr(>|t|)
#(Intercept)                           -0.33118    1.61642  -0.205    0.839
#log((marine_morpho_distance2$geo_km)) -0.02272    0.31739  -0.072    0.943
#
#Residual standard error: 1.274 on 43 degrees of freedom
#Multiple R-squared:  0.0001191,	Adjusted R-squared:  -0.02313 
#F-statistic: 0.005122 on 1 and 43 DF,  p-value: 0.9433

#no relationship 



#-----------------------------
#test for relationship between morphological distance and ecological distance PC1

y <- lm(log(marine_morpho_distance2$value) ~ log((marine_morpho_distance2$merged_Eco_PC1)))
summary(y)
plot( log(marine_morpho_distance2$merged_Eco_PC1), log(marine_morpho_distance2$value))
abline(y)
#Call:
#lm(formula = log(marine_morpho_distance2$value) ~ log((marine_morpho_distance2$merged_Eco_PC1)))
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-4.9250 -0.2628  0.5009  0.7937  1.5176 
#
#Coefficients:
#                                              Estimate Std. Error t value Pr(>|t|)  
#(Intercept)                                    -0.4497     0.1917  -2.346   0.0237 *
#log((marine_morpho_distance2$merged_Eco_PC1))   0.0192     0.1408   0.136   0.8921  
#---

#Residual standard error: 1.273 on 43 degrees of freedom
#Multiple R-squared:  0.0004327,	Adjusted R-squared:  -0.02281 
#F-statistic: 0.01861 on 1 and 43 DF,  p-value: 0.8921

#no relationship


#-----------------------------
#test for relationship between morphological distance and ecological distance PC2

#y <- lm(log(marine_morpho_distance2$value) ~ log((marine_morpho_distance2$merged_Eco_PC2)))
#summary(y)
#plot( log(marine_morpho_distance2$merged_Eco_PC2), log(marine_morpho_distance2$value))
#abline(y)
#
#Call:
#lm(formula = log(marine_morpho_distance2$value) ~ log((marine_morpho_distance2$merged_Eco_PC2)))
#
#Residuals:
#    Min      1Q  Median      3Q     Max 
#-4.7976 -0.2754  0.3839  0.8420  1.4283 
#
#Coefficients:
#                                              Estimate Std. Error t value Pr(>|t|)  
#(Intercept)                                    -0.4315     0.1906  -2.264   0.0287 *
#log((marine_morpho_distance2$merged_Eco_PC2))   0.1217     0.1989   0.612   0.5440  

#Residual standard error: 1.268 on 43 degrees of freedom
#Multiple R-squared:  0.008626,	Adjusted R-squared:  -0.01443 
#F-statistic: 0.3741 on 1 and 43 DF,  p-value: 0.544

#no relationship




####################################################################
####################################################################
#Parallelism Analysis

#theta function
f.theta <- function(table.of.diff.in.means, unique.id, select.col){
  angle.matrix.radians <- matrix(nrow = length(unique.id), ncol = length(unique.id))
  for(i in 1:length(unique.id)){
    for(j in 1:length(unique.id)){
      angle.matrix.radians[i,j] <- round(acos(cor(x = t(table.of.diff.in.means[i,select.col]), t(table.of.diff.in.means[j,select.col]), use = "na.or.complete")), 3) #
    }
  }

  rownames(angle.matrix.radians) <- unique.id
  colnames(angle.matrix.radians) <- unique.id
  angle.matrix.degrees <- round(angle.matrix.radians*(180/pi), 3)
  angle.output <- list(angle.matrix.radians, angle.matrix.degrees)
  names(angle.output) <- c("theta.radians", "theta.degrees")
  return(angle.output)
}

#meanL function
f.meanL <- function(table.of.diff.in.means, unique.id, select.col){
  get.vectlength <- function(vect){
    return(sqrt(sum(vect^2, na.rm = TRUE)))
  }
  table.of.diff.in.means.t <- table.of.diff.in.means[,select.col]
  length.of.vector.by.group <- apply(table.of.diff.in.means.t, 1 ,get.vectlength) #1 signifies by row. apply the function to every row in the input dataframe
  length.diff.matrix <- matrix(nrow = length(unique.id), ncol = length(unique.id))
  for(i in 1:length(unique.id)){
    for(j in 1:length(unique.id)){
      length.diff.matrix[i,j] <-mean(round(c(length.of.vector.by.group[i], length.of.vector.by.group[j]), 3))
    }
  }
  rownames(length.diff.matrix) <- unique.id
  colnames(length.diff.matrix) <- unique.id

  length.of.vector.by.group <- as.data.frame(length.of.vector.by.group)
  length.of.vector.by.group$wshd <- unique.id
  length.output <- list(length.diff.matrix, length.of.vector.by.group)
  return(length.output)
}


####
#create matrix of absolute difference for each PC  

mar_pca_temp <- mar_pca[,-c(11,13)]
fresh_pca_temp <- fresh_pca[,-c(11,13)]


df_means_mar <- mar_pca_temp %>% 
group_by(Site) %>% 
summarise(across(starts_with("PC"), mean, na.rm=TRUE))

df_means_mar <- as.data.frame(df_means_mar)
head(df_means_mar)

df_means_fresh <- fresh_pca_temp %>% 
group_by(Site) %>% 
summarise(across(starts_with("PC"), mean, na.rm=TRUE))

df_means_fresh <- as.data.frame(df_means_fresh)
head(df_means_fresh)

group1_1 <- df_means_mar[,c(2)]
group1_2 <- df_means_mar[,c(3)]
group1_3 <- df_means_mar[,c(4)]
group1_4 <- df_means_mar[,c(5)]
group1_5 <- df_means_mar[,c(6)]
group1_6 <- df_means_mar[,c(7)]
group1_7 <- df_means_mar[,c(8)]
group1_8 <- df_means_mar[,c(9)]
group1_9 <- df_means_mar[,c(10)]
group1_10 <- df_means_mar[,c(11)]


group2_1 <- df_means_fresh[,c(2)]
group2_2 <- df_means_fresh[,c(3)]
group2_3 <- df_means_fresh[,c(4)]
group2_4 <- df_means_fresh[,c(5)]
group2_5 <- df_means_fresh[,c(6)]
group2_6 <- df_means_fresh[,c(7)]
group2_7 <- df_means_fresh[,c(8)]
group2_8 <- df_means_fresh[,c(9)]
group2_9 <- df_means_fresh[,c(10)]
group2_10 <- df_means_fresh[,c(11)]


#distance function
diff <- function(x,y) {
	x-y
}

#apply function
diff_matrix1 <- as.data.frame(outer(group1_1, group2_1, diff))
diff_matrix1$pca <- "PCA1"
diff_matrix2 <- as.data.frame(outer(group1_2, group2_2, diff))
diff_matrix2$pca <- "PCA2"
diff_matrix3 <- as.data.frame(outer(group1_3, group2_3, diff))
diff_matrix3$pca <- "PCA3"
diff_matrix4 <- as.data.frame(outer(group1_4, group2_4, diff))
diff_matrix4$pca <- "PCA4"
diff_matrix5 <- as.data.frame(outer(group1_5, group2_5, diff))
diff_matrix5$pca <- "PCA5"
diff_matrix6 <- as.data.frame(outer(group1_6, group2_6, diff))
diff_matrix6$pca <- "PCA6"
diff_matrix7 <- as.data.frame(outer(group1_7, group2_7, diff))
diff_matrix7$pca <- "PCA7"
diff_matrix8 <- as.data.frame(outer(group1_8, group2_8, diff))
diff_matrix8$pca <- "PCA8"
diff_matrix9 <- as.data.frame(outer(group1_9, group2_9, diff))
diff_matrix9$pca <- "PCA9"
diff_matrix10 <- as.data.frame(outer(group1_10, group2_10, diff))
diff_matrix10$pca <- "PCA10"

#reformat 
row.names(diff_matrix1) <- df_means_mar[,1]
row.names(diff_matrix2) <- df_means_mar[,1]
row.names(diff_matrix3) <- df_means_mar[,1]
row.names(diff_matrix4) <- df_means_mar[,1]
row.names(diff_matrix5) <- df_means_mar[,1]
row.names(diff_matrix6) <- df_means_mar[,1]
row.names(diff_matrix7) <- df_means_mar[,1]
row.names(diff_matrix8) <- df_means_mar[,1]
row.names(diff_matrix9) <- df_means_mar[,1]
row.names(diff_matrix10) <- df_means_mar[,1]


all_pca_diff <- rbind(diff_matrix1, diff_matrix2, diff_matrix3, diff_matrix4, diff_matrix5, diff_matrix6, diff_matrix7, diff_matrix8, diff_matrix9, diff_matrix10)
colnames(all_pca_diff) <- df_means_fresh[,1]
head(all_pca_diff)

all_pca_diff$marine_sample <- rownames(all_pca_diff)
rownames(all_pca_diff) <- NULL
names(all_pca_diff)[7] <- "pca"
all_pca_diff$combined <- paste(all_pca_diff$pca, all_pca_diff$marine_sample, sep = "/") 
head(all_pca_diff)
#write.csv(all_pca_diff,"differences.csv")


library(tidyr)
all_pca_diff <- read.csv("differences.csv")
all_pca_diff <- all_pca_diff[,-c(7,8)]
head(all_pca_diff)
difference_long <- gather(all_pca_diff, fresh_sample, difference, -combined)
difference_long <- separate(difference_long, combined, into = c("pca","Marine"), sep = "/")
difference_long$pairwise <- paste(difference_long$Marine, difference_long$fresh_sample, sep = "/") 
difference_long <- difference_long[,-c(2:3)]

head(difference_long)

difference_wide <- dcast(difference_long, difference + pairwise ~ pca)

df_wide <- difference_long %>% spread(pca, difference)
head(df_wide)



#extract unique ids
unique.id <- c(unique(df_wide$pairwise))
#drop ID to only have differences in means
 table.of.diff.in.means <- df_wide[,-1]
 #identify columns
 select.col <- c(1:length(colnames(table.of.diff.in.means)))
 #estimate theta
  x.theta <- f.theta(table.of.diff.in.means, unique.id, select.col)
  
  print(x.theta$theta.degrees)  angles <- x.theta$theta.degreesangles2 <- anglesangles2[lower.tri(angles2)] <- NA  library(reshape2)  angles_l <- melt(angles2, na.rm=TRUE) #melt the data  head(angles_l)
  df_filtered <- angles_l %>% filter(Var1 != Var2)
    head(df_filtered)
df_filtered <- separate(df_filtered, Var1, into = c("Marine1","Fresh1"), sep = "/")
df_filtered <- separate(df_filtered, Var2, into = c("Marine2","Fresh2"), sep = "/")
  df_filtered2 <- df_filtered %>% filter(Marine1 == Marine2)
  df_filtered2$fresh_combo <- paste(df_filtered2$Fresh1, df_filtered2$Fresh2, sep = "/") 
    head(df_filtered2)

p <- ggplot(df_filtered2, aes(Marine1, value, color = factor(Marine1)))
p + geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

  #test whether marine reference matters
library(lmerTest)
marine_ref_effect_theta <- lmer(value ~ Marine1 + (1 | fresh_combo), data = df_filtered2)
anova(marine_ref_effect_theta)

#Type III Analysis of Variance Table with Satterthwaite's method
#        Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)    
#Marine1 5081.9  564.65     9   126  43.028 < 2.2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
  
  
  ##look at parallelism by marine reference 
  mean_parallel <- tapply(df_filtered2$value, df_filtered2 $Marine1, function(x) mean(x, na.rm = TRUE))
  min_parallel <- tapply(df_filtered2$value, df_filtered2 $Marine1, function(x) min(x, na.rm = TRUE))
  max_parallel <- tapply(df_filtered2$value, df_filtered2 $Marine1, function(x) max(x, na.rm = TRUE))

  
  #estimate length
    m_length <- f.meanL(table.of.diff.in.means, unique.id, select.col)
mean_length <- as.data.frame(m_length[2])
mean_length <- separate(mean_length, wshd, into = c("Marine","Fresh"), sep = "/")
head(mean_length)
p <- ggplot(mean_length, aes(Marine, length.of.vector.by.group, color = factor(Marine)))
p + geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
p + geom_violin() + geom_boxplot() + theme_classic() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

  #test whether marine reference matters
marine_ref_effect_length <- lmer(length.of.vector.by.group ~ Marine + (1 | Fresh), data = mean_length)
anova(marine_ref_effect_length)

#Type III Analysis of Variance Table with Satterthwaite's method
#       Sum Sq Mean Sq NumDF DenDF F value    Pr(>F)    
#Marine 15.774  1.7527     9    45  15.479 4.253e-11 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



#merge distance 
#theta
df_filtered2$combined1 <- paste(df_filtered2$Fresh1, df_filtered2$Marine1, sep = "-") 
df_filtered2$combined2 <- paste(df_filtered2$Fresh2, df_filtered2$Marine2, sep = "-") 
head(df_filtered2)
df_filtered2$geo_distance1 <- combined_pc_data$Geographical_Distance[match(df_filtered2$combined1, combined_pc_data $Comparison)]
df_filtered2$geo_distance2 <- combined_pc_data$Geographical_Distance[match(df_filtered2$combined2, combined_pc_data $Comparison)]
df_filtered2$mean_distance <- rowMeans(df_filtered2[, c("geo_distance1","geo_distance2")])

#length
mean_length$combined1 <- paste(mean_length$Fresh, mean_length$Marine, sep = "-") 
head(mean_length)
mean_length$geo_distance1 <- combined_pc_data$Geographical_Distance[match(mean_length$combined1, combined_pc_data $Comparison)]



#test for relationship between mean geographic distance and theta
p <- ggplot(df_filtered2, aes(mean_distance, value, color = factor(Marine1)))
p + geom_point() + theme_classic() + geom_smooth(method = lm)
#test for 
out <- lmer(value ~ log(mean_distance) + (1 | Marine1), data = df_filtered2, REML = FALSE)
summary(out) 

#Linear mixed model fit by maximum likelihood . t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: value ~ log(mean_distance) + (1 | Marine1)
#   Data: df_filtered2#
#
#     AIC      BIC   logLik deviance df.resid 
#  1215.7   1227.7   -603.8   1207.7      146 

#Scaled residuals: 
#     Min       1Q   Median       3Q      Max 
#-1.82707 -0.74910 -0.07135  0.57271  2.92554 

#Random effects:
# Groups   Name        Variance Std.Dev.
# Marine1  (Intercept)  14.69    3.833  
# Residual             173.92   13.188  
#Number of obs: 150, groups:  Marine1, 10
#
#Fixed effects:
#                   Estimate Std. Error      df t value Pr(>|t|)   
#(Intercept)         101.215     31.904  15.354   3.172  0.00616 **
#log(mean_distance)   -5.378      2.698  15.373  -1.993  0.06428 . 





#test for relationship between mean geographic distance and length
y <- lm(mean_length$length.of.vector.by.group ~ log((mean_length$geo_distance1)))
y <- lm(mean_length$length.of.vector.by.group ~ mean_length$geo_distance1)
summary(y)

plot(log(mean_length$geo_distance1), mean_length$length.of.vector.by.group)
plot( mean_length$geo_distance1, mean_length$length.of.vector.by.group)

abline(y)


p <- ggplot(mean_length, aes(log(geo_distance1), length.of.vector.by.group, color = factor(Marine)))
p + geom_point() + theme_classic() + geom_smooth(method = lm)
#test for 

out <- lmer(length.of.vector.by.group ~ log(geo_distance1) + (1 | Marine), data = mean_length, REML = FALSE)
summary(out) 











