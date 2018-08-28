# Kholofelo Sethebe
# 3137355
# 27 August 2018
# Quantitative Ecology Exam
# BCB743

library(tidyverse)
library(ggpubr)
library(vegan)
library(ggplot2)
library(gclus)
library(cluster)


# Question 1 [60 marks] ---------------------------------------------------

#The question concerns the file ‘dataset_1.xls’, which includes the abundances
#of 3O fish species at each sampling location, together with the bottom depth,
#the temperature and the geographic coordinates (latitude and longitude)
  
        
# Question 1A -------------------------------------------------------------

# Specify the dimensions of the two data sets 
#(i.e. excluding the geographic coordinates).

dim(env2_csv)
dim(spe2_csv)


# Question 1B -------------------------------------------------------------

# Recreate the map (Figure 1), making sure to include as much of the detail 
#displayed there as possible. Instead of showing colour coding, 
#scale the size of the sample locations by the......


# i) species richness -----------------------------------------------------

#(i.e. alpha-diversity, which is simply the number of species per location)

rowSums(spe_rich_csv > 0)
spe_rich <- rowSums(spe_rich_csv > 0)

plot1 <- ggplot(data = spa_csv, aes(x = Longitude, y = Latitude))+
  geom_point(aes(cex = spe_rich), colour = "red", fill = "red")+
  labs(title = "species richness")+
  annotate("text", label = "Norway", x = 20, y = 70.5, size = 3.5)+
  annotate("text", label = "Russia", x = 40, y = 69, size = 3.5)+
  annotate("text", label = "Svalbard", x = 20, y = 76, size = 3.5)+
  annotate("text", label = "Barents Sea", x = 36, y =75 , size = 3.5)+
  theme_bw() + 
  theme(legend.title = element_blank())
  

# ii) Shannon–Weaver index ------------------------------------------------

spe_csv[,-32]
speSH <- spe_csv[,-32]
SW <- diversity(speSH,index = "shannon")

plot2 <- ggplot(data = spa_csv, aes(x = Longitude, y = Latitude))+
  geom_point(aes(cex = SW), colour = "Purple", fill = "purple")+
  labs(title = "shannon-weaver")+
  annotate("text", label = "Norway", x = 20, y = 70.5, size = 3.5)+
  annotate("text", label = "Russia", x = 40, y = 69, size = 3.5)+
  annotate("text", label = "Svalbard", x = 20, y = 76, size = 3.5)+
  annotate("text", label = "Barents Sea", x = 36, y =75 , size = 3.5)+
  theme_bw() + 
  theme(legend.title = element_blank())



# iii) Simpson's index. ---------------------------------------------------

S <- diversity(speSH,index = "simpson")

plot3 <- ggplot(data = spa_csv, aes(x = Longitude, y = Latitude))+
  geom_point(aes(cex = SW), colour = "brown", fill = "brown")+
  labs(title = "Simpso's index")+
  annotate("text", label = "Norway", x = 20, y = 70.5, size = 3.5)+
  annotate("text", label = "Russia", x = 40, y = 69, size = 3.5)+
  annotate("text", label = "Svalbard", x = 20, y = 76, size = 3.5)+
  annotate("text", label = "Barents Sea", x = 36, y =75 , size = 3.5)+
  theme_bw() + 
  theme(legend.title = element_blank())

combinedplot1 <- ggarrange(plot1, plot2, plot3, ncol = 2, nrow = 2)
combinedplot1



# Question 1C -------------------------------------------------------------

#Make a new map as per (b), and this time, scale the symbols by

# i) Temperature ----------------------------------------------------------

plot4 <- ggplot(data = spa_csv, aes(x = Longitude, y = Latitude))+
  geom_point(aes(cex = env2_csv$Temperature), colour = "brown", fill = "brown")+
  labs(title = "Temperature")+
  annotate("text", label = "Norway", x = 20, y = 70.5, size = 3.5)+
  annotate("text", label = "Russia", x = 40, y = 69, size = 3.5)+
  annotate("text", label = "Svalbard", x = 20, y = 76, size = 3.5)+
  annotate("text", label = "Barents Sea", x = 36, y =75 , size = 3.5)+
  theme_bw() + 
  theme(legend.title = element_blank())


# ii) Depth. --------------------------------------------------------------

plot5 <- ggplot(data = spa_csv, aes(x = Longitude, y = Latitude))+
  geom_point(aes(cex = env2_csv$Depth), colour = "red", fill = "red")+
  labs(title = "Depth")+
  annotate("text", label = "Norway", x = 20, y = 70.5, size = 3.5)+
  annotate("text", label = "Russia", x = 40, y = 69, size = 3.5)+
  annotate("text", label = "Svalbard", x = 20, y = 76, size = 3.5)+
  annotate("text", label = "Barents Sea", x = 36, y =75 , size = 3.5)+
  theme_bw() + 
  theme(legend.title = element_blank())

ggarrange(plot4, plot5, ncol = 2)


# Question 1D -------------------------------------------------------------

#Based on a visual comparison between (b) and (c), 
#are there already some patterns evident? If so, what are they?

#The diagrams are showing evident patterns.   
#species richness increases towards Barents sea where the temperature and 
#depth is relativelt is low
#The abandance and diversity of species is higer all around the ares.



# Question 1E -------------------------------------------------------------

#Provide a i) table and ii) figure(s) of the descriptive statistics of 
#the environmental variables. 

# i) Table ----------------------------------------------------------------

summary(env2_csv)


# ii) Figures -------------------------------------------------------------


# The temperature vars ----------------------------------------------------

plt1 <- env2_csv %>%
  select(Temperature) %>%
  gather(key = var1, value = measurement) %>%
  ggplot(aes(x = var1, y = measurement)) +
  geom_boxplot() +
  labs( y = "Temperature",
       title = "Temperature") +
  theme_bw()


# The depth vars ----------------------------------------------------------

plt2 <-env2_csv %>%
  select(Depth) %>%
  gather(key = var2, value = measurement) %>%
  ggplot(aes(x = var2, y = measurement)) +
  geom_boxplot() +
  labs( y = "Depth",
        title = "Depth") +
  theme_bw()

ggarrange(plt1, plt2, ncol = 2)


# Explanation -------------------------------------------------------------

#What do the descriptive statistics tell us about the environment?

#The descriptive statistics tell us the temperature decreases with
#an increase in deapth


# Question 1F -------------------------------------------------------------

#f. This question requires you to perform a Correspondence Analysis (CA) 
#on the species table. You will analyse the (a) raw data, (b) log-transformed 
#data, and (c) presence-absence data. We will focus on four species 
#(Boreogadus saida, Triglops murrayi, Notolepis rissoi krøyeri, 
#and Trisopterus esmarkii) that vary strongly along CA axes 1 and 2.

#i) Show the code that produces the three CAs (i.e. on raw, presence-absence,
#and logtransformed data). 


# Raw Data ----------------------------------------------------------------
(spe.ca <- cca(spe_csv))
summary(spe.ca)	# default scaling 2
summary(spe.ca, scaling=1)

# Present-Absent Data -----------------------------------------------------

spe.pa <- decostand(spe_rich_csv,method = "pa")[, -31]
vegdist(spe.pa,method = "jacc",binary = T)

# Log Transformed Data ----------------------------------------------------

#spe.log <- log10(spe)
(sp.log.ca <- cca(spe.log))
summary(sp.log.ca)		

#ii. Focusing now on the output of the analysis on the raw data, 
#provide numerical support that the four species named above are 
#strongly influenced along CA1 and CA2 (i.e. provide the associated numerical 
#support next to each species name). In terms of biplots, 
#how would this visually manifest? 

(spe.ca <- cca(spe_csv))
summary(spe.ca)		# default scaling 2
summary(spe.ca, scaling=1)

                              #CA1      CA2

#Boreogadus saida            -4.95826 -0.11224
#Triglops murrayi            -3.32479  0.01091
#Notolepis rissoi krøyeri    0.72187  2.44504
#Trisopterus esmarkii        0.75067 -4.65093


# CA biplots --------------------------------------------------------------

dev.new(title="CA biplots", width=14, height=7)
par(mfrow=c(1,2))

# Scaling 1: sites are centroids of species

plot(spe.ca, scaling=1, main="CA fish abundances - biplot scaling 1")
Biplot1 <- plot(spe.ca, scaling=1, main="CA fish abundances - biplot scaling 1")

# Scaling 2 (default): species are centroids of sites

plot(spe.ca, main="CA fish abundances - biplot scaling 2")

Biplot2 <- plot(spe.ca, main="CA fish abundances - biplot scaling 2")


# In terms of biplots,how would this visually manifest? -------------------

#Species will only be available closer to the sites, 
#and sites with high abundance of will have bigger circles.


#iii). For each analysis (a-c), how much variance is associated 
#with CA1, CA2, and CA3? What is the cumulative variance explained
#by these three axes (show the calculations, and explain where to find 
#this information from the standard output)? Do you consider these 
#ordinations to be very good at capturing the variation that exist 
#across space in the community composition?
 

# Raw Data ----------------------------------------------------------------

(CA1 <- spe.ca$CA$eig[1] / sum(spe.ca$CA$eig))
(CA2 <- spe.ca$CA$eig[2] / sum(spe.ca$CA$eig))
(CA3 <- spe.ca$CA$eig[3] / sum(spe.ca$CA$eig))

# Cumulative variance 
((CA1 + CA2 + CA3) * 100)

# presence absence data ---------------------------------------------------



# log transformed data # Come back to  ------------------------------------



# Explaining --------------------------------------------------------------

  
  
#iv) Now, using vegan’s ‘ordisurf()’ function (see example code in your 
#handouts), create panelled plots (i.e. four figures arranged in two rows
#and two columns per panel) for each ordination (a-c). In each of the four
#sub panels, focus separately on the four key species (i.e.one species per 
#sub-panel), and superimpose the environmental vectors (temperature, depth,
#latitude, and longitude) on one of the sub-panels in each of the groups
#of plots. What are the major patterns that come out? Which are the most
#influential environmental drivers for each of the species? What is the
#effect of transforming the data on the analysis and the interpretation
#of the outcomes? 


# A surface plot for the CA raw species data ------------------------------

require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe_csv, tmp <- ordisurf(spe.ca ~ Bo_sa, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Boreogadus saida"))
abline(h = 0, v = 0, lty = 3)
with(spe_csv, tmp <- ordisurf(spe.ca ~ Tr_spp, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Triglops murrayi"))
abline(h = 0, v = 0, lty = 3)
with(spe_csv, tmp <- ordisurf(spe.ca ~ No_rk, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Notolepis rissoi krøyeri"))
abline(h = 0, v = 0, lty = 3)
with(spe_csv, tmp <- ordisurf(spe.ca ~ Tr_es, bubble = 3,
                           family = quasipoisson, knots = 2, col = 6,
                           display = "sites", main = "Trisopterus esmarkii"))
abline(h = 0, v = 0, lty = 3)

# A posteriori projection of environmental variables in a CA
# The last plot produced (CA scaling 2) must be active

 
(spe.ca.env <- envfit(spe.ca, env2_csv, scaling = 2)) # Scaling 2 is default
plot(spe.ca.env)

# Plot significant variables with a different colour

plot(spe.ca.env, p.max = 0.05, col = "red")

#Boreogadus saida and Triglops murrayi look very similar, which can mean that 
#they have similar species abundance.
#Temperature and Depth are the influencial environmental driver for 
#Trisopterus esmarkii



# Question 2 (110 marks) --------------------------------------------------


# Loading the required data -----------------------------------------------

data("BCI")
data("BCI.env")

# Converting the species data to Presence-absence -------------------------

BCI.pa <- decostand(BCI,method = "pa")
vegdist(BCI.pa,method = "jacc",binary = T)


# Question 2A -------------------------------------------------------------

# Provide a full descriptive analysis of the environmental data 
#and provide tables and/or figures as necessary. [1O]

summary(BCI.env)


# Environmental Heterogeneity ---------------------------------------------

plt1 <- BCI.env %>%
  select(EnvHet) %>%
  gather(key = var, value = measurement) %>%
  ggplot(aes(x = var, y = measurement)) +
  geom_boxplot() +
  labs(x = "environmental heterogeneity variable",
       y = "Simpson diversity",
       title = "Environmental Heterogeneity") +
  theme_bw()
plt1


# Question 1B -------------------------------------------------------------

#Create graphs that show the i) species richness, 
#ii) Shannon–Weaver index, and the iii) Simpson's index
#across the landscape. Explain the patterns that are visible.

BCI.pa %>% 
  select()
summary(BCI.pa)

#Gathering spa data
BCI.spa <- BCI.env %>% 
  select(UTM.EW, UTM.NS)



#i) species richness --------------------------------------------------------

rowSums(BCI.pa > 0)
spe_rich <- rowSums(BCI.pa > 0)

ggplot(BCI.spa, asp = 1, aes(x = UTM.EW, y = UTM.NS))+
  geom_point(aes(cex = spe_rich), colour = "red", fill = "red")+
  labs(x = "UTM.NS", y = "UTM.EW")+
  ggtitle("Species Richness")+
  theme(plot.title = element_text(hjust = 0.5))


# ii) Shannon-Weaver ------------------------------------------------------

SW <- diversity(BCI.pa,index = "shannon")

ggplot(BCI.spa, asp = 1, aes(x = UTM.EW, y = UTM.NS))+
  geom_point(aes(cex = SW), colour = "blue", fill = "blue")+
  labs(x = "UTM.NS", y = "UTM.EW")+
  ggtitle("Shannon-Weaver")+
  theme(plot.title = element_text(hjust = 0.5))


# iii) The Simpson's Index ------------------------------------------------

S <- diversity(BCI.spa,index = "simpson")

ggplot(BCI.spa, asp = 1, aes(x = UTM.EW, y = UTM.NS))+
  geom_point(aes(cex = S), colour = "purple", fill = "purple")+
  labs(x = "UTM.NS", y = "UTM.EW")+
  ggtitle("Simpson's Index")+
  theme(plot.title = element_text(hjust = 0.5))


# Explain the patterns that are visible. ----------------------------------

#species richness does not hold  constant pattern, it flactuates throughout
#The diversity of species seem to decrease with and increase in UTM.NS
#The abundance of species seem to increase with increasing UTM.NS


# Question 2C -------------------------------------------------------------

#Provide a detailed written account of the output of the PCA as 
#per the ‘summary()’ function. [15]

BCI.pa.pca <- rda(BCI.pa, binary = TRUE)
summary(BCI.pa.pca)


# Explain -----------------------------------------------------------------

#Inertia is either the sum of the variances of the variables (
#PCA on a covariance matrix) or, as in this case (PCA on a correlation matrix),
#the sum of the diagonal values of the correlation matrix, i.e. the sum of 
#all correlations of the variables with themselves, which corresponds 
#to the number of variables (49 in this example).

#Eigenvalues: these are measures of the importance (variance) of
#the axes. They can be expressed as Proportions Explained, or proportions of
#variation accounted for, by dividing them by the total inertia.the first PC 
#axis has the highest eigenvalue (3.791 in this example) and has the 
#greatest explanatory power.

#“Scaling” refers to the way ordination results are projected in
#the reduced space for graphical display. There is no single way to optimally
#display objects and variables together in a PCA biplot, i.e. a plot showing 
#two types of results, here the sites and the variables. Two main types 
#of scaling are generally used. Each of them has properties that must
#be kept in mind for proper interpretation of the biplots.

#Scaling 1 = distance biplot: the eigenvectors are scaled to unit length.
#(1) Distances among objects in the biplot are approximations of 
#their Euclidean distances in multidimensional space.
#(2) The angles among descriptor vectors are meaningless.

#Scaling 2 = correlation biplot: each eigenvector is scaled to 
#the square root of its eigenvalue. (1) Distances among objects in 
#the biplot are not approximations of their Euclidean distances 
#in multidimensional space. (2) The angles between descriptors in the
#biplot reflect their correlations.

#Species scores: coordinates of the arrow heads of the variables. 
#For historical reasons, response variables are always called “species” 
#in vegan, no matter what they represent.

#Site scores: coordinates of the sites in the ordination diagram. Objects 
#are always called “Sites” in vegan output files.



# Question 2D -------------------------------------------------------------

#Provide a detailed written account of the output of the CA 
#as per the ‘summary()’ function. 

BCI.CA <- cca(BCI)

BCI.CA
summary(BCI.CA) #default scaling 2
summary(BCI.CA, scaling = 1)


# Explain -----------------------------------------------------------------

#Inertia is either the sum of the variances of the variables (
#PCA on a covariance matrix) or, as in this case (PCA on a correlation matrix),
#the sum of the diagonal values of the correlation matrix, i.e. the sum of 
#all correlations of the variables with themselves, which corresponds 
#to the number of variables (50 in this example).

#Scaling 1 - the distances among samples (sites) in the reduced ordination 
#space approximate chi-square distances among samples in the full-dimensional
#space; any object found near the point representing a species is likely
#to contain a high contribution of that species.

#Scaling 2 - the distances among species in the reduced ordination space 
#approximate chi-square distances among species in the full-dimensional
#space; any species that lies close to the point representing an object
#is more likely to be found in that object or to have higher frequency there.

#Species score is represented by the original algorithm is based on 
#reciprocal averaging of column and row scores, which starts from
#random values, and by interative row- and column-averaging converge 
#into a unique solution

#Site scores (weighted sums of species scores): coordinates of the sites 
#as expressed in the space of the response variables Y.


# Question 2E -------------------------------------------------------------

#Using the functionality of the ‘ordisurf()’ package, demonstrate the major 
#difference between the analysis conducted using a PCA and a CA. 
#Hint, you will have to use the argument ‘family = "gaussian"’ 
#inside of ‘ordisurf()’. In your graphic display of the differences, 
#please use as demonstration the two species that are most heavily 
#loaded along CA9 / PCA9 and the two species that are most heavily 
#loaded along CA> / PCA>. Which analysis is the better one, 
#and why do you say so?


# A surface plot for the CA -----------------------------------------------


require('viridis')

plt3 <- palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(BCI.pa, tmp <- ordisurf(BCI.pa.pca ~ Myrcia.gatunensis, bubble = 3,
                          family = gaussian, knots = 2, col = 6,
                          display = "sites", main = "Myrcia.gatunensis"))
plt4 <- palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(BCI.pa, tmp <- ordisurf(BCI.CA ~ Myrcia.gatunensis, bubble = 3,
                             family = gaussian, knots = 2, col = 6,
                             display = "sites", main = "Myrcia.gatunensis"))
plt5 <- palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(BCI.pa, tmp <- ordisurf(BCI.pa.pca ~ Miconia.elata, bubble = 3,
                             family = gaussian, knots = 2, col = 6,
                             display = "sites", main = "Miconia.elata"))
plt6 <- palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(BCI.pa, tmp <- ordisurf(BCI.CA ~ Miconia.elata, bubble = 3,
                             family = gaussian, knots = 2, col = 6,
                             display = "sites", main = "Miconia.elata"))

plt7 <- palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(BCI.pa, tmp <- ordisurf(BCI.pa.pca ~ Macrocnemum.roseum, bubble = 3,
                             family = gaussian, knots = 2, col = 6,
                             display = "sites", main = "Macrocnemum.roseum"))
plt8 <- palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(BCI.pa, tmp <- ordisurf(BCI.CA ~ Macrocnemum.roseum, bubble = 3,
                             family = gaussian, knots = 2, col = 6,
                             display = "sites", main = "Macrocnemum.roseum"))
plt9 <- palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(BCI.pa, tmp <- ordisurf(BCI.pa.pca ~ Maclura.tinctoria, bubble = 3,
                             family = gaussian, knots = 2, col = 6,
                             display = "sites", main = "Maclura.tinctoria"))
plt10 <- palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(BCI.pa, tmp <- ordisurf(BCI.CA ~ Maclura.tinctoria, bubble = 3,
                             family = gaussian, knots = 2, col = 6,
                             display = "sites", main = "Maclura.tinctoria"))

abline(h = 0, v = 0, lty = 3)


# Question 2F -------------------------------------------------------------

#Analyse these datasets using a non-Metric Multidimensional Scaling (nMDS), 
#produce the necessary plots (of both axis 1 vs. axis 2 and axis 3 vs. 
#axis E), and interpret. Is any new insight possible from the 
#nMDS that was not possible before?


# Question 2G -------------------------------------------------------------

#Within the context of the published literature available on the studies 
#based on these data, how do your analyses add to that which is
#i. already known, and
#ii. what aspects of the published analysis are not captured by your own 
#analysis? 



# QUESTION 3 --------------------------------------------------------------


# Question 3A -------------------------------------------------------------
#Do a full RDA on the ‘dataset_1.xls’ data, explain the output 
#comprehensively, and provide any supporting figures you may deem necessary. 
#What is your interpretation of the analysis? What new information 
#can be obtained from the RDA that could not be found using the earlier 
#analysis? 


# Hellinger-transform the species dataset ---------------------------------

spe.hel <- decostand(spe_csv, "hellinger")
(spe.rda <- rda(spe.hel ~ ., env2_csv))

# Scaling 2 (default) -----------------------------------------------------

summary(spe.rda)	

#  To calculate sum of inertia of CCA and eigenvalues K -------------------

sum(spe.rda$CCA$eig) 


#  Canonical coefficients from the rda object -----------------------------

coef(spe.rda)


# Unadjusted R^2 retrieved from the rda object ----------------------------

(R2 <- RsquareAdj(spe.rda)$r.squared)


# Adjusted R^2 retrieved from the rda object ------------------------------

(R2adj <- RsquareAdj(spe.rda)$adj.r.squared)


# Global test of the RDA result -------------------------------------------

anova(spe.rda, permutations=how(nperm=999)) 


# Tests of all canonical axes ---------------------------------------------

# p value for each location
anova(spe.rda, by="term", permutations=how(nperm=999)) 

# p value for each axis
anova(spe.rda, by="axis", permutations=how(nperm=999)) 

# Variance inflation factors (VIF) ----------------------------------------

vif.cca(spe.rda)


# Scaling 1: distance triplot ---------------------------------------------

dev.new(title="RDA scaling 1 + wa") 
par(mfrow = c(2, 2))
plot(spe.rda, scaling=1, 
     main="Triplot RDA spe.hel ~ env2 - scaling 1 - wa scores")
spe.sc1 <- scores(spe_prac.rda, choices=1:2, scaling=1, display="sp")
arrows(0, 0, spe.sc1[, 1]*0.92, spe.sc1[, 2]*0.92, length=0, lty=1, col="red")


# Scaling 2 (default): correlation triplot --------------------------------

dev.new(title="RDA scaling 2 + wa")
plot(spe.rda, main="Triplot RDA spe.hel ~ env2 - scaling 2 - wa scores")
spe.sc2 <- scores(spe_prac.rda, choices=1:2, display="sp")
arrows(0, 0, spe.sc2[, 1]*0.92, spe.sc2[, 2]*0.92, length=0, lty=1, col="red")



# Triplots of the rda results (lc scores) ---------------------------------

## Site scores as linear combinations of the environmental variables
# Scaling 1

dev.new(title="RDA scaling 1 + lc")
plot(spe.rda, scaling=1, display=c("sp", "lc", "cn"), 
     main="Triplot RDA spe.hel ~ env2 - scaling 1 - lc scores")
arrows(0, 0, spe.sc1[, 1]*0.92, spe.sc1[, 2]*0.92, length=0, lty=1, col="red")

# Scaling 2

dev.new(title="RDA scaling 2 + lc")
plot(spe.rda, display=c("sp", "lc", "cn"), 
     main="Triplot RDA spe.hel ~ env2 - scaling 2 - lc scores")
arrows(0, 0, spe.sc2[,1]*0.92, spe.sc2[,2]*0.92, length=0, lty=1, col="red")


# Interpretation ----------------------------------------------------------

#The constrained fraction (0.1419):fraction is the amount of 
#variance of the Y matrix explained by the explanatory variables. 

#unconstrained fraction:(0.2157)  

#The proportion constarined is 0.3967

#Eigenvalues and their contribution to the variance: this analysis yielded 
#2 canonical axes (with eigenvalues labelled RDA1 to RDA2) 
#and 30 additional, unconstrained axes for the residuals 
#(with eigenvalues labelled PC1 to PC16).


#Accumulated constrained eigenvalues: these are cumulative amounts of 
#variance expressed as proportions of the total explained variance, as 
#opposed to their contribution to the total variance described above.

#Species scores are the coordinates of the tips of the vectors representing 
#the response variables in the bi- or triplots. As in PCA, they depend on 
#the scaling chosen.

#Site scores (weighted sums of species scores): coordinates of the sites 
#as expressed in the space of the response variables Y.

#Site constraints (linear combinations of constraining variables): 
#coordinates of the sites in the space of the explanatory variables X. 
#These are the fitted site scores.

#Biplot scores for constraining variables: coordinates of the tips of the
#vectors representing the explanatory variables. These coordinates are 
#obtained as follows: correlations are computed between the explanatory 
#variables and the fitted site scores, and then these correlations are
#transformed to produce the biplot scores. 

#The triplots show that temperature and depth do play an important role 
# in the ordination of a few sites,

# Question 3B -------------------------------------------------------------

#Do a full RDA on the Barro Colorado Island Tree Counts datasets, 
#xplain the output comprehensively,and provide any supporting figures 
#you may deem necessary. What is your interpretation of the analysis? 
#What new information can be obtained from the RDA that could not be 
#found using the earlier


# Hellinger-transform the species dataset ---------------------------------

BCI.pa.hel <- decostand(BCI.pa, "hellinger")
(BCI.pa.rda <- rda(BCI.pa.hel ~ ., BCI.env))


# Scaling 2 (default) -----------------------------------------------------

summary(BCI.pa.rda)	

# To calculate sum of inertia of CCA and eigenvalues  ---------------------

sum(BCI.pa.rda$CCA$eig) 

# Canonical coefficients from the rda object ------------------------------

coef(BCI.pa.rda)

# Unadjusted R^2 retrieved from the rda object ----------------------------

(R2 <- RsquareAdj(BCI.pa.rda)$r.squared)

# Adjusted R^2 retrieved from the rda object ------------------------------

(R2adj <- RsquareAdj(BCI.pa.rda)$adj.r.squared)



# Global test of the RDA result -------------------------------------------

anova(BCI.pa.rda, permutations=how(nperm=999)) 


# Tests of all canonical axes ---------------------------------------------

# p value for each location
anova(BCI.pa.rda, by="term", permutations=how(nperm=999))

# p value for each axis
anova(BCI.pa.rda, by="axis", permutations=how(nperm=999)) 


# Variance inflation factors (VIF) ----------------------------------------

vif.cca(BCI.pa.rda)


# Scaling 1: distance triplot ---------------------------------------------

dev.new(title="RDA scaling 1 + wa") 
par(mfrow = c(2, 2))
plot(BCI.pa.rda, scaling=1, 
     main="Triplot RDA spe.hel ~ env2 - scaling 1 - wa scores")
spe.sc1 <- scores(BCI.pa.rda, choices=1:2, scaling=1, display="sp")
arrows(0, 0, spe.sc1[, 1]*0.92, spe.sc1[, 2]*0.92, length=0, lty=1, col="red")


# Scaling 2 (default): correlation triplot --------------------------------

dev.new(title="RDA scaling 2 + wa")
plot(BCI.pa.rda, main="Triplot RDA spe.hel ~ env2 - scaling 2 - wa scores")
spe.sc2 <- scores(BCI.pa.rda, choices=1:2, display="sp")
arrows(0, 0, spe.sc2[, 1]*0.92, spe.sc2[, 2]*0.92, length=0, lty=1, col="red")


# Triplots of the rda results (lc scores) ---------------------------------

#Site scores as linear combinations of the environmental variables
# Scaling 1

dev.new(title="RDA scaling 1 + lc")
plot(BCI.pa.rda, scaling=1, display=c("sp", "lc", "cn"), 
     main="Triplot RDA spe.hel ~ env2 - scaling 1 - lc scores")
arrows(0, 0, spe.sc1[, 1]*0.92, spe.sc1[, 2]*0.92, length=0, lty=1, col="red")

# Scaling 2

dev.new(title="RDA scaling 2 + lc")
plot(BCI.pa.rda, display=c("sp", "lc", "cn"), 
     main="Triplot RDA spe.hel ~ env2 - scaling 2 - lc scores")
arrows(0, 0, spe.sc2[,1]*0.92, spe.sc2[,2]*0.92, length=0, lty=1, col="red")


# Interpretation ----------------------------------------------------------

#The constrained fraction (0.08413): fraction is the amount of 
#variance of the Y matrix explained by the explanatory variables. 
#9 canonical axes, eigenvalues: RDA1 to RDA9
#unconstrained fraction (0.25481)
#proportion constarined (0.2482)

#These triplots show that UTM.NS, UTM.ES and environmental heterogeneity
#play an important role in the dispersion of the species along the sites. 







