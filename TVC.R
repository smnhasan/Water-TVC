library(ggplot2)
library(ggrepel)
library(ggpubr)
library(rstatix)

options(scipen = 999)


#selection of Top-20 countries
#Data Management


setwd('E:\\WaterTVC')
TVC <- read.csv("Boxplot1.csv")

#Between Tubewell
stat.test <- TVC %>%
  group_by(Tubewell) %>%
  t_test(TVC ~ Place) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test

# Create a box plot
bxp <- ggboxplot(
  TVC, x = "Tubewell", y = "TVC", 
  color = "Place", palette = c("#00AFBB", "#E7B800", "#CC0000", "#006600", "black")
)

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Tubewell", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p", tip.length = 0
)

# Add 10% spaces between the p-value labels and the plot border
bxp + stat_pvalue_manual(
  stat.test,  label = "p", tip.length = 0
) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


# Use adjusted p-values as labels
# Remove brackets
# bxp + stat_pvalue_manual(
#   stat.test,  label = "p.adj", tip.length = 0,
#   remove.bracket = TRUE
# )
# 
# # Show adjusted p-values and significance levels
# # Hide ns (non-significant)
# bxp + stat_pvalue_manual(
#   stat.test,  label = "{p.adj}{p.adj.signif}", 
#   tip.length = 0, hide.ns = TRUE
# )


setwd('E:\\WaterTVC')
TVC <- read.csv("Boxplot3.csv")

#Between Tubewell
stat.test <- TVC %>%
  group_by(Tubewell) %>%
  t_test(TVC ~ Place) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test

# Create a box plot
bxp <- ggboxplot(
  TVC, x = "Tubewell", y = "TVC", 
  color = "Place", palette = c("#00AFBB", "#E7B800", "#CC0000", "#006600", "black")
)

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Tubewell", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p", tip.length = 0
)

# Add 10% spaces between the p-value labels and the plot border
bxp + stat_pvalue_manual(
  stat.test,  label = "p", tip.length = 0
) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


# Use adjusted p-values as labels
# Remove brackets
# bxp + stat_pvalue_manual(
#   stat.test,  label = "p.adj", tip.length = 0,
#   remove.bracket = TRUE
# )
# 
# # Show adjusted p-values and significance levels
# # Hide ns (non-significant)
# bxp + stat_pvalue_manual(
#   stat.test,  label = "{p.adj}{p.adj.signif}", 
#   tip.length = 0, hide.ns = TRUE
# )


setwd('E:\\WaterTVC')
TVC <- read.csv("Boxplot1.csv")


#Between Place
stat.test <- TVC %>%
  group_by(Place) %>%
  t_test(TVC ~ Tubewell) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test

# Create a box plot
bxp <- ggboxplot(
  TVC, x = "Place", y = "TVC", 
  color = "Tubewell", palette = c("#00AFBB", "#E7B800", "#CC0000", "#006600", "black")
)

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Place", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p", tip.length = 0
)

# Add 10% spaces between the p-value labels and the plot border
bxp + stat_pvalue_manual(
  stat.test,  label = "p", tip.length = 0
) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


setwd('E:\\WaterTVC')
TVC <- read.csv("Boxplot3.csv")

#Between Place
stat.test <- TVC %>%
  group_by(Place) %>%
  t_test(TVC ~ Tubewell) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p.adj")
stat.test

# Create a box plot
bxp <- ggboxplot(
  TVC, x = "Place", y = "TVC", 
  color = "Tubewell", palette = c("#00AFBB", "#E7B800", "#CC0000", "#006600", "black")
)

# Add p-values onto the box plots
stat.test <- stat.test %>%
  add_xy_position(x = "Place", dodge = 0.8)
bxp + stat_pvalue_manual(
  stat.test,  label = "p", tip.length = 0
)

# Add 10% spaces between the p-value labels and the plot border
bxp + stat_pvalue_manual(
  stat.test,  label = "p", tip.length = 0
) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))





# Creating the Data
Types = c(rep(c("Tubewell of human", "Tubewell of dairy farm", "Manger of dairy farm", 
                  "Tubewell of poultry farm", "Waterer of poultry farm"), times = 7))

Species = c(rep(c("Escherichia coli", "Klebsiella spp", "Salmonella spp",
               "Shigella spp","Staphylococcus spp","Pseudomonas spp", "Vibrio spp"), each = 5))

Values = c(26,13,14,15,14,24,15,14,12,14,24,13,
                    15,14,15,25,12,14,10,15,15,8,15,7,14,
                    15,7,15,8,15,25,10,15,10,15)

# Passing the Data to DataFrame
BarData = data.frame(Types,Species,Values)

library(gtsummary)

# loading the Library
library(ggplot2)

# Plotting the Data in ggplot2
ggplot(BarData, aes(x = Species, y = Values, 
                          fill = Types, label = Values)) +
  geom_bar(stat = "identity") + geom_text(
    size = 3, position = position_stack(vjust = 0.5))   







#Pie
library(ggplot2)

Species = c(rep(c("Escherichia coli", "Klebsiella spp", "Salmonella spp",
                  "Shigella spp","Staphylococcus spp","Pseudomonas spp", "Vibrio spp")))

PieData <- data.frame(Category = Species, 
                        value = c(82, 79,81,80,59,60,73))

piechart <- ggplot(PieData, aes(x="", y=value, fill=Category)) +
  geom_bar(width=1, stat="identity") +
  coord_polar("y", start=0) +
  xlab("") +
  ylab("Value") +
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

piechart

piechart +
  geom_text(aes(label = paste0(value,
                               " (",
                               scales::percent(value / sum(value)),
                               ")")),
            position = position_stack(vjust = 0.5))


#Antibiotics
Supp = c(rep(c("E. coli", "Klebsiella", "Salmonella", 
                "Shigella", "Pseudomonas", "Vibrio"), each = 30))
Susceptibility <- c(rep(c(rep(c("Susceptible", "Intermediate", "Resistant"),each=10)),times=6))
antibiotic <- c(rep(c(rep(c("GEN", "CIP", "C","LE","CTR","CL","AZM","AMP","AMX","E"),times=6)), times=3))
Values <- c(9,8,2,8,2,2,2,0,0,0,
            5,4,6,5,9,8,6,6,5,6,
            1,3,7,2,4,5,7,9,10,9,
            
            9,8,0,8,5,9,9,0,0,0,
            6,7,6,5,10,6,4,7,6,5,
            0,0,9,2,0,0,2,8,9,10,
            
            9,10,2,1,5,8,8,0,0,0,
            6,5,5,5,10,6,7,5,5,6,
            0,0,8,9,0,1,0,10,10,9,
            
            10,10,2,8,11,1,0,1,0,0,
            3,3,4,5,4,9,7,4,5,5,
            2,2,9,2,0,5,8,10,10,10,
            
            9,12,2,7,2,4,10,0,0,0,
            4,2,4,5,4,4,3,4,3,4,
            2,1,9,3,9,7,2,11,12,11,
            
            10,10,2,8,11,6,0,0,0,1,
            3,3,4,5,4,9,7,5,5,5,
            2,2,9,2,0,0,8,10,10,9
            
            )

# Passing the Data to DataFrame
PieData2 = data.frame(Supp, Susceptibility, antibiotic,Values)

ggplot(data=PieData2, aes(x=antibiotic, y=Values,fill=Susceptibility, angle = 90)) + 
  geom_bar(stat="identity")+
  #facet_grid(~Supp)+
  facet_wrap(vars(Supp), ncol = 3)+
  geom_text(aes(label = paste0(Values)),
            position = position_stack(vjust = 0.5))





