library(ape)

tree <- read.tree("../../../TA/Spring 2020/Assignments/Student projects/Anika/Final_alignments/rooted.tre")

data <- read.csv("../../../TA/Spring 2020/Assignments/Student projects/Anika/Final_alignments/Plantaginaceae County Distributions Copy.csv")

#tips <- tree$tip.label

tree$tip.label <- gsub("_plantaginaceae", "", tree$tip.label)
tree$tip.label <- gsub("_verbenaceae", "", tree$tip.label)
tree$tip.label <- gsub("_bignoniaceae", "", tree$tip.label)

data$species <- gsub(" ", "_", data$species)

drop <- tree$tip.label[-which(tree$tip.label %in% data$species)]
drop_2 <- data$species[-which(data$species %in% tree$tip.label)]
drop <- c(drop, "Gratiola_virginiana")
drop_2 <- c(drop_2, "Gratiola_virginiana")
#all_drop <- c(drop, drop_2)

new_tree <- drop.tip(tree, drop)
new_data <- data[-which(data$species %in% drop_2),]
plot(new_tree)

write.tree(new_tree, "../../../TA/Spring 2020/Assignments/Student projects/Anika/Final_alignments/trimmed_tree.tre")
write.csv(new_data, "../../../TA/Spring 2020/Assignments/Student projects/Anika/Final_alignments/renamed_data.csv")
new_tree$tip.label
"Kigelia_africana" %in% new_tree$tip.label
