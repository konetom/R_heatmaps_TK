# prerequisites:
library(pheatmap)
library(viridis)
library(readxl)
library(tidyverse)
library(dplyr)
library(xlsx)
library(scales)


# load data:
c <- read_excel("Input/C25.xlsx")
cframe <- data.frame("Name" = c$ID, "K27me3_coverage_over_gene_body_in_WT_shoot" = c$MACS_C5_H3_corrected, "K27me3_coverage_over_gene_body_in_WT_root" = c$MACS_C6_H3_corrected)
r <- read_excel("Input/R.xlsx",col_names = T,sheet = 3)
rframe <- data.frame("Name" = r$Name, "Relative_gene_expression__fold_change_of_ccss_and_WT" = r$Fold_change)
mine <- read_excel("Input/mine.xlsx",col_names = T,sheet = 2)
mineframe <- data.frame("Name" = mine$ID, "Gene" = mine$Gene, "Num" = mine$Num)


# merging all dataframes into one df:
merged <- merge.data.frame(cframe, rframe, by = "Name", all.y = T)
merged[is.na(merged)] <- 0
fullymerged <- merge.data.frame(mineframe, merged, by = "Name", all.x = T)
fullymerged <- fullymerged[,c(3:6)]
joined <- merge.data.frame(mineframe, fullymerged, by = "Num", sort = T)
joined <- joined[, c(3,4,5,6)]
rf1 <- joined$Relative_gene_expression__fold_change_of_ccss_and_WT
joined$Relative_gene_expression__fold_change_of_ccss_and_WT <- rescale(rf1, from = c(min(rf1),max(rf1)), to = c(0,100))
df <- joined %>% remove_rownames %>% column_to_rownames(var = "Gene")


# creating groups:
grps <- c("1: Receptors","2: Transcription factors upstream COP1", "3: Transcription factors downstream COP1", "4: Effectors")
fac <- factor(rep(grps, c(8,20,11,11)))
rannot <- data.frame("Function" = fac)
rownames(rannot) <- rownames(df)


# building "pretty heatmap - pheatmap"
height = 15
pdf(paste0("heatmap_(height_",height,").pdf"), height = height, width = 10)
pheatmap(as.matrix(df),
         cluster_cols = F,
         # cluster_rows = F,
         number_format = "%.1f",
         number_color = "white",
         main = "Candidate genes",
         clustering_method = "centroid", # check all possible methods
         cutree_rows = 2,
         border_color = "grey60",
         annotation_legend = T,
         legend_breaks = T,
         legend_labels = c(""),
         # display_numbers = T,
         cellheight = 15,
         cellwidth = 75,
         angle_col = 90,
         na_col = "white",
         annotation_row = rannot,
         col = c(colorRampPalette(c("green", "cyan", "purple"))(2000)))
dev.off() # save as PDF
