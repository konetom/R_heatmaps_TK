source("C:\\Users\\tom4k\\Desktop\\libraries.R")

filen = "expr1-2"
sheetIndex = 1

df_mne <- read.xlsx(paste(filen,".xlsx", sep = ""), sheetIndex = sheetIndex, header = TRUE, rowIndex = 2:30, colIndex = 1:11, row.names = "Group1")
df_mnesem <- read.xlsx(paste(filen,".xlsx", sep = ""), sheetIndex = sheetIndex, header = TRUE, rowIndex = 32:60, colIndex = 1:11, row.names = "Group1")

normalize <- function(x) (x-min(x))*100/(max(x)-min(x))

norm_df_mne <- data.frame(lapply(df_mne,normalize),row.names = rownames(df_mne))
norm_df_mne <- round_df(norm_df_mne,digits = 0)

italrow <- lapply(rownames(df_mnesem),function(x) bquote(italic(.(x))))
italcol <- lapply(colnames(df_mnesem),function(x) bquote(italic(.(x))))


pdf(paste0(filen,"_mne0-100.pdf", sep = ""), width = 10)
pheatmap(as.matrix(norm_df_mne),
         cutree_cols = 4,
         cluster_rows = F,
         # cluster_cols = F,
         fontsize = 8,
         na_col = "white",
         labels_row = as.expression(italrow),
         labels_col = as.expression(italcol),
         display_numbers = df_mnesem,
         number_color = "black",
         # number_format = "%.2f",
         cellwidth = 50,
         gaps_row = c(7,14,21),
         col = c(colorRampPalette(c("white","green","purple"))(n = 1000))
)
dev.off()
