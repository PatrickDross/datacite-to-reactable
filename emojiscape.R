remotes::install_github("matt-dray/emojiscape")
library(emojiscape)

getwd()

sink("sink-examp.txt")
generate("woods", grid_size = 20)
sink()

