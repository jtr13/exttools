# clean up "in grammar" false positives
#
df <- read_csv("in_out_grammar.csv")

# scale_by and scale_df are not ggplot2 scale functions
df$category[df$package == "doBy"] <- "other"
df$component[df$package == "doBy"] <- NA

write_csv(df, file = "in_out_grammar.csv")


