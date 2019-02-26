##### Imputation of multiple rows #####

for(i in 1:nrow(Newdf)) {
  Newdf[i, ][is.na(Newdf[i, ])] <- mean(as.numeric(Newdf[i, ]), na.rm = TRUE)
}
head(Newdf) 