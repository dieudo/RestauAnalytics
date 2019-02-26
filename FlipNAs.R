
#################FLIPING NA TO THE FRONT#########################

df=my_data

#Flip NA to the front to conserve order
df[] <-  t(apply(df, 1, function(x) c(x[is.na(x)],x[!is.na(x)])))
head(df)