##------ Tue Sep 17 13:13:19 2024 ------##
# analysis of distance to woodland for ATI records

# distance to woodland ----
# nfi
hist(ati$nfi.dist)
table(ati$nfi.dist>20)/dim(ati)[1]
# FALSE      TRUE 
# 0.4901211 0.5098789 
table(ati$nfi.dist>100)/dim(ati)[1]
table(ati$nfi.dist>200)/dim(ati)[1]
# FALSE      TRUE 
# 0.8066507 0.1933493 
sort(table(ati$nfi_WOODTYPE[ati$nfi.dist>20] )/dim(ati[ati$nfi.dist>20,])[1])
# 78% are braodleaf
hist(ati$nfi.dist[ati$nfi.dist>0])

#awi
hist(ati$awi.dist)
table(ati$awi.dist>20)/dim(ati)[1]
# FALSE       TRUE 
# 0.04174329 0.95825671 
table(ati$awi.dist>200)/dim(ati)[1]

hist((ati$awi.dist[ati$awi.dist>0])/1000, xlab = "Distance to AWI (km)")

# 



quantile(ati$nfi.dist, probs = c(0.,0.5,0.95))


ati.nfi = st_join(ati, nfi, join = st_nearest_feature)

st_nn(ati[1:5,], nfi, k = 1, returnDist = T, maxdist = 100)
?st_join
