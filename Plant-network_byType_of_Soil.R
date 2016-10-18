#plant netwrork Analysis

NHN2=read.csv("NHN2.csv", header=T, row.names=2)
#NHN2=read.csv("NHN2.csv", header=T)
NHN2=NHN2[,-1]
str(NHN2)
## create data frame for each mitmach or match soil!! 
# so i need three data frames!!
# match mean that they are in the same soil specialization; so clay specialist are in clay and sandy specialist are in sandy loam soil
# Nmacthing means not matching so plant specialist are not in the same soil

matching=data.frame(NHN2[NHN2$match =="Match",])
Nmatching=data.frame(NHN2[NHN2$match =="Mismatch",])
table(Nmatching$Species.code)
table(matching$Species.code)

# Paring number of samples equal for both! matchign and not matching because matching has more survival species.
#max DIPTGL =11 so erase from nmatchign 3

a = Nmatching[ sample( which(Nmatching$Species.code=="DIPTGL"), 11, replace = F), ]
b = Nmatching[ sample( which(Nmatching$Species.code=="DIPTPA"), 3, replace = F), ]
c = Nmatching[ sample( which(Nmatching$Species.code=="DRYOAR"), 7, replace = F), ]
d = Nmatching[ sample( which(Nmatching$Species.code=="DRYOLA"), 13, replace = F), ]
e = Nmatching[ sample( which(Nmatching$Species.code=="HOPEBE"), 4, replace = F), ]
f = Nmatching[ sample( which(Nmatching$Species.code=="HOPEDR"), 1, replace = F), ]
g = Nmatching[ sample( which(Nmatching$Species.code=="SHORBE"), 8, replace = F), ]
h = Nmatching[ sample( which(Nmatching$Species.code=="SHORLA"), 8, replace = F), ]
i = Nmatching[ sample( which(Nmatching$Species.code=="SHORML"), 10, replace = F), ]
j = Nmatching[ sample( which(Nmatching$Species.code=="SHORXA"), 11, replace = F), ]
k = Nmatching[ sample( which(Nmatching$Species.code=="VATINT"), 11, replace = F), ]

NmatchingF=rbind(a,b,c,d,e,f,g,h,i,j,k)



#matching portion!

aa = matching[ sample( which(matching$Species.code=="DIPTGL"), 11, replace = F), ]
bb = matching[ sample( which(matching$Species.code=="DIPTPA"), 3, replace = F), ]
cc = matching[ sample( which(matching$Species.code=="DRYOAR"), 7, replace = F), ]
dd = matching[ sample( which(matching$Species.code=="DRYOLA"), 13, replace = F), ]
ee = matching[ sample( which(matching$Species.code=="HOPEBE"), 4, replace = F), ]
ff = matching[ sample( which(matching$Species.code=="HOPEDR"), 1, replace = F), ]
gg = matching[ sample( which(matching$Species.code=="SHORBE"), 8, replace = F), ]
hh = matching[ sample( which(matching$Species.code=="SHORLA"), 8, replace = F), ]
ii = matching[ sample( which(matching$Species.code=="SHORML"), 10, replace = F), ]
jj = matching[ sample( which(matching$Species.code=="SHORXA"), 11, replace = F), ]
kk = matching[ sample( which(matching$Species.code=="VATINT"), 11, replace = F), ]

matchingF=rbind(aa,bb,cc,dd,ee,ff,gg,hh,ii,jj,kk)

#DIPTGL as sample!! to loook!
DriolaNmatchign=subset(Nmatching[Nmatching$Species.code=="DRYOLA",])
Driolamatchign=subset(matching[matching$Species.code=="DRYOLA",])
##
matchingF=matchingF[-c(307:310)]
NmatchingF=NmatchingF[-c(307:310)]

generalist=data.frame(NHN2[NHN2$match =="Generalist",], na.omit=T)
generalist=generalist[-c(307:310)]


## now just keep a matrix of 1 and 0
generalist[generalist > 1] = 1
generalist1=na.omit(generalist)
write.csv(generalist1, file="generalist1.csv")

matchingF[matchingF > 1] = 1
matching1=matchingF
write.csv(matching1, file="matching1.csv")

NmatchingF[NmatchingF > 1] = 1
Nmatching1=NmatchingF
write.csv(Nmatching1, file="Nmatching1.csv")



## gonna apply and index of similarity between nmatching and matching!!

library(proxy)
library(ape)
simil(Nmatching1, matching1)

```
```{r graphs, include=FALSE}

library(igraph)
#graph ECTBI
Sample1=as.matrix(Nmatching1)
eg1=graph_from_incidence_matrix(Nmatching1)
eg1
col=rgb(runif(78),runif(78),runif(78), runif(78))
plot(eg1,edge.color=col, main="Plant-Fungal network", vertex.label="", vertex.size=3/4) # so messy
V(eg1)$type
shapes=c("circle", "square")[V(eg1)$type+1]
shapes
colors=c("red", "blue")[V(eg1)$type+1]
##first are plants so circle is plants/ blu squares are fungus
##windows(6,6)
## fisrt using degree!
plot(eg1, vertex.shape=shapes, vertex.size=4, vertex.color=colors, edge.color="grey", main="Plant- fungal network EMF-Not mathing", vertex.label="")
mtext("Red= Plants species, Blue=Fungus species") ## how to add text!!
#blue=fungus/ red=plants

##Final graph
Nmatching2=na.omit(Nmatching1)
eg11=graph_from_incidence_matrix(Nmatching2)
eg11
V(eg11)$type
shapes=c("circle", "square")[V(eg11)$type+1]
shapes
colors=c("red", "black")[V(eg11)$type+1]
degree=degree(Nmatching2)
plot(eg11, vertex.shape=shapes, vertex.size=degree/3, vertex.color=colors, edge.color="grey", main="Plant- fungal network EMF- No soil association", vertex.label="")
mtext("Red= Plants species, Black=Fungus species") ## how to add text!!

## graph 2

Sample2=as.matrix(matching1)
eg2=graph_from_incidence_matrix(matching1)
eg2
col=rgb(runif(78),runif(78),runif(78), runif(78))
plot(eg2,edge.color=col, main="Plant-Fungal network", vertex.label="", vertex.size=3/4) # so messy
V(eg2)$type
shapes=c("circle", "square")[V(eg2)$type+1]
shapes
colors=c("red", "blue")[V(eg2)$type+1]
##first are plants so circle is plants/ blu squares are fungus
##windows(6,6)

plot(eg2, vertex.shape=shapes, vertex.size=3, vertex.color=colors, edge.color=col, main="Plant- fungal network EMF", vertex.label="")
mtext("Red= Plants species, Blue=Fungus species") ## how to add text!!
##plot 4!!

##Final graph 2
matching2=na.omit(matching1)## taking out all NA
eg22=graph_from_incidence_matrix(matching2)
eg22
V(eg22)$type
shapes=c("circle", "square")[V(eg22)$type+1]
shapes
colors=c("red", "black")[V(eg22)$type+1]
degree=degree(matching2)

plot(eg22, vertex.shape=shapes, vertex.size=degree/3, vertex.color=colors, edge.color="grey", main="Plant- fungal network EMF-soil association", vertex.label="")
mtext("Red= Plants species, Black=Fungus species") ## how to add text!!

## bipartitte network! eg22!! matching

proj2=bipartite_projection(eg22,multiplicity=TRUE) ## bipartite projection!!
proj2
plot(proj2[[2]],vertex.size=4, vertex.label="", edge.color="grey", main="Fungal network EMF") #fungus
#plot 5

plot(proj2[[1]],vertex.size=4, vertex.label="", edge.color="grey", main="Plants network EMF")

#bipartite proectioneg11
proj1=bipartite_projection(eg11,multiplicity=TRUE) ## bipartite projection!!
proj1
plot(proj1[[2]],vertex.size=4, vertex.label="", edge.color="grey", main="Fungal network EMF") #fungus
#plot 5

plot(proj1[[1]],vertex.size=4, vertex.label="", edge.color="grey", main="Plants network EMF")

###propierties
#indexes!! 
library(bipartite)
Sample1=na.omit(Sample1) ## this is the matrix without "NA"
nested(Sample1, "ALL")
nested(Sample1, "ALL", rescale=TRUE)
# illustration that non-normalised C.score and checker are the same:
nested(Sample1, c("C.score", "checker"), normalise=FALSE)
# C.score   checker 
#0.3630753 3.2156006 
#the old one:
#C.score   checker 
#0.4714373 4.7320690

## EMF
#c.score = 0 indicate nestedness meanwhile c.score=1 indicate perfect checkedboard. So here
#we see that is no nested c.score =0.47


Sample2=na.omit(Sample2)
nested(Sample2, "ALL", rescale=TRUE)
# illustration that non-normalised C.score and checker are the same:
nested(Sample2, c("C.score", "checker"), normalise=FALSE)

# C.score  checker 
#0.377235 2.189050 
#----- old one!!
#C.score   checker 
#0.5433435 5.4885889 


#GENERALIST 
Sample3=as.matrix(generalist1)
Sample3=na.omit(Sample3)
nested(Sample3, "ALL", rescale=TRUE)
# illustration that non-normalised C.score and checker are the same:
nested(Sample3, c("C.score", "checker"), normalise=FALSE)
#  C.score   checker 
#0.2500047 0.8676630 

#ALL ECM

Prueba2=na.omit(Prueba2)
nested(Prueba2, "ALL")
nested(Prueba2, "ALL", rescale=TRUE)
# illustration that non-normalised C.score and checker are the same:
nested(Prueba2, c("C.score", "checker"), normalise=FALSE)

#  C.score    checker 
#0.9373804 20.3015523
#we see that is no nested c.score =0.95 we see is not nested!!

#what happend with nestedness that suposse to be high? actully nop!!

Ch.score=c(0.3630753, 0.377235 , 0.2516441 )
Checkers=c(3.2156006, 2.189050 , 0.8733526 )
plot(Ch.score,type="b")
plot(Checkers,type="b")

# number of OTUS by specie
##NOMATCHING
data1=data.frame(Nmatching1)
data1$speciesN=with(data1, rowSums(data1))
mean(data1$speciesN, na.rm = TRUE)
#6.908046
#6.554545(old)
min(data1$speciesN,na.rm = TRUE)
#1
max(data1$speciesN,na.rm = TRUE)
#25

#MATCHING
data2=data.frame(matching1)
data2$speciesN=with(data2, rowSums(data2))
mean(data2$speciesN, na.rm = TRUE)
#5.714286
#5.984375(old)
min(data2$speciesN,na.rm = TRUE)
#1
max(data2$speciesN,na.rm = TRUE)
#15

## GENERALIST

data3=data.frame(generalist1)
data3$speciesN=with(data3, rowSums(data3))
mean(data3$speciesN, na.rm = TRUE)
#8.452381
min(data3$speciesN,na.rm = TRUE)
#1
max(data3$speciesN,na.rm = TRUE)
#32

generalist2=na.omit(generalist1)


A=6.908046
B=5.714286
C=8.452381

c=c(A,B,C)

barplot(c, ylim=c(0,35), xlab=c("not matching", "matching", "generalist"), col=c(1,2,3))



connectedness(Sample1)
#0.5034663
connectedness(Sample2)
#0.5039351
connectedness(Sample3)
#0.3409742


## matrixes 
#Sample1 =Nmtching2
#Sample2=matching2
#sample3=generlaist2
#eg11= Nmatching
#eg22= matching

#for not matching!
#  C.score   checker 
#0.3630753 3.2156006 

NMCS=0.3630753 

##matching
# C.score  checker 
#0.377235 2.189050 
MMCS=0.377235

#connectedness(sample1)
NMCC=0.5034663

#connectedness(sample2)
MMCC=0.5039351



A1=sample(Nmatching2, replace=T)  #we randomize plant names!
A1=na.omit(A1)
connectedness(A1)
0.4507711

## permuattions
t=1000
r.pre.permutation=vector(length=t)
r.pre.permutationNE=vector(length=t)
for (i in 1:t){
  s=sample(Nmatching2, replace=T)
  r.pre.permutation[i]=connectedness(s)
  r.pre.permutationNE[i]=nested(s, "C.score", normalise=FALSE)
}

p.valueNMCC=length(which(r.pre.permutation>NMCC))/t
p.valueNMCC

p.valueNMCS=length(which(r.pre.permutationNE>NMCS))/t
p.valueNMCS


A2=sample(matching2, replace=T)  #we swept 
A2=na.omit(A2)
connectedness(A2)
0.4542351

t=1000
Mr.pre.permutation=vector(length=t)
Mr.pre.permutationNE=vector(length=t)
for (i in 1:t){
  ss=sample(matching2, replace=T)
  Mr.pre.permutation[i]=connectedness(ss)
  Mr.pre.permutationNE[i]=nested(ss, "C.score", normalise=FALSE)
}

p.valueMMCC=length(which(Mr.pre.permutation>MMCC))/t
p.valueMMCC

p.valueMMCS=length(which(Mr.pre.permutationNE>MMCS))/t
p.valueMMCS

##boxplot connectedness!
par(mfrow=c(1,1), mar=c(2.5,2.5,2.5,2.5))
boxplot(r.pre.permutation, Mr.pre.permutation, col=c("white", "gray"), ylim=c(0.2,0.6), names=c("Not-matching", "Matching"), main="connectedness")
#abline(h=0, lty=2)
points(1,NMCC, pch="*", cex=3, col="blue")
points(2,MMCC, pch="*", cex=3, col="red")

##boxplot nestedness

par(mfrow=c(1,1), mar=c(2.5,2.5,2.5,2.5))
boxplot(r.pre.permutationNE, Mr.pre.permutationNE, col=c("white", "gray"), ylim=c(0.2,0.6), names=c("Not-matching", "Matching"), main="nestedness")
points(1,NMCS, pch="*", cex=3, col="blue")
points(2,MMCS, pch="*", cex=3, col="red")




## modularity!

#NO MATCHING
lay=layout.fruchterman.reingold(eg11) #set up layout
par(mfrow=c(2,2), mar=c(1,1,1,1))
wt=walktrap.community(eg11)
fg=fastgreedy.community(eg11)
le=leading.eigenvector.community(eg11)
eb=edge.betweenness.community(eg11)

modularity(wt)
#0.4598963
modularity(fg)
modularity(le)
modularity(eb)
lay=layout.fruchterman.reingold(eg11) #set up layout
par(mfrow=c(1,2), mar=c(1,1,1,1))
plot(wt, eg11, vertex.shape=shapes, vertex.size=2, layout=lay, main="walktrap-Not Matching", vertex.label="")

## matching
lay=layout.fruchterman.reingold(eg22) #set up layout
wt=walktrap.community(eg22)
fg=fastgreedy.community(eg22)
le=leading.eigenvector.community(eg22)
eb=edge.betweenness.community(eg22)
modularity(wt)
#0.5842839
modularity(fg)

modularity(le)

modularity(eb)

lay=layout.fruchterman.reingold(eg22) #set up layout
plot(wt, eg22, vertex.shape=shapes,vertex.size=2, layout=lay, main="walktrap-Matching", vertex.label="")

#No change in structure nestedness or connectance ocurred!
#Neither tree species identity, nor edaphic specialisation, showed any influence on the composition and richness of the EMF colonists of seedlings
#increase in the number of fungal association in not-mathcing soil (compensation effect)