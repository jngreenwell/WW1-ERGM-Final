# The idea is to first get some impressions of the data by
# calculating descriptive statistics and making plots
# so we can run an "informed" ERGM.
plot(mat1,vertex.col='Gender')

# The network is highly segregated according to sex.
# Let us recolour so it looks more clich?-intuitive:
color=rep('white',length(atts1$Gender)) ## Alternate way of looping for the color when gplot
color[atts1$Gender==2]='tomato'
color[atts1$Gender==1]='navy'

# This has created a vector to use in plot statement below along with other vectors
# The binary attribute for participant smoking will show smokers as larger sized nodes compared to those who don't smoke
size=rep(1,length(atts1$PersonSmoke))
size[atts1$PersonSmoke==2]=2

#The node shape will indicate high risk and low risk ethnicities with high risk nodes having a triangle shape and low risk nodes as circles
shape=rep(8.0,length(atts1$MaoPI))
shape[atts1$MaoPI==2]=3

###version 1 for plotting with node ID's
gplot(mat1,gmode='digraph',displaylabels=TRUE,label.cex=0.2,label.col='red',vertex.col=color,vertex.cex=size,vertex.sides=shape,displayisolates=FALSE)

#can also make a 3d graph
gplot3d(mat1,gmode='digraph',displaylabels=TRUE,vertex.col=color)

# Histograms and plots of in and out degree as well as reciprocals, in one graphic but need to change mat1 back to a matrix from a network:
matrix1=as.matrix(mat1)

par(mfrow=c(1,3))
odegree <- rowSums(matrix1)
idegree <- colSums(matrix1)
rdegree <- rowSums(symmetrize(matrix1, rule="strong"))
hist(odegree)
hist(idegree)
hist(rdegree)

#Need to switch off the plot screen
dev.off()

# Now plot the sociomatrix:

plot.sociomatrix(matrix1)

# This relieves fears of strong outliers.
# 
# What about the joint distribution of indegree, outdegree, and
# reciprocated degree?
par(mfrow=c(1,3))
plot(odegree,idegree)
plot(odegree,rdegree)
plot(idegree,rdegree)

# All seems positively related. However, a frequent finding for
# friendship at school is that there is hierarchy in the network
# as well. To show this, first switch off the graphics screen:

dev.off()

# Then calculate the correlation of indegree and outdegree: 
cor(odegree,idegree)

# A positive value - as one would expect, given the diagrams.
# It would suggest that 'popular' students (high indegree) are
# also 'active' students (high outdegree), and not a hierarchy
# between only-senders at the bottom and only-receivers on top.
# 
# However, this impression is contaminated by reciprocity!
# Let us calculate the correlation between the un-reciprocated
# parts of indegree and outdegree:

cor(odegree-rdegree,idegree-rdegree)

# Indeed a negative result! Senders of asymmetric friendship
# relations tend NOT to be receivers of such relations, and
# vice versa -- this indicates a (globally) hierarchical relation!

# Compare average degree & standard deviation between sexes:

tapply(odegree,atts1$Gender,mean)
tapply(odegree,atts1$Gender,sd)

# It seems the girls are slightly more connected than the boys.
# 
# A bit more descriptives still about structural features:

gden(mat1)
grecip(mat1, measure="edgewise")
gtrans(mat1)

# The following command gives a test of observed transitivity
# against the amount expected by simulations of conditionally
# random networks, given the correct (empirical) dyad census:

cug.test(mat1,"gtrans",cmode="dyad.census")

# We can also take a look at this census, and the triad census:

dyad.census(mat1)
triad.census(mat1)

# Now let us look at two more 'global' features of the network.
# What does (1) the distribution of geodesic distances and (2) the
# shared partner distribution look like?
#
# Keep in mind these are "global" structural features we are trying 
# to also achieve with a good fitting ERG model - i.e., goodness of
# fit indicators!
distances <- geodist(mat1)$gdist
distances[1:5,1:5]
hist(distances,xlab="directed distance between pairs")

# 'Edgewise shared partners' distribution (a measure of higher
# order clustering):

plot(0:10,summary(mat1~esp(0:10)), type="b",
     xlab="number of edgewise shared partners", ylab="frequency")

# close graphics screen:

dev.off()