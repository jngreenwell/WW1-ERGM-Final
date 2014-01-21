#Need to load statnet and associated packages
library(statnet)

#To Load in the Matrices and Attributes for W1
matrix1=as.matrix(read.csv('WC_W1 Matrix No Zero.csv',header=FALSE))
atts1=read.csv('WC_W1 Attributes No Zero.csv',header=TRUE,na.strings='0')
dyad1=as.matrix(read.csv('WC_W1 Dyad No Zero-Sym.csv',header=FALSE))
#Check Attribute Collums names
colnames(atts1)

#Provide IDs for the matrix1
rownames(matrix1)=atts1$ID
colnames(matrix1)=atts1$ID

#Provide ID's for the Dyads
rownames(dyad1)=atts1$ID
colnames(dyad1)=atts1$ID

#change matrix to a network object and check it
mat1=network(matrix1,directed=TRUE)
mat1
summary(mat1)

#same for the dyadic covariates
dyadic1=network(dyad1,directed=TRUE)
dyadic1
summary(dyadic1)

#Now to add attributes to the network object
mat1%v%'ID'=atts1$ID
mat1%v%'Gender'=atts1$Gender
mat1%v%'MaoPI'=atts1$MaoPI
mat1%v%'Ethnic'=atts1$Ethnic
mat1%v%'PTJob'=atts1$PTJob
mat1%v%'CigsOk'=atts1$CigsOk
mat1%v%'AlcOk'=atts1$AlcOk
mat1%v%'DrugsBad'=atts1$DrugsBad
mat1%v%'SchoolPercept'=atts1$SchoolPercep
mat1%v%'PhysActive'=atts1$PhysActive
mat1%v%'WorkerSmoke'=atts1$WorkerSmoke
mat1%v%'WorkerDrink'=atts1$WorkerDrink
mat1%v%'WorkQuitSmoke'=atts1$WorkQuitSmoke
mat1%v%'WorkQuitDrink'=atts1$WorkQuitDrink
mat1%v%'PersonSmoke'=atts1$PersonSmoke
mat1%v%'PersonDrink'=atts1$PersonDrink
mat1%v%'PosSmoke'=atts1$PosSmoke
mat1%v%'PosDrink'=atts1$PosDrink
mat1%v%'Adult.Smoke'=atts1$Adult.Smoke
mat1%v%'Sibling.Smoke'=atts1$Sibling.Smoke
mat1%v%'FemaleSmoke'=atts1$FemaleSmoke
mat1%v%'MaleSmoke'=atts1$MaleSmoke
mat1%v%'FemQuitSmok'=atts1$FemQuitSmok
mat1%v%'MaleQuitSmok'=atts1$MaleQuitSmok
mat1%v%'SibQuitSmoke'=atts1$SibQuitSmoke
mat1%v%'FemaleDrink'=atts1$FemaleDrink
mat1%v%'MaleDrink'=atts1$MaleDrink
mat1%v%'SiblingDrink'=atts1$SiblingDrink
mat1%v%'FemQuitDrink'=atts1$FemQuitDrink
mat1%v%'MaleQuitDrink'=atts1$MaleQuitDrink
mat1%v%'SibQuitDrink'=atts1$SibQuitDrink

