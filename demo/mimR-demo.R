
## This demo contains (for the time being) only some of the examples
## from the documentation files

####
#### Findes i DOC-filerne ####

##
## Til DOC - partition.string.by
partition.string.by("here,comes,the,sun", ",")
## Til DOC
##

##
## Til DOC - mcm
#mcm()
#fact  a2 b2 c2
#statread abc 
#25 2 17 8 14 9 6 8 !
#satmod; fit
#print f
## Til DOC
##


##
## Til DOC - mim.display
data(rats)
rats$z <- round(rnorm(length(rats$W1),sd=2, mean=2))
names.conv <-mim.read(rats, mim.names=c("a","b","x","y","z"),mim.labels=names(rats))
mim.cmd("mod ab/abx,aby,abz/abxyz; fit"); 

src()
mim.display("y","ax") ## OK
mim.display("b","ax") ## OK

src();
mim.display("xy","a") #OK
mim.display("xy","az") ## OK
## Til DOC
##


##
## Til DOC - mim.read
data(rats)
mim.read(rats)

mim.read(rats, mim.names=c("a","b","x","y"), mim.labels=names(rats))
# is equivalent to
mim.read(rats, file="c:\\mimrats.txt", submit=F, mim.names=c("a","b","x","y"), mim.labels=names(rats))
mim.cmd("input c:\\mimrats.txt")
## Til DOC
##

##
## Til DOC - mim.diary.data
d1 <- mim.print("d")

d2 <- mim.diary.data()
## Til DOC
##



##
## Til DOC - get.mim.parameters
data(rats)
mim.read(rats, mim.names=c("a","b","x","y"), mim.labels=names(rats))
mim.cmd("model ab/abx,aby/x,y; fit")

r1 <- mim.cmd("print t")
r2 <- get.mim.parameters(r1)
r2
## equivalent to 
mim.print("t")

mim.cmd(c("fact a2 b2 c2; statread abc",  "25 2 17 8 14 9 6 8 !")) 
mim.cmd("mod ab,ac,bc; fit")

s1 <- mim.cmd("print f")
get.mim.parameters(s1)
## equivalent to 
mim.print("f")
## Til DOC
##


##
## Til DOC - mim.fit
data(carcass)
mim.read(carcass, mim.names=c("a","b","c","d","e","f","y"),mim.labels=names(carcass))

mim.cmd("model //ay,by,cy,dy,ey,fy")
mim.fit()

mim.cmd("model //abcdefy")
mim.fit()
## Til DOC
##


##
## Til DOC - mim.emfit
data(carcass)
mim.read(carcass, mim.names=c("a","b","c","d","e","f","y"))

mim.cmd("cont L; calculate L=ln(0)");
mim.cmd("model //aL,bL,cL,dL,eL,fL,yL")
mim.emfit()

# Insert initial values for EM algorithm
#carcass$L <- rnorm(nrow(carcass))
#mim.read(carcass, mim.names=c("a","b","c","d","e","f","y","L"))
#mim.cmd("calculate L=ln(0)");
#mim.cmd("model //aL,bL,cL,dL,eL,fL,yL")
#mim.emfit("S")
## Til DOC
##


##
## Til DOC - mim.stepwise
data(carcass)
mim.read(carcass, mim.names=c("a","b","c","d","e","f","y"))

mim.cmd("model //abcdefy")
mim.stepwise()
## Til DOC
##

##
## Til DOC - mim.testdelete
data(carcass)
mim.read(carcass, mim.names=c("a","b","c","d","e","f","y"))

mim.cmd("model //abcdefy")
mim.testdelete("ay")

mim.testdelete("ay",options="S")
## Til DOC
##

#### Findes i DOC-filerne ####
####

