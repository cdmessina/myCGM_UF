# Model application & simulation

# evaluate effect of early planting on yield, transp, EWU
o120<-myModel(sg1,weather, year, 90, 10, irrigation)
o150<-myModel(sg1,weather, year, 180, 10, irrigation)
o180<-myModel(sg1,weather, year, 270, 10, irrigation)

o120$run<-rep("T1",dim(o120)[[1]])
o150$run<-rep("T2",dim(o150)[[1]])
o180$run<-rep("T3",dim(o180)[[1]])

data.in<-as.data.frame(rbind(o180,o120,o150))
data.in$run<-as.factor(data.in$run)
data.in<-data.in[data.in$EWU != "NaN",]

ggplot(data.in, aes(ctt,grain,group=run)) + theme_bw(8) +ylab("Yield (g/m2)") +xlab("Thermal time") +geom_line(aes(color=run), size=0.5) +geom_line(aes(ctt, mass, color=run), size=0.5)

ggplot(data.in, aes(ctt,EWU,group=run)) + theme_bw(8) +ylab("EWU (g/mm)") +xlab("Thermal time")+geom_line(aes(color=run), size=0.5)

ggplot(data.in, aes(ctt,transpiration,group=run)) + theme_bw(8) +ylab("Transpiration (mm)") +xlab("Thermal time")+geom_line(aes(color=run), size=0.5)


# evaluate effect of G on yield, transp, EWU
sg0<-1.25
sg1<-1.65
sg2<-2.0

o0<-myModel(sg0,weather, year, 90, 10, irrigation)
o1<-myModel(sg1,weather, year, 90, 10, irrigation)
o2<-myModel(sg2,weather, year, 90, 10, irrigation)


o0$run<-rep("T1",dim(o0)[[1]])
o1$run<-rep("T2",dim(o1)[[1]])
o2$run<-rep("T3",dim(o2)[[1]])

data.in<-as.data.frame(rbind(o0,o1,o2))
data.in$run<-as.factor(data.in$run)
data.in<-data.in[data.in$EWU != "NaN",]

ggplot(data.in, aes(ctt,grain,group=run)) + theme_bw(8) +ylab("Yield or mass (g/m2)") +xlab("Thermal time")+geom_line(aes(color=run), size=0.5) +geom_line(aes(ctt, mass, color=run), size=0.5)

ggplot(data.in, aes(ctt,transpiration,group=run)) + theme_bw(8) +ylab("Transpiration (mm)") +xlab("Thermal time")+geom_line(aes(color=run), size=0.5)


#Demonstrate gxe from additive g and perfect markers
# Case 1 Planting date Doy 90 (spring planting)
sg0<-c(0,0,0,0,0,0,0,0,0,0)
sg1<-c(1,0,0,0,1,0,0,0,1,0)
sg2<-c(1,1,1,1,1,1,1,1,1,1)


o0<-myModel(sg0,weather, year, 90, 10, 100)
o0a<-myModel(sg0,weather, year, 90, 10, 200)

o1<-myModel(sg1,weather, year, 90, 10, 100)
o1a<-myModel(sg1,weather, year, 90, 10, 200)

o2<-myModel(sg2,weather, year, 90, 10, 100)
o2a<-myModel(sg2,weather, year, 90, 10, 200)

data.in<-as.data.frame(matrix(c("G1", 100, max(o0$grain), "G1", 200, max(o0a$grain), "G2", 100, max(o1$grain), "G2", 200, max(o1a$grain), "G3", 100, max(o2$grain), "G3", 200, max(o2a$grain)), ncol=3, byrow=T))
names(data.in)<-c("Genotype", "Environment", "Yield")
data.in$Enviroment<-as.numeric(data.in$Environment)
data.in$Yield<-as.numeric(data.in$Yield)

ggplot(data.in, aes(Environment,Yield,group=Genotype)) + theme_bw(8) +ylab("Yield (g/m2)") +xlab("Environment (mm)")+geom_line(aes(color=Genotype), size=0.5) 

# Case 2 Planting date Doy 230 (Fall planting)

o0<-myModel(sg0,weather, year, 230, 10, 100)
o0a<-myModel(sg0,weather, year, 230, 10, 200)

o1<-myModel(sg1,weather, year, 230, 10, 100)
o1a<-myModel(sg1,weather, year, 230, 10, 200)

o2<-myModel(sg2,weather, year, 230, 10, 100)
o2a<-myModel(sg2,weather, year, 230, 10, 200)

data.in<-as.data.frame(matrix(c("G1", 100, max(o0$grain), "G1", 200, max(o0a$grain), "G2", 100, max(o1$grain), "G2", 200, max(o1a$grain), "G3", 100, max(o2$grain), "G3", 200, max(o2a$grain)), ncol=3, byrow=T))
names(data.in)<-c("Genotype", "Environment", "Yield")
data.in$Enviroment<-as.numeric(data.in$Environment)
data.in$Yield<-as.numeric(data.in$Yield)

ggplot(data.in, aes(Environment,Yield,group=Genotype)) + theme_bw(8) +ylab("Yield (g/m2)") +xlab("Environment (mm)")+geom_line(aes(color=Genotype), size=0.5)
