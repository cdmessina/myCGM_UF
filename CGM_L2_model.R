
library(ggplot2)

setwd("/Users/cmessina/documents/UF/HOS6932/model")

#___________________________________________
# Set some funtions
#___________________________________________
#environment

vpd<-function(tmx,tmn, relHum){
  tmean<-(tmx+tmn)/2
  svp<-610.7*10^(7.5*tmean/(237.3+tmean))  #Pa
    #http://physics.holsoft.nl/physics/ocmain.htm
    #Murray FW (1967) On the computation of saturation vapor pressure. J. Appl. Meteorol. 6: 203-204.
    #Monteith JL, Unsworth MH (1990) Principles of environmental physics. Arnold.
  vpd<-(1 - (relHum/100))*svp / 1000 #kPa
  return(vpd)
}



# Development

tb<- 11
topt<- 30
tmax<- 42

tt<-function(tmx,tmn, tb, topt, tmax){

  tmean<- (tmx + tmn) / 2

  if(tmean >= tb)   t1<-tmean - tb
  if(tmean >= topt) t1<- (topt - tb) / (topt - tmax) * (tmean - tmax)
  if(tmean > tmax | tmean <= tb) t1 <- 0
  return(t1)
  
}


xvar<-c(0,tb,topt,tmax)
yvar<-c(0,0,19,0)
data.in<-as.data.frame(cbind(xvar,yvar))
ggplot(data.in, aes(xvar, yvar))+ theme_bw(8) +ylab("Development rate") +xlab("Temperature C") +geom_line(colour="red")

# Leaf area green and senescence (Hammer et al TPLA & SLA)

plant_leaf_area.f<- function(ctt, p_tln){
    tplamax<-p_tln^2.89
    tpla<-tplamax / (1 + exp(-0.017 * (ctt-390))) #First Flower
    spla<-tplamax / (1 + exp(-0.0071* (ctt-1146))) #Maturity
    gpla<-tpla - spla
    return(gpla)
}

# test plan_leaf_area.f
ti<-seq(0,2000,50)
to<-plant_leaf_area.f(ti, 20)
data.in<-data.frame(cbind(ti,to))
names(data.in)<-c("xvar","yvar")
ggplot(data.in, aes(xvar, yvar))+ theme_bw(8) +ylab("Green Leaf area (cm^2)") +xlab("Thermal time (C)") +geom_line(colour="darkgreen")



# Light interception
light_interception.f<-function(p_coefExt, glai){
  li<-1-exp(-p_coefExt * glai)
  return(li)
}

# test light interception
p_coefExt<-0.4
ti<-seq(0,7,0.1)
to<-light_interception.f(p_coefExt, ti)
  
data.in<-data.frame(cbind(ti,to))
names(data.in)<-c("xvar","yvar")
ggplot(data.in, aes(xvar, yvar))+ theme_bw(8) +ylab("Ligth Interception fraction") +xlab("Leaf area Index") +geom_line(colour="blue") +geom_line(aes(y=0.95),linetype="dotted")

# Growth
  
growth.f<-function(p_rue, int_light, hi){
  if(hi <= 0.15) growth <- p_rue * int_light else growth <- p_rue * 0.75 * int_light
  return(growth)  
}

#test growth
p_rue<-1.25
hi<-0.1
ti<-seq(0,1600,10)
to<-growth.f(p_rue, ti,hi)

data.in<-data.frame(cbind(ti,to))
names(data.in)<-c("xvar","yvar")
ggplot(data.in, aes(xvar, yvar))+ theme_bw(8) +xlab("Intercepted radiation (MJ/m2/d)") +ylab("Dry matter growth (g/m2/d)") +geom_line(colour="darkgreen")


# Reproductive allocation
rallo.f<-function(ctt, tt_hi, p_hi, allo_rate){
  out<-p_hi / (1 + exp(allo_rate* (ctt-tt_hi)))
}  

#test reproductive allocation  
  
ti<-seq(0,1500,10)
tt_hi<-1300
p_hi<-0.5  #maximum harvest index
allo_rate<- -0.01
to<-rallo.f(ti, tt_hi, p_hi, allo_rate)

data.in<-data.frame(cbind(ti,to))
names(data.in)<-c("xvar","yvar")
ggplot(data.in, aes(xvar, yvar))+ theme_bw(8) +xlab("Thermal from planting") +ylab("Reproductive allocation (harvest index)") +geom_line(colour="darkred")


#transpiration calculation
transp.f<-function(dmg, vpd, te_coef){
  transp<- dmg * vpd / te_coef  #mm
}

#test transpiration
te_coef<-9 #Pa
dmg<-1.25*20 #g/m2/d
ti<-seq(0.5,4,0.1) #vpd in KPa
to<-transp.f(dmg, ti, te_coef)

data.in<-data.frame(cbind(ti,to))
names(data.in)<-c("xvar","yvar")
ggplot(data.in, aes(xvar, yvar))+ theme_bw(8) +xlab("vpd (kPa)") +ylab("Transpiration (mm)") +geom_line(colour="blue")

rue_g<-c(1,0,0,0,1,0,0,0,1,0)
planting_date<-150
plant_population<-10

#___________________________________________
# Simple model
#___________________________________________
myModel<-function(rue_g, localEnvironment, yearData, localManagement1, localManagement2, localManagement3){

  #Describe the system

  #Environment
  env<-read.table(localEnvironment, header = T, sep=',')
  environment<-env[env$year == yearData,]

  #Management
  planting_date<-localManagement1    #Day of year
  plant_population<-localManagement2 #10 #plants/m2
  irrigation<-localManagement3 #mm
  
  #Genotype
  tb<-11   #base temperature
  topt<-30 #optimal temperature
  tmax<-42 #maximum temperature
  tt_fl<-900  #thermal time to flowering
  tt_mat<-1500 #thermal time to maturity
  p_tln<- 20  #number of leaves
  p_coefExt<-0.4  #coefficient of extinction
  k_rue<-c(0.01, 0.02, 0.15,0.03, 0.14,0.02,0.03,0.08,0.1,0.02) #1.25  #radiation use efficiency  
  if(length(rue_g) >= 2)  p_rue<- 1 + rue_g %*% k_rue else p_rue<-rue_g
  print(p_rue)
  tt_hi<-1300  #dynamic of harvest index
  p_hi<-0.5 #maximum harvest index
  allo_rate<- -0.01 #rate of change in harvest index
  te_coef<-9 #Pa

  #Initialize variables
  doy<-planting_date
  daily_tt<-0
  today_plant_leaf<-0
  daily_light<-0
  daily_growth<-0
  today_hi<-0
  daily_transpiration<-0
  
  c_tt<-0
  c_lai<-0
  c_light<-0
  c_growth<-0
  c_reproductive<-0
  c_transpiration<-0

  #Define start (planting) and stop (exit criteria)
  
  while(doy >= planting_date){

        #calculate rates or daily values
          daily_tmx<-environment$tmx[environment$doy==doy]  # the day of the simulation
          daily_tmn<-environment$tmn[environment$doy==doy]
          daily_srad<-environment$srad[environment$doy==doy]
          daily_relHum<-environment$relHum[environment$doy==doy]
          daily_vpd<-vpd(daily_tmx, daily_tmn, daily_relHum)
            
          daily_tt<-tt(daily_tmx,daily_tmn, tb, topt, tmax)
          today_plant_leaf<-plant_leaf_area.f(c_tt, p_tln) #differential form in the future
          daily_light<-light_interception.f(p_coefExt, c_lai)
          daily_israd<-daily_light * daily_srad
          daily_growth<-growth.f(p_rue, daily_israd, today_hi)
          today_hi<-rallo.f(c_tt, tt_hi, p_hi, allo_rate)  #differential form in the future
          daily_transpiration<-transp.f(daily_growth, daily_vpd, te_coef)

        #integrate rates or daily values
          c_tt<-c_tt + daily_tt
          c_lai<-today_plant_leaf * plant_population / 10000
          c_light<-c_light + daily_light
          c_growth<-c_growth + daily_growth
          c_reproductive<-c_growth * today_hi
          c_transpiration<-c_transpiration + daily_transpiration
          
        #record values
          effective_water_use<- c_reproductive / c_transpiration
          
          daily_out<-c(doy, c_tt, today_plant_leaf, c_lai, c_growth, c_reproductive, c_transpiration, effective_water_use)
          if(doy == planting_date) output<- daily_out else output<- rbind(output, daily_out)
          
          
        #report & exit
          if(c_tt >= tt_mat | daily_tmn <= -3 | c_transpiration >= localManagement3){
            output<-as.data.frame(output)
            names(output)<-list("doy", "ctt","leafArea", "lai","mass","grain","transpiration", "EWU")
            break
          }
        
        #Advance to next day
          doy <- doy + 1
        } #while loop 
  return(output)
  write.csv(output, "output.csv", row.names = F,)
  }





#___________________________________________
# Run the model one time
#___________________________________________


sg1<-c(1,0,0,0,1,0,0,0,1,0)
weather<-"CGM_L2_alachuaDat.csv"
year<-2001
mg<-150
irrigation<-1000
  
myModel(1.25,weather, year ,mg, 10, irrigation)

