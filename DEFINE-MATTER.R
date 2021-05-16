#Clear the workspace and identify how many time periods (T) you wish your model to run (once you write the commands, press 'Source')

rm(list=ls(all=TRUE))
T<-83

#STEP 1: For each endogenous variable, create a vector that has a length equal to the time periods. (Once you have written the commands, press 'Source'.)

#Endogenous variables
Y<- vector(length=T)
MY<- vector(length=T)
M<- vector(length=T)
REC<- vector(length=T)
DEM<- vector(length=T)
SES<- vector(length=T)
W<- vector(length=T)
HW_CUM<- vector(length=T)
REV_M<- vector(length=T)
CON_M<- vector(length=T)
RES_M<- vector(length=T)
dep_M<- vector(length=T)

#STEP 2: Give values to the parameters (use the values reported in the table in Section 3).

#Parameters
for (i in 1:T) {
  if (i == 1) {
    for (iterations in 1:10){
      
      g_Y<-0.029
      mu<- MY[i]/Y[i] #Gt/$ trillion  or kg/$
      rho<- REC[i]/DEM[i]  
      prop<-0.013
      haz<-0.04 
      con_M<-0.0015 
      
      #STEP 3: Give values to your initial variables (use the values reported in the table in Section 3).
      
      #Initial values
      Y[i] <- 85.9 #trillion US$
      MY[i]<- 52.22#Gt
      M[i]<- MY[i]-REC[i]#Gt 
      REC[i]<-4.8 #Gt
      DEM[i]<-17.67 #Gt
      SES[i]<- 1230.5 #Gt
      W[i]<-DEM[i]-REC[i]#Gt
      HW_CUM[i]<-14.09 #Gt
      REV_M[i]<-M[i]/dep_M[i] #Gt
      CON_M[i]<-con_M*RES_M[i] #Gt
      RES_M[i]<-63.81*REV_M[i] #Gt
      dep_M[i]<-0.02
      
    }
  }
  
  
  #STEP 4: Write down the equations and run the model. (Once you have written the commands, press 'Source'.)
  
  #Equations
  else {
    
    for (iterations in 1:10){
      #Economy
      Y[i] <- Y[i-1]*(1+g_Y)
      
      #Matter, recycling and waste 
      MY[i]<- mu*Y[i] 
      M[i]<-MY[i]-REC[i] 
      REC[i]<-rho*DEM[i] 
      DEM[i]<- prop*SES[i-1]
      SES[i]<-SES[i-1]+MY[i]-DEM[i] 
      W[i]<-DEM[i]-REC[i] 
      HW_CUM[i]<-HW_CUM[i-1]+haz*W[i] 
      REV_M[i]<-REV_M[i-1]+CON_M[i]-M[i] 
      CON_M[i]<-con_M*RES_M[i-1] 
      RES_M[i]<-RES_M[i-1]-CON_M[i] 
      dep_M[i]<-M[i]/REV_M[i-1] 
      
    }
  }
}

#STEP 5: Create a table to report the following variables: Y, W and dep_M. Create also 3 graphs for these variables. (Once you have written the commands, press 'Source'.)

#Table
matrixname<-paste("Table")
assign (matrixname, (round(cbind(Y, W, dep_M), digits=4)))

plot(Table[1:T,c("Y")], type="l", xlab= "Year", ylab="GDP (US$ trillion)", xaxt="n")
axis(side=1, at=c(1, 13, 23, 33, 43, 53, 63, 73, 83) , labels=c("2018", "2030","2040","2050","2060", "2070", "2080", "2090", "2100"))

plot(Table[1:T,c("W")], type="l", xlab= "Year", ylab=expression("Waste"),xaxt="n")
axis(side=1, at=c(1, 13, 23, 33, 43, 53, 63, 73, 83) , labels=c("2018", "2030","2040","2050","2060", "2070", "2080", "2090", "2100"))

plot(Table[1:T,c("dep_M")], type="l", xlab= "Year", ylab="Matter depletion ratio", xaxt="n")
axis(side=1, at=c(1, 13, 23, 33, 43, 53, 63, 73, 83) , labels=c("2018", "2030","2040","2050","2060", "2070", "2080", "2090", "2100"))




