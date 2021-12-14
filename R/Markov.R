# install.packages("heemod")
cAsymp<-500
cProg<-3000
cDrug<-1000
cDeath<-1000
uAsymp<-0.95
uProg<-0.75
tpProg<-0.01
tpDcm<-0.15

Eff<-.5
ini_age<-55
oDR<-0.035
cDR<-0.035
ciclos<-45

crear_traza<-function(ciclos) {
  traza=matrix(ncol = 15,nrow = ciclos+1)
  colnames(traza)<-c("Asymptomatic","Progressive disease",
                    "Dead from disease","Dead other causes",
                    "pmuerte","edad","check","statecost","transcost",
                    "totalcost","disc_totalcost","lifeyears","qalys",
                    "disc_lifeyears","disc_qalys")
  return(traza)
}


no_drug<-crear_traza(ciclos)
no_drug[1,1]<-1
no_drug[1,2:4]=0
no_drug[,6]=0:ciclos+ini_age

drug=crear_traza(ciclos)
drug[1,1]=1
drug[1,2:4]=0
drug[,6]=0:ciclos+ini_age

# mortalidad=data.frame(parameters=c("natDeath55","natDeath65","natDeath75","natDeath85"),
#                       p_muerte=c(0.014,0.038,0.091,0.196))

for (i in 1:(ciclos+1)){
  if (no_drug[i,"edad"]>=55 & no_drug[i,"edad"]<65) {
    no_drug[i,"pmuerte"]=0.0138
  } else if (no_drug[i,"edad"]>=65 & no_drug[i,"edad"]<75) {
    no_drug[i,"pmuerte"]=0.0379
  } else if (no_drug[i,"edad"]>=75 & no_drug[i,"edad"]<85) {
    no_drug[i,"pmuerte"]=0.0912
  } else {
    no_drug[i,"pmuerte"]=0.1958
  }
  
  if (drug[i,"edad"]>=55 & drug[i,"edad"]<65) {
    drug[i,"pmuerte"]=0.0138
  } else if (drug[i,"edad"]>=65 & drug[i,"edad"]<75) {
    drug[i,"pmuerte"]=0.0379
  } else if (drug[i,"edad"]>=75 & drug[i,"edad"]<85) {
    drug[i,"pmuerte"]=0.0912
  } else {
    drug[i,"pmuerte"]=0.1958
  }
}


for (i in 2:(ciclos+1)){
  no_drug[i,"Asymptomatic"]=no_drug[i-1,"Asymptomatic"]*(1-tpProg*(i-1)-no_drug[i-1,"pmuerte"])
  no_drug[i,"Progressive disease"]=no_drug[i-1,"Progressive disease"]*(1-tpDcm-no_drug[i-1,"pmuerte"])+no_drug[i-1,"Asymptomatic"]*tpProg*(i-1)
  no_drug[i,"Dead from disease"]=no_drug[i-1,"Progressive disease"]*tpDcm+no_drug[i-1,"Dead from disease"]
  no_drug[i,"Dead other causes"]=no_drug[i-1,"Dead other causes"]+no_drug[i-1,"Asymptomatic"]*no_drug[i-1,"pmuerte"]+no_drug[i-1,"Progressive disease"]*no_drug[i-1,"pmuerte"]
  no_drug[i,"statecost"]=no_drug[i,"Asymptomatic"]*cAsymp+no_drug[i,"Progressive disease"]*cProg
  no_drug[i,"transcost"]=no_drug[i,"Progressive disease"]*tpDcm*cDeath
  no_drug[i,"totalcost"]=no_drug[i,"statecost"]+no_drug[i,"transcost"]
  no_drug[i,"disc_totalcost"]=no_drug[i,"totalcost"]/((1+cDR)^(i-1))
  no_drug[i,"lifeyears"]=no_drug[i,"Asymptomatic"]+no_drug[i,"Progressive disease"]
  no_drug[i,"qalys"]=no_drug[i,"Asymptomatic"]*uAsymp+no_drug[i,"Progressive disease"]*uProg
  no_drug[i,"disc_lifeyears"]=no_drug[i,"lifeyears"]/((1+oDR)^(i-1))
  no_drug[i,"disc_qalys"]=no_drug[i,"qalys"]/((1+oDR)^(i-1))
  
  drug[i,"Asymptomatic"]=drug[i-1,"Asymptomatic"]*(1-tpProg*(i-1)*Eff-drug[i-1,"pmuerte"])
  drug[i,"Progressive disease"]=drug[i-1,"Progressive disease"]*(1-tpDcm-drug[i-1,"pmuerte"])+drug[i-1,"Asymptomatic"]*tpProg*(i-1)*Eff
  drug[i,"Dead from disease"]=drug[i-1,"Progressive disease"]*tpDcm+drug[i-1,"Dead from disease"]
  drug[i,"Dead other causes"]=drug[i-1,"Dead other causes"]+drug[i-1,"Asymptomatic"]*drug[i-1,"pmuerte"]+drug[i-1,"Progressive disease"]*drug[i-1,"pmuerte"]
  drug[i,"statecost"]=drug[i,"Asymptomatic"]*(cAsymp+cDrug)+drug[i,"Progressive disease"]*cProg
  drug[i,"transcost"]=drug[i,"Progressive disease"]*tpDcm*cDeath
  drug[i,"totalcost"]=drug[i,"statecost"]+drug[i,"transcost"]
  drug[i,"disc_totalcost"]=drug[i,"totalcost"]/((1+cDR)^(i-1))
  drug[i,"lifeyears"]=drug[i,"Asymptomatic"]+drug[i,"Progressive disease"]
  drug[i,"qalys"]=drug[i,"Asymptomatic"]*uAsymp+drug[i,"Progressive disease"]*uProg
  drug[i,"disc_lifeyears"]=drug[i,"lifeyears"]/((1+oDR)^(i-1))
  drug[i,"disc_qalys"]=drug[i,"qalys"]/((1+oDR)^(i-1))
}

no_drug[,"check"]<-rowSums(no_drug[,c(1:4)])
drug[,"check"]<-rowSums(drug[,c(1:4)])

costo_esp_nodrug <- sum(no_drug[,"disc_totalcost"],na.rm = T)
costo_espdrug <- sum(drug[,"disc_totalcost"],na.rm = T)

efect_esp_nodrug <-sum(no_drug[,"disc_qalys"],na.rm = T)
efect_esp_drug <- sum(drug[,"disc_qalys"],na.rm = T)

C <- c(costo_esp_nodrug,costo_espdrug)
LY <- c(efect_esp_nodrug,efect_esp_drug)
IC <- C[2] - C[1] #costo_espA - costo_espB
IE <- LY[2] - LY[1]
ICER <- IC/IE


out <- data.frame(Costo=c(C[1],C[2]),
                  Efectvidad=c(LY[1],LY[2]),
                  Inc_costos=c(NA,IC),
                  Inc_efectividad=c(NA,IE),
                  RCEI=c(NA,ICER))

