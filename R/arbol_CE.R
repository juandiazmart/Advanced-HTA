input <- data.frame(p.C.A=.90,p.C.B=.80,p.EA.A=.20,p.EA.B=.30,
                    c.A=100,c.B=80,c.EA=500,c.NoC=150,
                    e.C=20,e.NC=10)

dec_tree <- function(params){
  with(as.list(params),
       {
         ep1 <- p.C.A*p.EA.A
         ep2 <- p.C.A*(1-p.EA.A)
         ep3 <- (1-p.C.A)*(p.EA.A)
         ep4 <- (1-p.C.A)*(1-p.EA.A)
         
         ep5 <- (p.C.B)*(p.EA.B)
         ep6 <- (p.C.B)*(1-p.EA.B)
         ep7 <- (1-p.C.B)*(p.EA.B)
         ep8 <- (1-p.C.B)*(1-p.EA.B)
         
         tc1 <- c.A+c.EA
         tc2 <- c.A
         tc3 <- c.A+c.NoC+c.EA
         tc4 <- c.A+c.NoC
         
         tc5 <- c.B+c.EA
         tc6 <- c.B
         tc7 <- c.B+c.NoC+c.EA
         tc8 <- c.B+c.NoC
         
         costo_espA <- (ep1*tc1) + (ep2*tc2) + (ep3*tc3) + (ep4*tc4)
         costo_espB <- (ep5*tc5) + (ep6*tc6) + (ep7*tc7) + (ep8*tc8)
         
         efect_espA <- (ep1*e.C) + (ep2*e.C) + (ep3*e.NC) + (ep4*e.NC)
         efect_espB <- (ep5*e.C) + (ep6*e.C) + (ep7*e.NC) + (ep8*e.NC)
         
         C <- c(costo_espA,costo_espB)
         LY <- c(efect_espA,efect_espB)
         IC <- C[2] - C[1] #costo_espA - costo_espB
         IE <- LY[2] - LY[1]
         ICER <- IC/IE
         
         # names(C) <- paste("C",c("A","B"),sep="_")
         # names(LY) <- paste("E",c("A","B"),sep="_")
         # names(IC) <- "Costo_incremental"
         # names(IE) <- "Efectividad_incremental"
         # names(ICER) <- "RCEI"
         
         out <- data.frame(Costo=c(C[1],C[2]),
                           Efectvidad=c(LY[1],LY[2]),
                           Inc_costos=c(NA,IC),
                           Inc_efectividad=c(NA,IE),
                           RCEI=c(NA,ICER))
         row.names(out) <- c("Trat_A","Trat_B")
         
         # return(c(C,LY,IC,IE,ICER))
         return(out)
         
       })
}

dec_tree(input)
