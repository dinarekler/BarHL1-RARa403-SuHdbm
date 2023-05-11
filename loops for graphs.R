###   Everything at once   ####
###   Run first the main analysis, this code is using that data ###

###   Change the values to get the percentage of cells that falls in this
###   range per embryo in each group

list_all <- list(control_list,dnRAR_list,dnRAR_SuH_list,control_SuH_list)

control <- numeric()
dnRAR <- numeric()
dnRAR_SuH <- numeric()
control_SuH <- numeric()
list_result <- list(control,dnRAR,dnRAR_SuH,control_SuH)
names(list_result) <- c("control", "dnRAR", "dnRAR_SuH", "control_SuH")




for (j in c(1:length(list_all))) {
  x <- 1
  a <- numeric()
  
  
  for (i in c(1:length(list_all[[j]]))) {
    a <- list_all[[j]][[i]][list_all[[j]][[i]]<0.76 & 0<list_all[[j]][[i]]]
    
    
    list_result[[j]][x] <- length(a)
    
    if (list_result[[j]][x] != 0)
    {list_result[[j]][x] <- list_result[[j]][x]/length(list_all[[j]][[i]])*100}
    
    x <- x+1
  }
}


list_result











#### Each one on it's own ####


control <- numeric()

x <- 1
a <- numeric()

for (i in c(1:9)) {
  a <- control_list[[i]][control_list[[i]]<5 & 4<control_list[[i]]]
  
  control[x] <- length(a)
  
  if (control[x] != 0)
  {control[x] <- control[x]/length(control_list[[i]])*100}
  
  x <- x+1
}



dnRAR <- numeric()

x <- 1
a <- numeric()

for (i in c(1:10)) {
  a <- dnRAR_list[[i]][dnRAR_list[[i]]<5 & 4<dnRAR_list[[i]]]
  
  dnRAR[x] <- length(a)
  
  if (dnRAR[x] != 0)
  {dnRAR[x] <- dnRAR[x]/length(dnRAR_list[[i]])*100}
  
  x <- x+1
}

control_SuH <- numeric()

x <- 1
a <- numeric()

for (i in c(1:8)) {
  a <- control_SuH_list[[i]][control_SuH_list[[i]]<5 & 4<control_SuH_list[[i]]]
  
  control_SuH[x] <- length(a)
  
  if (control_SuH[x] != 0)
  {control_SuH[x] <- control_SuH[x]/length(control_SuH_list[[i]])*100}
  
  x <- x+1
}

dnRAR_SuH <- numeric()

x <- 1
a <- numeric()

for (i in c(1:9)) {
  a <- dnRAR_SuH_list[[i]][dnRAR_SuH_list[[i]]<5 & 4<dnRAR_SuH_list[[i]]]
  
  dnRAR_SuH[x] <- length(a)
  
  if (dnRAR_SuH[x] != 0)
  {dnRAR_SuH[x] <- dnRAR_SuH[x]/length(dnRAR_SuH_list[[i]])*100}
  
  x <- x+1
}

control
dnRAR
dnRAR_SuH
control_SuH