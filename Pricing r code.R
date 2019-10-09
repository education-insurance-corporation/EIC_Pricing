#####################################################################################################
#         Program name  : EIC_Flat_withMin.R                                                        #
#         Input         : ~/Input/Cohort_Assump.csv                                                 #
#         Output        : ~/Output/PricingSim_Sample_mm-dd-yy_hhmm.csv                              #
#         Purpose       : Simulation model to estimate expected loss under EIC insurance program    #
#         Author        : Milliman (with EIC changes)                                               #
#                                                                                                   #        
#####################################################################################################


#Needed Libraries: (MASS, dplyr and tictoc). If these libraries are not already 
#loaded used install.packages("package_name") 

#R Version Check
#This analysis was created using R version 3.5.2.
library(XLConnect)
library(openxlsx)
library(MASS)
library(dplyr)
library(tictoc)

#Step 1: Create Unemployment sheet from Previous Cohort Assumptions file
#Step 2: Set Directory to the folder with all the csv files
#Step 3: Create Output location inside folder with all the csv files
#Step 4: Make sure to change the Output location at the bottom
#Step 5: To change inputs go to line 117-122 


Directory <- getwd()
Unemployment <- read.csv(paste0(Directory,"/Unemployment_major_category.csv"), header = TRUE)
#Directory Loop
setwd(paste0(Directory,"/Rhode Island state schools"))
Schools <- Schools <- list.files(getwd(), pattern = ".csv")
for (i in 1:length(Schools)) {
  #Import csv's from backend
  backend <- read.csv(Schools[i], header = FALSE)
  
  #Fix Columns, Add grad rates, Merge Unemployment Data
  backend <- data.frame(backend)
  Unemployment <- data.frame(Unemployment)
  backend$School <- backend[1,1]
  backend$School <- gsub("Name: ","",backend$School)
  backend <- backend[-c(1:4),]
  #backend$Base_Wage_2018 <- 35000
  output <- merge(backend, Unemployment, by.x = 'V1', by.y = 'Major', all = TRUE)
  backend <- output[1:NROW(backend),]
  
  #Create Unemployment Rates for future years
  backend$Unemp_2019[is.na(backend$Unemp_2019)] <- .02
  backend$Unemp_2020[is.na(backend$Unemp_2020)]<- .025
  backend$Unemp_2021[is.na(backend$Unemp_2021)]<- .03
  Unemployment1 <- matrix(data = NA,NROW(backend), 9)
  colnames(Unemployment1) <- paste("Unemp_",2022:2030,sep="")
  backend_unemp <- cbind(backend[,18:20],Unemployment1)
  backend_unemp[,4:12] <- backend_unemp[,3]
  
  #Populate 12 year Median Salary Increment
  Median1 <- data.frame("Median_2019" = as.numeric(as.character(backend$V5))*1.03)
  for (i in 2:12) {
    Median1[,i] <- matrix(data = NA,NROW(backend),1)
    Median1[,i] <- Median1[, i-1] * 1.03
    names(Median1)[i] <- paste0("Median_",(2018 + i),sep="")
  }
  
  #Populate 12 year Base Wage Salary Increment
  Base_wage_1 <- data.frame("Base_Wage_2019" = as.numeric(as.character(backend$V15)) * 1.03)
  for (i in 2:12) {
    Base_wage_1[,i] <- matrix(data = NA,NROW(backend),1)
    Base_wage_1[,i] <- Base_wage_1[, i-1] * 1.03
    names(Base_wage_1)[i] <- paste0("Base_Wage_",(2018 + i),sep="")
  }
  
  #Create Cumulative Income growth
  backend$V6 <- (as.numeric(as.character(backend[,6])) + 1)
  for (i in 1:3) {
    backend[,6+i] <- (as.numeric(as.character(backend[,6+i])) + 1) * backend[,(5+i)]
  }
  
  #Create Enrollee Increment
  Enrollee1 <- data.frame("Enrollee_2019" = as.numeric(as.character(backend$V3)))
  Enrollee2 <- matrix(data = NA,NROW(backend),11)
  colnames(Enrollee2) <- paste("Enrollee_",2020:2030,sep="")
  Enrollee1 <- cbind(Enrollee1, Enrollee2)
  Enrollee1[,2:12] <- Enrollee1[,1]
  
  #Create final data frame
  assump <- data.frame("cohort" = 1:NROW(backend),
                       "school" = backend$School,
                       "major" = backend$V1,
                       "Num" = as.numeric(as.character(backend$V3)),
                       "grad_rate" = as.numeric(as.character(backend$V4)),
                       "grad_3" = as.numeric(as.character(backend$V11)),
                       "grad_4" = as.numeric(as.character(backend$V12)),
                       "grad_5" = as.numeric(as.character(backend$V13)),
                       "grad_6" = as.numeric(as.character(backend$V14)),
                       "median_2018" = as.numeric(as.character(backend$V5)),
                       "logsig" = as.numeric(as.character(backend$V10)),
                       "base_wage_2018" = backend$V15,
                       "Cum_Inc_Growth1" = backend$V6,
                       "Cum_Inc_Growth2" = backend$V7,
                       "Cum_Inc_Growth3" = backend$V8,
                       "Cum_Inc_Growth4" = backend$V9,
                       "2019_Median" = as.numeric(as.character(backend$V16)))
  
  #Bind Base Wage, Median, Enrollee, Unemployment to final data frame
  assump <- cbind(assump, backend_unemp, Median1, Base_wage_1, Enrollee1)
  
  #Scenario Name - For output file label. This name can be changed and the output can reflect the name change. 
  output_name <- noquote(paste0(assump[1,2]," Output",sep = ""))  # A name more descriptive can be used to help differentiate model runs
  output_name1 <- noquote(paste0(assump[1,2]))
  
  tic("Total_Runtime")
  #Seeding random generators
  seed_inc <- 123
  seed_unemp <- 456
  seed_grad <- 789
  trials <- 100000
  
  #Define correlation matrix
  rho <- matrix(c(1,.7,.6,.5,.4,.7,1,.7,.6,.5,.6,.7,1,.7,.6,.5,.6,.7,1,.7,.4,.5,.6,.7,1),5,5)
  
  #Inputs
  insured_level <- 1
  rng <- 1:NROW(assump)
  LsRatio <- .3
  num1 <- sum(assump[rng,4])
  tuition_cap <- 1000000000
  premium_cap <- 2000
  Base_Wage_Level <- 2000
  
  repeat {
    
    #Create output data frame
    fiveyrtotal <- data.frame("cohort" = numeric(),
                              "grad_yr" = numeric(),
                              "major" = character(),
                              "PY_enrollees" = numeric(),
                              "median" = numeric(),
                              "exp_loss" = numeric(),
                              "Loss_Calc" = numeric())
    
    #Loop through each cohort -- use 1:nrow(assump) if want everything)
    for (row in rng) {
      
      #Define cohort specific assumptions
      cohort <- assump[row,1]
      school <- as.character(assump[row,2])
      major <- as.character(assump[row,3])
      grad_rate <- assump[row,5]
      grad_3 <- assump[row, 6]
      grad_4 <- assump[row, 7]
      grad_5 <- assump[row, 8]
      grad_6 <- assump[row, 9]
      median_2018 <- assump[row,10]
      logsig <- assump[row,11]
      sigma <- diag(logsig,5,5)
      base_wage_2018 <- assump[row,12]
      
      #Define economomic wage and unemployment
      for (yr_add in 1:12){
        yr <- 2018 + yr_add
        
        assign(paste("unemp_",yr,sep=""),assump[row,17+yr_add]) 
        assign(paste("median_",yr,sep=""),assump[row,29+yr_add])
        assign(paste("base_wage_",yr,sep=""),assump[row,41+yr_add])
        assign(paste("enrollees_",yr,sep=""),assump[row,53+yr_add])  
      }
      
      #Define flat base wage
      if (assump[row,42]+Base_Wage_Level>=assump[row,17]) {
        base_wage_flat<-assump[row,17]-Base_Wage_Level
      } else {
        base_wage_flat<-assump[row,42]
      }
      
      #Convert to covariance matrix
      cov <- sigma %*% rho %*% sigma
      
      #Loop for each cohort year
      for (yr_add2 in 4:7){
        grad_yr <- 2018 + yr_add2
        #grad_yr1 <- ifelse(grad_yr >= 2022 & grad_yr <= 2025, 1, 0)
        
        
        ##Simulate income##
        
        #Define projected enrollees
        enrollees <- assump[row,53+yr_add2]
        
        
        #Define lognormal mu
        mu_1 <- assump[row,30+yr_add2]
        mu_2 <- assump[row,30+yr_add2]*assump[row,13]
        mu_3 <- assump[row,30+yr_add2]*assump[row,14]
        mu_4 <- assump[row,30+yr_add2]*assump[row,15]
        mu_5 <- assump[row,30+yr_add2]*assump[row,16]
        
        #Create matrix of lognormal mu
        logmu <- matrix(c(log(mu_1),log(mu_2),log(mu_3),log(mu_4),log(mu_5)),5,1)
        
        #Run multivariate normal distribution for 
        set.seed(seed_inc)
        fiveyrsim<-mvrnorm(n=trials,logmu,cov,empirical=TRUE)
        
        #Convert to dataframe
        fiveyrsimd <- as.data.frame(fiveyrsim)
        
        #Add column labels
        colnames(fiveyrsimd) <- c("Yr1","Yr2","Yr3","Yr4","Yr5")
        
        #Calculate incomes
        fiveyrsimd <- fiveyrsimd %>%
          dplyr::mutate(Yr1_Inc = exp(Yr1),
                        Yr2_Inc = exp(Yr2),
                        Yr3_Inc = exp(Yr3),
                        Yr4_Inc = exp(Yr4),
                        Yr5_Inc = exp(Yr5))
        
        #Add flat base wage to the data frame
        fiveyrsimd$base_wage_flat<-base_wage_flat
        
        ##Simulate unemployment##
        
        #Create matrix of lognormal mu
        normmu_un <- matrix(c(1,1,1,1,1),5,1)
        
        #Define uniform sigma
        normsig_un <- 1
        sigma_un <- diag(normsig_un,5,5)
        
        #Convert to covariance matrix
        cov_un <- sigma_un %*% rho %*% sigma_un
        
        #Run multivariate normal dist, confirm same n as income mvrnorm
        set.seed(seed_unemp)
        fiveyrsim_un<-mvrnorm(n=trials,normmu_un,cov_un,empirical=TRUE)
        
        #Convert to dataframe
        fiveyrsim_und <- as.data.frame(fiveyrsim_un)
        
        #Add column labels
        colnames(fiveyrsim_und) <- c("Yr1_un","Yr2_un","Yr3_un","Yr4_un","Yr5_un")
        
        #Convert to a number between 0 to 1
        fiveyrsim_und$uni_un_1 <- pnorm(fiveyrsim_und$Yr1_un,1,1)
        fiveyrsim_und$uni_un_2 <- pnorm(fiveyrsim_und$Yr2_un,1,1)
        fiveyrsim_und$uni_un_3 <- pnorm(fiveyrsim_und$Yr3_un,1,1)
        fiveyrsim_und$uni_un_4 <- pnorm(fiveyrsim_und$Yr4_un,1,1)
        fiveyrsim_und$uni_un_5 <- pnorm(fiveyrsim_und$Yr5_un,1,1)
        
        #Define unemployment rates
        fiveyrsim_und$un_1 <- assump[row,18+yr_add2]
        fiveyrsim_und$un_2 <- assump[row,19+yr_add2]
        fiveyrsim_und$un_3 <- assump[row,20+yr_add2]
        fiveyrsim_und$un_4 <- assump[row,21+yr_add2]
        fiveyrsim_und$un_5 <- assump[row,22+yr_add2]           
        
        #Calculate unemployment in each year
        fiveyrsim_und$un_flag_1 <- ifelse(fiveyrsim_und$uni_un_1<=fiveyrsim_und$un_1,0,1)
        fiveyrsim_und$un_flag_2 <- ifelse(fiveyrsim_und$uni_un_2<=fiveyrsim_und$un_2,0,1)
        fiveyrsim_und$un_flag_3 <- ifelse(fiveyrsim_und$uni_un_3<=fiveyrsim_und$un_3,0,1)
        fiveyrsim_und$un_flag_4 <- ifelse(fiveyrsim_und$uni_un_4<=fiveyrsim_und$un_4,0,1)
        fiveyrsim_und$un_flag_5 <- ifelse(fiveyrsim_und$uni_un_5<=fiveyrsim_und$un_5,0,1)
        
        #Append all unemployment data to random income records
        fiveyrsimd <- cbind(fiveyrsimd,fiveyrsim_und)
        
        ##Simulate graduation##   
        
        #Random number between 0 and 1, confirm n same as above simulations
        set.seed(seed_grad)
        fiveyrsimd$grad_rand <- runif(trials, min = 0, max=1)   
        
        #Flag if graduated
        fiveyrsimd$grad_flag <- ifelse(fiveyrsimd$grad_rand <= grad_rate,1,0)
        
        ##Calculate five year incomes##      
        
        #Define 5 year median
        fiveyrsimd$median_5yr <- assump[row,17]*5
        
        #Define 5 year insured income levels
        fiveyrsimd$insured_100 <- pmax(((fiveyrsimd$base_wage_flat + Base_Wage_Level)*5), fiveyrsimd$median_5yr*insured_level)      
        
        #Apply unemployment
        fiveyrsimd <- fiveyrsimd %>%
          dplyr::mutate(Yr1_Inc_un = Yr1_Inc*un_flag_1,
                        Yr2_Inc_un = Yr2_Inc*un_flag_2,
                        Yr3_Inc_un = Yr3_Inc*un_flag_3,
                        Yr4_Inc_un = Yr4_Inc*un_flag_4,
                        Yr5_Inc_un = Yr5_Inc*un_flag_5)
        
        
        #Calculate 5 year simulated income               
        fiveyrsimd <- fiveyrsimd %>%
          dplyr::mutate(income_5yr = ifelse(Yr1_Inc_un>base_wage_flat,Yr1_Inc_un,base_wage_flat)
                        +ifelse(Yr2_Inc_un>base_wage_flat,Yr2_Inc_un,base_wage_flat)
                        +ifelse(Yr3_Inc_un>base_wage_flat,Yr3_Inc_un,base_wage_flat)
                        +ifelse(Yr4_Inc_un>base_wage_flat,Yr4_Inc_un,base_wage_flat)
                        +ifelse(Yr5_Inc_un>base_wage_flat,Yr5_Inc_un,base_wage_flat))    
        
        ##Calculate expected loss##
        
        fiveyrsimd <- fiveyrsimd %>%
          dplyr::mutate(exp_loss_100 = ifelse(grad_flag==0,0,ifelse(income_5yr>insured_100,0,ifelse((insured_100-income_5yr)<tuition_cap,insured_100-income_5yr,tuition_cap))))     
        
        
        fiveyrtotallist1 <- list(
          "cohort" = cohort,
          "grad_yr" = grad_yr,
          "Major" = major,
          "Enrollees" = enrollees,
          "Median_2019" = mu_1,
          "exp_loss" = mean(fiveyrsimd$exp_loss_100),
          "Loss_Calc" = ifelse(grad_yr==2022, mean(fiveyrsimd$exp_loss_100)*grad_3,
                               ifelse(grad_yr==2023, mean(fiveyrsimd$exp_loss_100)*grad_4,
                                      ifelse(grad_yr==2024, mean(fiveyrsimd$exp_loss_100)*grad_5,
                                             ifelse(grad_yr==2025, mean(fiveyrsimd$exp_loss_100)*grad_6,0))))*enrollees/num1)
        
        
        #Add results to total data 
        fiveyrtotal <- dplyr::bind_rows(fiveyrtotal, fiveyrtotallist1)
        
      }#end grad year loop
    }#end cohort loop
    
    insured_level1 <- insured_level * 100
    names(fiveyrtotal)[6] <- paste("Exploss_",(insured_level1),sep="")
    names(fiveyrtotal)[7] <- paste("Losscalc_",(insured_level1),sep="")
    premium <- sum(fiveyrtotal[,7])/LsRatio
    print(noquote(paste(insured_level1,'% premium is:', premium,sep="")))
    
    #add either the whole thing or just columns 6 and 7 to the final dataframe
    if (insured_level == 1){
      fiveyrtotal_final <- fiveyrtotal
      Cveragechart <- c(paste0(output_name1),"","","")
      Cveragechart <- rbind(Cveragechart,c(noquote(paste('Total Number of Students',sep="")),"","",num1),c(noquote(paste('Premium',sep="")),"",noquote(paste(insured_level)),noquote(paste(round(premium)))))
    } else {
      fiveyrtotal_final <- cbind(fiveyrtotal_final,fiveyrtotal[,6:7])
      Cveragechart <- rbind(Cveragechart,c("","",noquote(paste(insured_level)),noquote(paste(round(premium)))))
    }
    
    #stop if premium is less than or equal to 2000
    if (premium <= premium_cap) {
      print(output_name)
      break
    }
    
    insured_level = insured_level - .05
    rm(fiveyrtotal)
    
  } #end repeat loop
  
  toc() #Total_Runtime
  
  #Export
  
  Pivottable <- select(fiveyrtotal_final,Major,Median_2019,Enrollees,starts_with("Losscalc_")) %>% group_by(Major) %>% summarise_each(funs(sum)) %>% mutate(Median_2019 = Median_2019 / 4, Enrollees = Enrollees / 4) %>% arrange(desc(Losscalc_100))
  Prmtable <- data.frame(X1=Cveragechart[,1],
                         X2=Cveragechart[,2],
                         X3=as.numeric(as.character(Cveragechart[,3])),
                         X4=as.numeric(as.character(Cveragechart[,4])))
  CoverageChart <- data.frame("Major" = as.character(assump[,3]),
                              "Students" = as.numeric(as.character(backend$V3)),
                              "Median_Salary_2019" = assump[,17],
                              "Coverage" = pmax(10000, (assump[,17]-assump[,42])*5))
  
  # Create a blank workbook
  OUTPUT <- createWorkbook()
  
  # Add some sheets to the workbook
  addWorksheet(OUTPUT,"Data") 
  addWorksheet(OUTPUT,"Pivot Table")
  addWorksheet(OUTPUT,"Coverage Chart")
  
  # Write the data to the sheets
  writeData(OUTPUT,sheet = "Data",x = fiveyrtotal_final)
  writeData(OUTPUT,sheet = "Pivot Table",x = Pivottable)
  writeData(OUTPUT,sheet = "Coverage Chart",x = Prmtable, colNames = FALSE)
  writeData(OUTPUT,sheet = "Coverage Chart",x = CoverageChart, startCol = 1, startRow = (NROW(Prmtable)+1), colNames = TRUE)
  
  # Reorder worksheets and merge cells and append data
  worksheetOrder(OUTPUT) <- c(1,2,3)
  mergeCells(OUTPUT,"Coverage Chart",cols = 1:4, rows = 1)
  mergeCells(OUTPUT,"Coverage Chart",cols = 1:3, rows = 2)
  mergeCells(OUTPUT,"Coverage Chart",cols = 1:2, rows = 3:NROW(Prmtable))
  cs <- createStyle(valign = "top")
  addStyle(OUTPUT,sheet = "Coverage Chart",row = 3,col = 1,cs)
  cs1 <- createStyle(numFmt = "$0,0")
  addStyle(OUTPUT,sheet = "Coverage Chart",rows = 3:NROW(Prmtable),col = 4,cs1)
  cs2 <- createStyle(numFmt = "0%")
  addStyle(OUTPUT,sheet = "Coverage Chart",rows = 3:NROW(Prmtable),col = 3,cs2)
  
  # Export the file
  saveWorkbook(OUTPUT, file = paste(Directory,"/Rhode Island state schools/Outputs/",output_name,".xlsx",sep=""))
}
setwd("..")