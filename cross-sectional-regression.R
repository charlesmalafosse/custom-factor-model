###################################################################################################################
#
#
# Code written by Charles Malafosse, 2020
# This is an example of cross-sectional regression implemented in R. For more information please check my article on medium.com
#
#
###################################################################################################################

library ("systemfit")
library ("splitstackshape")


################################################################################################################
#
#  CODE FOR CROSS-SECTIONAL REGRESSION ON A SINGLE DATE 
#
################################################################################################################


#################################################################
#  LOAD THE DATA
#################################################################
# EXPOSURE DATA
data_filename = "data.csv"
factor_data <- data.frame(read.csv(data_filename))
factornames <- colnames(factor_data[ ,!(colnames(factor_data) %in% c("X", "total_return_1d", "wgt"))]) 
# MATRIX DATA
constraints_matrix_filename = "constraints_matrix.csv" 
constraints_matrix <- data.frame(read.csv(constraints_matrix_filename))
constraints_matrix <-  as.matrix(constraints_matrix[ ,!(colnames(constraints_matrix) %in% c("X"))])


#################################################################
#  FORMAT THE ZSCORES TO CREATE STANDARDIZED NORMAL ZSCORES
#################################################################
# (ALREADY DONE IN FILE EXAMPLES) CREATE Z-SCORES OF Z-SCORES UNTIL THE Z-SCORE MEAN GET CLOSE TO 0


#################################################################
#  FORMAT THE DATA TO APPLY DIFFERENT WEIGHTS ON EACH EQUATION
#  ADD MANY TIMES OVER THE SAME ROWS TO CREATE A WEIGHTED SUR REGRESSION
#################################################################
factor_data$wgt = round(factor_data$wgt * 100000)
factor_data_for_fit = expandRows(factor_data, "wgt") # Column wgt contains square root of market cap weights
factor_data_for_fit <-  factor_data_for_fit[ ,!(colnames(factor_data_for_fit) %in% c("X", "Intercept", "wgt"))] 


#################################################################
# BUILD FORMULA
#################################################################
formulaSectorStyleRegression <- as.formula(paste("total_return_1d ~ 1 + ", paste(colnames(factor_data[ ,!(colnames(factor_data) %in% c("X","Intercept", "total_return_1d", "wgt"))]), collapse= "+")))


#################################################################
#  PERFORM CROSS SECTIONAL REGRESSION WITH SYSTEMFIT (SUR:  Seemingly Unrelated Regressions)
#  https://cran.r-project.org/web/packages/systemfit/systemfit.pdf
#################################################################
CrossSectionalFit <- systemfit(formulaSectorStyleRegression, "SUR", data=factor_data_for_fit, restrict.matrix=constraints_matrix, 
                               pooled = TRUE, methodResidCov ="noDfCor", residCovWeighted = TRUE )


#################################################################
#  REGRESSION RESULT
#################################################################
CrossSectionalFitAtDate <- coef(summary( CrossSectionalFit ))


#################################################################
#  FACTOR IMPLICIT RETURN AND FACTOR STATS
#################################################################
FactorModelResults <- data.frame(factornames)
i_factor<- 1
for(i_factor in 1:length(factornames)){
  
  if (factornames[i_factor]=="Intercept") {
     FactorModelResults[i_factor,"FactorReturn"] <- CrossSectionalFitAtDate ["eq1_(Intercept)", "Estimate"] 
     FactorModelResults[i_factor,"TStat"] <- CrossSectionalFitAtDate ["eq1_(Intercept)", "t value"] 
     FactorModelResults[i_factor,"PValue"] <- CrossSectionalFitAtDate ["eq1_(Intercept)", "Pr(>|t|)"] 
     FactorModelResults[i_factor,"Standard Error"] <- CrossSectionalFitAtDate ["eq1_(Intercept)", "Std. Error"]
  } else {
     FactorModelResults[i_factor,"FactorReturn"] <- CrossSectionalFitAtDate [paste("eq1","_", factornames[i_factor],sep=""), "Estimate"] 
     FactorModelResults[i_factor,"TStat"] <- CrossSectionalFitAtDate [paste("eq1","_", factornames[i_factor],sep=""), "t value"] 
     FactorModelResults[i_factor,"PValue"] <- CrossSectionalFitAtDate [paste("eq1","_", factornames[i_factor],sep=""), "Pr(>|t|)"] 
     FactorModelResults[i_factor,"Standard Error"] <- CrossSectionalFitAtDate [paste("eq1","_", factornames[i_factor],sep=""), "Std. Error"]
  }
  
}


#################################################################
#  R-SQUARED OF MODEL
#################################################################
RsqData <- data.frame(c(summary( CrossSectionalFit$eq[[1]])$r.squared),c(summary( CrossSectionalFit$eq[[1]])$adj.r.squared),c(summary( CrossSectionalFit$eq[[1]])$sigma))
colnames(RsqData) <- c("R2","AdjR2","EstimatedStandardErrorOfResiduals")





