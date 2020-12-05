###################################################################################################################
#
# CROSS-SECTIONAL REGRESSION ON A SINGLE DATE 
# Code written by Charles Malafosse, 2020
# This is an example of cross-sectional regression implemented in R. For more information please check my article on medium.com
#
#
###################################################################################################################

library ("systemfit")
library ("splitstackshape")


################################################################################################################
#  Z-SCORE CALCULATION FUNCTION - FOR WEIGHTED MEAN OF 0 AND STDEV OF 1
################################################################################################################
zscoreweighted <- function (x, wgt, UpperBoundWinsorized, LowerBoundWinsorized) {
  ## Returns z-scored values
  x[x == 0] <- NA # Exposure kept to zero.
  x.mean <- weighted.mean(x,wgt,na.rm=TRUE)
  x.sd <- sd(x,na.rm=TRUE)
  x.z <- (x-x.mean) /x.sd
  x.z <- sapply(x.z, min, UpperBoundWinsorized)
  x.z <- sapply(x.z, max, LowerBoundWinsorized)
  x[is.na(x)] <- 0
  return(x.z)
}

  
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
# Z-scores are modified to be market cap weighted market neutral.
list_factors <- c('factor1','factor2','factor3','factor4','factor5')
for (istyle in list_factors) {
  print(istyle)
  zscores_values<- factor_data[,istyle]
  zscores_new_values<- factor_data[,istyle]
  ZscoresSum<- 0
  print(abs(weighted.mean(zscores_values, factor_data$wgt, na.rm=TRUE)))
  # ITERATE ZSCORE CALCULATION UNTIL THE ZSCORE DISTRIBUTION HAS A WEIGHTED MEAN OF 0 AND A STDEV OF 1
  while ( (abs(weighted.mean(zscores_values, factor_data$wgt ,na.rm=TRUE)) > 0.0001 | abs(sd(zscores_values,na.rm=TRUE)-1) > 0.0001 ) & ZscoresSum != abs(weighted.mean(zscores_values, factor_data$wgt,na.rm=TRUE))) {
    ZscoresSum <- abs(weighted.mean(zscores_values, factor_data$wgt,na.rm=TRUE))
    zscores_new_values <- zscoreweighted(zscores_values, factor_data$wgt, 3, -3)
    
    if (abs(weighted.mean(zscores_new_values, factor_data$wgt ,na.rm=TRUE) ) < abs(weighted.mean(zscores_values, factor_data$wgt, na.rm=TRUE))) {
      zscores_values = zscores_new_values
    }
    print(abs(weighted.mean(zscores_values, factor_data$wgt ,na.rm=TRUE)))
  }
  
  factor_data[,istyle] <- zscores_values
}

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





