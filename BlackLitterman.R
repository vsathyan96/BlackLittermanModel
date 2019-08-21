#Upload available information as CSV files
table1 <- read.csv(file = "Table1.csv", header = TRUE)
covmatrix <- data.frame(read.csv(file = "Covariance.csv", header = TRUE))
row.names(covmatrix) <- covmatrix[,1] #Set rownames from first column
covmatrix <- covmatrix[,-1] #Delete first column containing stock names
covmatrix <- as.matrix(covmatrix)
Pmatrix <- data.matrix(read.csv(file = "PMatrix.csv", header = FALSE))
MarketCapWeights <- data.matrix(read.csv(file = "MarketCapWeights.csv", header = FALSE)) #value*100 for percentage

delta =2.25 #Risk aversion parameter provided in Appendix B(2)
riskfree= 0.05 #Risk free rate of 5% provided

#Compute Implied Equilibrium Returns to plug into E[R]
ImpliedEqReturn = delta*(covmatrix%*%MarketCapWeights)
ImpliedEqReturn = (ImpliedEqReturn + riskfree)*100 #Factoring in risk free rate and converting to percent
#Compare Computed Implied Equilibrium Returns with Data provided in research paper
ImpliedEqAccuracy <- abs(ImpliedEqReturn-table1[,4])/((ImpliedEqReturn+table1[,4])/2)
colnames(ImpliedEqAccuracy) <- c("Percentage Difference (%)")
#Round off small percentages to 0
ImpliedEqAccuracy <- round(ImpliedEqAccuracy,4)

#Set View vector and Tau
Q <- matrix(c(10,3,1.5))
LC <- matrix(c(0.5,0.65,0.3))
CF= sum(Pmatrix%*%covmatrix%*%t(Pmatrix)*0.5) #Provided in Page 9

#Compute Omega Matrix
Omega <- matrix(0,nrow = 3, ncol = 3)
for (i in 1:3) {
Omega[i,i] <- matrix((1/LC[i,1])*CF)  
}

#Compute tau: Actual formula results in 3x3 matrix
#tau = Pmatrix%*%covmatrix%*%t(Pmatrix)/sum(Omega/3) 
#Professor advised to directly use tau value provided since there is no information on how to convert to scalar
tau = 0.873

#Table 2
table2 <- matrix(0,nrow = 30, ncol = 4)
rownames(table2) <- table1[,1]
#Compute Weights using formula in Appendix B(7): w = (δΣ)−1 μ
for (j in 1:3) {
   table2[,j] <- ((solve(delta*covmatrix)%*%(table1[,j+1]-(riskfree*100))))
}
table2[,4] <- cbind(MarketCapWeights*100)
colnames(table2) <- c("Historical Weight (%)","CAPM Weight (%)", "Implied Eq. Weight (%)", "Market Cap. Weight (%)")
#Round off data to 2 decimal places
table2 <- round(table2,2)

#Table 4
NewExpRet <- solve(solve(tau*covmatrix)+t(Pmatrix)%*%solve(Omega)%*%Pmatrix)%*%(solve(tau*covmatrix) %*%ImpliedEqReturn+t(Pmatrix)%*%solve(Omega)%*%Q)
ReturnDiff <- NewExpRet - ImpliedEqReturn
OmegaHat <- ((solve(delta*covmatrix)%*%(NewExpRet-(riskfree*100))))

#Compute Weights Difference
WeightDiff <- OmegaHat - (MarketCapWeights*100)

table4 <- cbind(NewExpRet, ImpliedEqReturn, ReturnDiff, OmegaHat, MarketCapWeights*100, WeightDiff)
colnames(table4) <- (c("New Return Vector E[R]", "Implied Eq. Vector", "Return Diff.", "New Weight", "Market Cap. Weight", "Weight Diff."))
table4 <- round(table4,2)

#Comparative Analysis
OfficialWeightDiff <- c(0,-0.85,0.99,0,0,0.05,0,-0.99,0,-0.25,0,0,0,0,0,0,0,0,0,0,0,0,0,0.51,0,0,0.77,0.53,0,0)
ComparativeAnalysis <- (OfficialWeightDiff-WeightDiff)
colnames(ComparativeAnalysis) <- c("Deviation from Official Percentage Weights")
