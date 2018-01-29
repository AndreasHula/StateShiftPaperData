#load core data: investments and repayments
#load model parameters for final and intermediary models
#minimal simplification of the intermediary parameters:
#level 0 trustee only included in final model (no influence on model selection 
#or testing)
library(fields)
library(tikzDevice)
library(timeSeries)
library(extRemes)
library(perm);
library(stats);
library(coin);
library(plyr);
library(knitr);
load("PCBGameDataAndFinalParams.RData");
#Process investments to discrete grid on 2 scales (0-4 for code processing)
#saved in "data"
OldModelLikelihoodInvestor = Holder$OInvestorLikelihood
OldModelLikelihoodTrustee = Holder$OTrusteeLikelihood
AversionModelLikelihoodInvestor = Holder$AvInvestorLikelihood
AversionModelLikelihoodTrustee = Holder$AvTrusteeLikelihood

source("GeneratedOld.R");
source("GeneratedAverse.R");
source("GeneratedFlexible.R");
Investor = as.matrix(GameData[,1:10]);
Trustee = as.matrix(GameData[,11:20]);
InvestorWithBPD = Investor[1:55,];
InvestorWithHC = Investor[56:93,];
BPDTrustee =  Trustee[1:55,];
HCTrustee = Trustee[56:93,];
NewInvestor = round(Investor/5);
NewTrustee = round(6*Trustee/(3*5*NewInvestor)); 
NewTrustee[is.na(NewTrustee)] = 0;
NewTrustee[is.infinite(NewTrustee)] = 0;
NewTrustee[NewTrustee > 4]=4;
FracInvestorWithBPD = 1/4*NewInvestor[1:55,];
FracInvestorWithHC = 1/4*NewInvestor[56:93,];
FracBPDTrustee =  1/6*NewTrustee[1:55,];
FracHCTrustee = 1/6*NewTrustee[56:93,];
FracInvestor = 1/4*NewInvestor
FracTrustee =  1/6*NewTrustee

## Parameter separation
HCTrusteeIrritability = FinalParams$Data.FTrusteeIrritability[56:93];
BPDTrusteeIrritability = FinalParams$Data.FTrusteeIrritability[1:55];
HCTrusteeShift = FinalParams$Data.FTrusteeShift[56:93];
BPDTrusteeShift = FinalParams$Data.FTrusteeShift[1:55];
HCTrusteeAversion = FinalParams$Data.FTrusteeAversion[56:93];
BPDTrusteeAversion = FinalParams$Data.FTrusteeAversion[1:55];
HCTrusteeToM = FinalParams$Data.FTrusteeToM[56:93];
BPDTrusteeToM = FinalParams$Data.FTrusteeToM[1:55];
Guilt = FinalParams$Data.FTrusteeGuilt
#Guilt = Guilt*(0.1*Guilt+0.3);
HCTrusteeGuilt = Guilt[56:93];
BPDTrusteeGuilt = Guilt[1:55];
HCTrusteePlan = FinalParams$Data.FTrusteePlan[56:93];
BPDTrusteePlan = FinalParams$Data.FTrusteePlan[1:55];
HCTrusteeITemp = 1/FinalParams$Data.FTrusteeTemp[56:93];
BPDTrusteeITemp = 1/FinalParams$Data.FTrusteeTemp[1:55];
##Trustee Setup-complete

HCInvestorIrritability = FinalParams$Data.FInvestorIrritability[56:93];
BPDInvestorIrritability = FinalParams$Data.FInvestorIrritability[1:55];
HCInvestorShift = FinalParams$Data.FInvestorShift[56:93];
BPDInvestorShift = FinalParams$Data.FInvestorShift[1:55];
HCInvestorAversion =FinalParams$Data.FInvestorAversion[56:93] #1.0+0.2*Data$FInvestorAversion[56:93];
BPDInvestorAversion =FinalParams$Data.FInvestorAversion[1:55] #1.0+0.2*Data$FInvestorAversion[1:55];
HCInvestorToM = FinalParams$Data.FInvestorToM[56:93];
BPDInvestorToM = FinalParams$Data.FInvestorToM[1:55];
Guilt = FinalParams$Data.FInvestorGuilt
#Guilt = Guilt*(0.1*Guilt+0.3);
HCInvestorGuilt = Guilt[56:93];
BPDInvestorGuilt = Guilt[1:55];
HCInvestorPlan = FinalParams$Data.FInvestorPlan[56:93];
BPDInvestorPlan = FinalParams$Data.FInvestorPlan[1:55];
HCInvestorITemp = 1/FinalParams$Data.FInvestorTemp[56:93];
BPDInvestorITemp = 1/FinalParams$Data.FInvestorTemp[1:55];

##Investor Setup-Complete

TrusteeAversion = FinalParams$Data.FTrusteeAversion;
InvestorAversion = FinalParams$Data.FInvestorAversion;

##Parameter Tests
#Permutation two-sided mean comparisons 
#p_investor_ToM = permTS(HCInvestorToM, BPDInvestorToM, alternative = "two.sided")$p.value
#p_trustee_ToM = permTS(HCTrusteeToM, BPDTrusteeToM, alternative = "two.sided")$p.value
p_investor_plan = permTS(HCInvestorPlan, BPDInvestorPlan, alternative = "two.sided")$p.value
#p_trustee_plan = permTS(HCTrusteePlan, BPDTrusteePlan, alternative = "two.sided")$p.value
#p_investor_guilt = permTS(HCInvestorGuilt, BPDInvestorGuilt, alternative = "two.sided")$p.value
p_trustee_guilt = permTS(HCTrusteeGuilt, BPDTrusteeGuilt, alternative = "two.sided")$p.value
#p_investor_Aversion = permTS(HCInvestorAversion, BPDInvestorAversion, alternative = "two.sided")$p.value
#p_trustee_Aversion = permTS(HCTrusteeAversion, BPDTrusteeAversion, alternative = "two.sided")$p.value
#p_investor_Temp = permTS(HCInvestorITemp, BPDInvestorITemp, alternative = "two.sided")$p.value
#p_trustee_Temp = permTS(HCTrusteeITemp, BPDTrusteeITemp, alternative = "two.sided")$p.value
p_investor_Irritability = permTS(HCInvestorIrritability, BPDInvestorIrritability, alternative = "two.sided")$p.value
p_trustee_Irritability = permTS(HCTrusteeIrritability, BPDTrusteeIrritability, alternative = "two.sided")$p.value
p_investor_Shift = permTS(HCInvestorShift, BPDInvestorShift, alternative = "two.sided")$p.value
p_trustee_Shift = permTS(HCTrusteeShift, BPDTrusteeShift, alternative = "two.sided")$p.value
#specific comparisons of proportions
GuiltProportion = prop.test(c(sum(BPDTrusteeGuilt == 0) , sum(HCTrusteeGuilt == 0) ), 
                            c(length(BPDTrusteeGuilt), length(HCTrusteeGuilt)), alternative = "two.sided", correct = FALSE)$p.value
AwarenessProportion = prop.test(c(sum(BPDTrusteeShift == 0) , sum(HCTrusteeShift == 0) ), 
                                c(length(BPDTrusteeShift), length(HCTrusteeShift)), alternative = "two.sided", correct = FALSE)$p.value
PerilousProportion = prop.test(c(sum(BPDTrusteeShift == 0 | BPDTrusteeGuilt == 0) , sum(HCTrusteeShift == 0 | HCTrusteeGuilt == 0) ),
                               + c(length(BPDTrusteeShift), length(HCTrusteeShift)), alternative = "two.sided", correct = FALSE)$p.value

##Investor-Trustee Groupings
InvestorHCwithPerilous = FracInvestorWithHC[HCTrusteeShift == 0 | HCTrusteeGuilt == 0,]
InvestorBPDwithPerilous = FracInvestorWithBPD[BPDTrusteeShift == 0 | BPDTrusteeGuilt == 0,]
TrusteeHCPerilous = FracHCTrustee[HCTrusteeShift == 0 | HCTrusteeGuilt == 0,]
TrusteeBPDPerilous = FracBPDTrustee[BPDTrusteeShift == 0 | BPDTrusteeGuilt == 0,]

InvestorHCwithNonPerilous = FracInvestorWithHC[HCTrusteeShift > 0 & HCTrusteeGuilt > 0,]
InvestorBPDwithNonPerilous = FracInvestorWithBPD[BPDTrusteeShift > 0 & BPDTrusteeGuilt > 0,]
TrusteeHCNonPerilous = FracHCTrustee[HCTrusteeShift > 0 & HCTrusteeGuilt > 0,]
TrusteeBPDNonPerilous = FracBPDTrustee[BPDTrusteeShift > 0 & BPDTrusteeGuilt > 0,]

InvestorAverse = FracInvestor[InvestorAversion > 1.0,]
InvestorNonAverse = FracInvestor[InvestorAversion < 1.2,]
TrusteeWithAverse = FracTrustee[InvestorAversion > 1.0,]
TrusteeWithNonAverse = FracTrustee[InvestorAversion < 1.2,]

I_Investments = vector("double",10);
for(i in 1:10){
  I_Investments[i] =  permTS(FracInvestorWithHC[,i], FracInvestorWithBPD[,i], alternative = "two.sided")$p.value;
}
T_Repayments = vector("double",10)
for(i in 1:10){
  T_Repayments[i] =  permTS(FracBPDTrustee[,i], FracHCTrustee[,i], alternative = "two.sided")$p.value;
}
Non_Perilous_Investments = vector("double",10);
for(i in 1:10){
  Non_Perilous_Investments[i] =  t.test(InvestorBPDwithNonPerilous[,i], InvestorHCwithNonPerilous[,i], alternative = "two.sided")$p.value;
}
Perilous_Investments = vector("double",10);
for(i in 1:10){
  Perilous_Investments[i] =  t.test(InvestorBPDwithPerilous[,i], InvestorHCwithPerilous[,i], alternative = "two.sided")$p.value;
}
Perilous_Repayments = vector("double",10)
for(i in 1:10){
  Perilous_Repayments[i] =  t.test(TrusteeBPDPerilous[,i], TrusteeHCPerilous[,i], alternative = "two.sided")$p.value;
}
Non_Perilous_Repayments = vector("double",10)
for(i in 1:10){
  Non_Perilous_Repayments[i] =  t.test(TrusteeBPDNonPerilous[,i], TrusteeHCNonPerilous[,i], alternative = "two.sided")$p.value;
}
SumInvestmentsHC = rowSums(InvestorWithHC);
SumInvestmentsBPD = rowSums(InvestorWithBPD);
SumRepaymentsHC = rowSums(HCTrustee);
SumRepaymentsBPD = rowSums(BPDTrustee);
#SignificantProportions = prop.test(Data$)

### Generative Models, Reproduction
n = 200;
fid =file(paste0("FlexibleFullCurve",".bin"),"rb"); 
FinalInvestor = matrix(0, n, 10);
FinalTrustee = matrix(0, n, 10);
for( t in 1:10){ 
  for( i in 1:n){
    FinalInvestor[i,t]=readBin(fid,integer(),endian = "little");
  }
  for( i in 1:n){
    FinalTrustee[i,t]=readBin(fid,integer(),endian = "little");
  }
}

close(fid);

fid =file(paste0("AvBreak29",".bin"),"rb"); 
AversionInvestor = matrix(0, n, 10);
AversionTrustee = matrix(0, n, 10);
for( t in 1:10){ 
  for( i in 1:n){
    AversionInvestor[i,t]=readBin(fid,integer(),endian = "little");
  }
  for( i in 1:n){
    AversionTrustee[i,t]=readBin(fid,integer(),endian = "little");
  }
}

close(fid);

fid =file(paste0("OBreak29",".bin"),"rb"); 
ClassicInvestor = matrix(0, n, 10);
ClassicTrustee = matrix(0, n, 10);
for( t in 1:10){ 
  for( i in 1:n){
    ClassicInvestor[i,t]=readBin(fid,integer(),endian = "little");
  }
  for( i in 1:n){
    ClassicTrustee[i,t]=readBin(fid,integer(),endian = "little");
  }
}

close(fid);

ZeroInvestmentsStep4Final = sum(FinalInvestor[,4]==0)/200;
ZeroInvestmentsStep4Aversion = sum(AversionInvestor[,4]==0)/200;
ZeroInvestmentsStep4Classic = sum(ClassicInvestor[,4]==0)/200;
ZeroInvestmentsStep7Final = sum(FinalInvestor[,7]==0)/200;
ZeroInvestmentsStep7Aversion = sum(AversionInvestor[,7]==0)/200;
ZeroInvestmentsStep7Classic = sum(ClassicInvestor[,7]==0)/200;

p_value_Trustee_Aversion = lr.test(sum(Holder$OTrusteeLikelihood),sum(Holder$AvTrusteeLikelihood), df = 2*93)$p.value
p_value_Trustee_Full_Model = lr.test(sum(Holder$AvTrusteeLikelihood),sum(FinalParams$Data.FTrusteeLikelihoods), df = 2*93)$p.value
p_value_Investor_Aversion = lr.test(sum(Holder$OInvestorLikelihood),sum(Holder$AvInvestorLikelihood), df = 2*93)$p.value
p_value_Investor_Full_Model = lr.test(sum(Holder$AvInvestorLikelihood),sum(FinalParams$Data.FInvestorLikelihood), df = 2*93)$p.value

#mean Likelihoods & BIC block
MeanOldInvestor = mean(OldModelLikelihoodInvestor) 
MeanOldTrustee = mean(OldModelLikelihoodTrustee) 
MeanRiskInvestor = mean(AversionModelLikelihoodInvestor) 
MeanRiskTrustee = mean(AversionModelLikelihoodTrustee)
MeanFinal = mean(FinalParams$Data.FInvestorLikelihood)
MeanFinalTrust = mean(FinalParams$Data.FTrusteeLikelihood)
ProbabilityOldInvestor = exp(-mean(OldModelLikelihoodInvestor)/10) 
ProbabilityOldTrustee = exp(-mean(OldModelLikelihoodTrustee)/10) 
ProbabilityRiskInvestor = exp(-mean(AversionModelLikelihoodInvestor)/10) 
ProbabilityRiskTrustee = exp(-mean(AversionModelLikelihoodTrustee)/10)
ProbabilityFinal = exp(-mean(FinalParams$Data.FInvestorLikelihood)/10)
ProbabilityFinalTrust = exp(-mean(FinalParams$Data.FTrusteeLikelihood)/10)
InvestorReferenceBreakProbabilityOld = exp(-OldModelLikelihoodInvestor[29]/10)
TrusteeReferenceBreakProbabilityOld = exp(-OldModelLikelihoodTrustee[29]/10)
InvestorReferenceBreakProbabilityAverse = exp(-AversionModelLikelihoodInvestor[29]/10)
TrusteeReferenceBreakProbabilityAverse = exp(-AversionModelLikelihoodTrustee[29]/10)
InvestorReferenceBreakProbabilityFinal = exp(-FinalParams$Data.FInvestorLikelihood[29]/10)
TrusteeReferenceBreakProbabilityFinal = exp(-FinalParams$Data.FTrusteeLikelihood[29]/10)
InvestorReferenceBreakNLLOld = OldModelLikelihoodInvestor[29]
TrusteeReferenceBreakNLLOld = OldModelLikelihoodTrustee[29]
InvestorReferenceBreakNLLAverse = AversionModelLikelihoodInvestor[29]
TrusteeReferenceBreakNLLAverse = AversionModelLikelihoodTrustee[29]
InvestorReferenceBreakNLLFinal = FinalParams$Data.FInvestorLikelihood[29]
TrusteeReferenceBreakNLLFinal = FinalParams$Data.FTrusteeLikelihood[29]
#SumOldInvestor = sum(OldModelLikelihoodInvestor) 
#SumOldTrustee = sum(OldModelLikelihoodTrustee) 
#SumRiskInvestor = sum(AversionModelLikelihoodInvestor) 
#SumRiskTrustee = sum(AversionModelLikelihoodTrustee)
#SumFinal = sum(FinalParams$Data.FInvestorLikelihood)
#SumFinalTrust = sum(FinalParams$Data.FTrusteeLikelihood)

Correction = log(10)-log(2*pi)
MeanOldInvestorBIC = mean(2*OldModelLikelihoodInvestor + 3*Correction) 
MeanOldTrusteeBIC = mean(2*OldModelLikelihoodTrustee + 3*Correction) 
MeanRiskInvestorBIC = mean(2*AversionModelLikelihoodInvestor+5*Correction) 
MeanRiskTrusteeBIC = mean(2*AversionModelLikelihoodTrustee+5*Correction)
MeanFinalBIC = mean(2*FinalParams$Data.FInvestorLikelihood+7*Correction)
MeanFinalTrustBIC = mean(2*FinalParams$Data.FTrusteeLikelihood+7*Correction)
SumOldInvestorBIC = sum(2*OldModelLikelihoodInvestor + 3*Correction) 
SumOldTrusteeBIC = sum(2*OldModelLikelihoodTrustee + 3*Correction) 
SumRiskInvestorBIC = sum(2*AversionModelLikelihoodInvestor+5*Correction) 
SumRiskTrusteeBIC = sum(2*AversionModelLikelihoodTrustee+5*Correction)
SumFinalBIC = sum(2*FinalParams$Data.FInvestorLikelihood+7*Correction)
SumFinalTrustBIC = sum(2*FinalParams$Data.FTrusteeLikelihood+7*Correction)
 #Generated reproductions of the reference 
#example breaking interaction - Interaction pair 29 in the human data
source("Figure1LowerPart.R")
source("FiatRepairShift.R")
#Choose an illustrative example: Repair trajectory #10
source("BreakRepairShift.R")
# Break trajectory #1
source("FinFig2.R")
source("FinalFigure3.R")
source("FinalFig4.R")
print("Outputting results in order of appearance in the Manuscript:")
Sys.sleep(1)
source("Outputs.R")