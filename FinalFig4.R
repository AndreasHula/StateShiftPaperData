library(tikzDevice)
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
    "\\usepackage{amssymb}"))
tikz("Figure4.tex", width = 6, height = 6, standAlone = TRUE,
    packages = c("\\usepackage{tikz}",
                 "\\usepackage[active,tightpage,psfixbb]{preview}",
                 "\\PreviewEnvironment{pgfpicture}",
                 "\\setlength\\PreviewBorder{0pt}",
                 "\\usepackage{amssymb}"))

par(mar=c(1,1,1,1)+0.1, mai = c(0.5,0.5,0.5,0.5), #oma = c(3,3,3,3),
cex = 1, cex.lab = 1, cex.axis =1, cex.main = 1)#, cex.text = 1)
layout(matrix(c(1,3, 2,4), 2, 2))

	Value2 = matrix(c( colMeans(TrusteeBPDPerilous), # red
colMeans(TrusteeHCPerilous), #brown
colMeans(InvestorBPDwithPerilous), # blue
colMeans(InvestorHCwithPerilous) #turq
),10,4)



	Value5 = matrix(c(
	  1/sqrt(sum(BPDTrusteeShift == 0 | BPDTrusteeGuilt == 0))*colSds(TrusteeBPDPerilous), # red
	  1/sqrt(sum(HCTrusteeShift == 0 | HCTrusteeGuilt == 0))*colSds(TrusteeHCPerilous), #brown
	  1/sqrt(sum(BPDTrusteeShift == 0 | BPDTrusteeGuilt == 0))*colSds(InvestorBPDwithPerilous), # blue
	  1/sqrt(sum(HCTrusteeShift == 0 | HCTrusteeGuilt == 0))*colSds(InvestorHCwithPerilous) #turq
	  ),10,4)

	matplot(array(c((1:10),(1:10),(1:10), (1:10) ), c(10,4) ), 
	main= "A)  Average Exchanges with perilous Trustees   ",  
	cex.main = 0.8, 
	array(c(Value2[,1], Value2[,2], Value2[,3], Value2[,4]),c(10,4)),
	axes = TRUE,
	xlab = ' ', 
	ylab = ' ', 
	xaxt = 'n',
	xlim = c(1,10), xaxp = c(1,10,9),
	 ylim=c(0,1),yaxp=c(0,1,4),
	frame.plot = FALSE, 
	type =c("l", "l", "l", "l"), lwd =2, lty = c(1,1,1,1), 
	col = c("red","coral","blue", "lightblue"), cex.axis = 1 , cex.lab = 1);
	arrows((1:10), Value2[,1], (1:10), Value2[,1]+Value5[,1], 0.01, 90);
	arrows((1:10), Value2[,1], (1:10), Value2[,1]-Value5[,1], 0.01, 90);
	arrows((1:10), Value2[,2], (1:10), Value2[,2]+Value5[,2], 0.01, 90);
	arrows((1:10), Value2[,2], (1:10), Value2[,2]-Value5[,2], 0.01, 90);
	arrows((1:10), Value2[,3], (1:10), Value2[,3]+Value5[,3], 0.01, 90);
	arrows((1:10), Value2[,3], (1:10), Value2[,3]-Value5[,3], 0.01, 90);
	arrows((1:10), Value2[,4], (1:10), Value2[,4]+Value5[,4], 0.01, 90);
	arrows((1:10), Value2[,4], (1:10), Value2[,4]-Value5[,4], 0.01, 90);
	legend(x="topright", 2, c("BPD paired Investor", "HC paired Investor", 
"BPD Trustee perilous", "HC Trustee perilous"),
       lty = c(1,1,1,1), col = 
c("blue", "lightblue","red","coral"),
	 bty = 'n', cex = 0.7)
	mtext(text = " Fraction sent ", side = 2, cex = 0.7, line = 2.2)
	mtext(text = " At Step ", side = 1, cex = 0.7, line = 2.2)
	axis(1, at= (1:10),labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), col.axis="black")




	Value9 = matrix(c( colMeans(TrusteeBPDNonPerilous), # red
	                   colMeans(TrusteeHCNonPerilous), #brown
	                   colMeans(InvestorBPDwithNonPerilous), # blue
	                   colMeans(InvestorHCwithNonPerilous) #turq
	),10,4)
	
	
	
	Value7 = matrix(c(
	  1/sqrt(sum(BPDTrusteeShift > 0 & BPDTrusteeGuilt > 0))*colSds(TrusteeBPDNonPerilous), # red
	  1/sqrt(sum(HCTrusteeShift > 0 & HCTrusteeGuilt > 0))*colSds(TrusteeHCNonPerilous), #brown
	  1/sqrt(sum(BPDTrusteeShift > 0 & BPDTrusteeGuilt > 0))*colSds(InvestorBPDwithNonPerilous), # blue
	  1/sqrt(sum(HCTrusteeShift > 0 & HCTrusteeGuilt > 0))*colSds(InvestorHCwithNonPerilous) #turq
	),10,4)

	matplot(array(c((1:10),(1:10),(1:10), (1:10) ), c(10,4) ), 
	main= "B)  Average Exchanges with unperilous Trustees   ",  
	cex.main = 0.8, 
	array(c(Value9[,1], Value9[,2], Value9[,3], Value9[,4]),c(10,4)),
	axes = TRUE,
	xlab = ' ', 
	ylab = ' ', 
	xaxt = 'n',
	#xlab = 'At Step', 
	#ylab = 'Fraction sent', 
	xlim = c(1,10), xaxp = c(1,10,9),
	 ylim=c(0,1),yaxp=c(0,1,4),
	#outer = TRUE,
	frame.plot = FALSE, 
	#legend(1,2, c("LH", "HL")),
	# legend.text = c("LH", "HL"),
	# args.legend = list(x="topright", bty = 'n', cex = 2.5),
	type =c("l", "l", "l", "l"), lwd =2, lty = c(1,1,1,1), #line = 8,
	col = c("red","coral","blue", "lightblue"), cex.axis = 1 , cex.lab = 1);
	arrows((1:10), Value9[,1], (1:10), Value9[,1]+Value7[,1], 0.01, 90);
	arrows((1:10), Value9[,1], (1:10), Value9[,1]-Value7[,1], 0.01, 90);
	arrows((1:10), Value9[,2], (1:10), Value9[,2]+Value7[,2], 0.01, 90);
	arrows((1:10), Value9[,2], (1:10), Value9[,2]-Value7[,2], 0.01, 90);
	arrows((1:10), Value9[,3], (1:10), Value9[,3]+Value7[,3], 0.01, 90);
	arrows((1:10), Value9[,3], (1:10), Value9[,3]-Value7[,3], 0.01, 90);
	arrows((1:10), Value9[,4], (1:10), Value9[,4]+Value7[,4], 0.01, 90);
	arrows((1:10), Value9[,4], (1:10), Value9[,4]-Value7[,4], 0.01, 90);
	#legend(1,2, text= c("LH", "HL"), x="topright", bty = 'n', cex = 1.8)
	legend(x="topright", 2, c("BPD paired Investor", 
"HC paired Investor", 
"BPD Trustee unperilous", "HC Trustee unperilous"),
       lty = c(1,1,1,1), col = 
c("blue", "lightblue","red","coral"),
	 bty = 'n', cex = 0.7)
	mtext(text = " Fraction sent ", side = 2, cex = 0.7, line = 2.2)
	mtext(text = " At Step ", side = 1, cex = 0.7, line = 2.2)
	axis(1, at= (1:10),labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), col.axis="black")



	Value2 = matrix(c( colMeans(FracTrustee[InvestorAversion<1.2,]) # red
,
colMeans(FracTrustee[InvestorAversion>1.0,]), #brown
colMeans(FracInvestor[InvestorAversion<1.2,]), # blue
         colMeans(FracInvestor[InvestorAversion>1.0,]) #turq
),10,4)



	Value5 = matrix(c(
	  1/sqrt(sum(InvestorAversion<1.2))*colSds(FracTrustee[InvestorAversion<1.2,]),
  1/sqrt(sum(InvestorAversion>1.0))*colSds(FracTrustee[InvestorAversion>1.0,]),
  1/sqrt(sum(InvestorAversion<1.2))*colSds(FracInvestor[InvestorAversion<1.2,]),
  1/sqrt(sum(InvestorAversion>1.0))*colSds(FracInvestor[InvestorAversion>1.0,]))
 ,
  10,4)

	matplot(array(c((1:10),(1:10),(1:10), (1:10) ),c(10,4) ), 
	main= "C)  Average Exchanges of different Risk Aversion Populations,      
$\\omega^I \\leq 1.0$ vs $\\omega^I \\geq 1.2$",  
	cex.main = 0.8, 
	array(c(Value2[,1], Value2[,2], Value2[,3], Value2[,4]),c(10,4)),
	axes = TRUE,
	xlab = ' ', 
	ylab = ' ', 
	xaxt = 'n',
	#xlab = 'At Step', 
	#ylab = 'Fraction sent', 
	xlim = c(1,10), xaxp = c(1,10,9),
	 ylim=c(0,1),yaxp=c(0,1,4),
	frame.plot = FALSE, 
	type =c("l", "l", "l", "l"), lwd =2, lty = c(1,1,1,1), #line = 8,
	col = c("red","coral","blue", "lightblue"), cex.axis = 1 , cex.lab = 1);
	arrows((1:10), Value2[,1], (1:10), Value2[,1]+Value5[,1], 0.01, 90);
	arrows((1:10), Value2[,1], (1:10), Value2[,1]-Value5[,1], 0.01, 90);
	arrows((1:10), Value2[,2], (1:10), Value2[,2]+Value5[,2], 0.01, 90);
	arrows((1:10), Value2[,2], (1:10), Value2[,2]-Value5[,2], 0.01, 90);
	arrows((1:10), Value2[,3], (1:10), Value2[,3]+Value5[,3], 0.01, 90);
	arrows((1:10), Value2[,3], (1:10), Value2[,3]-Value5[,3], 0.01, 90);
	arrows((1:10), Value2[,4], (1:10), Value2[,4]+Value5[,4], 0.01, 90);
	arrows((1:10), Value2[,4], (1:10), Value2[,4]-Value5[,4], 0.01, 90);
	#legend(1,2, text= c("LH", "HL"), x="topright", bty = 'n', cex = 1.8)
	legend(x="topright", 2, c("Investors, $\\omega^I \\leq 1.0$", "Investors, $\\omega^I \\geq 1.2$", 
"Trustees paired with $\\omega^I \\leq 1.0$", "Trustees paired with $\\omega^I \\geq 1.2$"),
       lty = c(1,1,1,1), col = 
c("blue", "lightblue","red","coral"),
	 bty = 'n', cex = 0.7)
	mtext(text = " Fraction sent ", side = 2, cex = 0.7, line = 2.2)
	mtext(text = " At Step ", side = 1, cex = 0.7, line = 2.2)
	axis(1, at= (1:10),labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), col.axis="black")

	Value =c(sum(BPDTrusteeGuilt==0)/55,	
	         sum(HCTrusteeGuilt==0)/38,
	         sum(BPDTrusteeGuilt==0.4)/55,	
	         sum(HCTrusteeGuilt==0.4)/38,
	         sum(BPDTrusteeGuilt==1)/55,	
	         sum(HCTrusteeGuilt==1)/38
)
	bar=barplot(matrix(Value,2,3), main = "D)  Trustee Guilt $(\\alpha^T)$ Distribution  ",
	cex.main = 0.8,
	xaxt = 'n',
	 ylab = " ",
	 col = c("red", "coral"),# "turquoise2", "turquoise"), 
	 axisnames = TRUE,
	 legend.text = c("BPD Trustee", "HC Trustee"),
	 args.legend = list(x="topright", bty = 'n', cex = 1),
	beside = TRUE, xaxp = c(0,3,3), ylim = c(0,1), yaxp = c(0,1,4));
	axis(1, at= (-1+3*(1:3)), labels=c("0","0.4", "1.0"), col.axis="black", lwd = 0)
	mtext(text = " Relative Empirical Frequency ", side = 2, cex = 0.7, line = 2.2)
	mtext(text = " Parameter Values ", side = 1, cex = 0.7, line = 2.2)
	text(bar[c(1 )]+0.5, 
	Value[bar[c( 2 )]]+0.3, "*", cex = 2);
	segments(bar[c(1 )], Value[bar[c( 1 )]], bar[c(1 )], Value[bar[c( 1 )]]+0.05, lwd =2.2);
	segments(bar[c(1 )], Value[bar[c( 1 )]]+0.05,  bar[c(2 )],
	 Value[bar[c( 1 )]]+0.05 , lwd=2.2);
	segments(bar[c(2 )],
	 Value[bar[c( 1 )]]+0.05 ,  bar[c(2 )],
	Value[bar[c( 2 )]], lwd=2.2);

dev.off();

tools::texi2pdf("Figure4.tex")
#system(paste(getOption("pdfviewer"), "ModBeh.pdf"))