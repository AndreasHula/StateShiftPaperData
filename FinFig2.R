library(fields)
library(tikzDevice)
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
    "\\usepackage{amssymb}"))
tikz("Figure2.tex", width = 5, height = 6.6, standAlone = TRUE,
    packages = c("\\usepackage{tikz}",
                 "\\usepackage[active,tightpage,psfixbb]{preview}",
                 "\\PreviewEnvironment{pgfpicture}",
                 "\\setlength\\PreviewBorder{0pt}",
                 "\\usepackage{amssymb}"))

par(mar=c(0.5,0.5,0.5,0.5)+0.1, mai = c(0.4,0.4,0.4,0.4), #oma = c(0.1,0.1,0.1,1),
cex = 1, cex.lab = 1, cex.axis =1, cex.main = 1)#, cex.text = 1)
layout(matrix( c( 6, 7, 9,9, 6, 7, 9, 9, 8, 8, 10, 11, 8, 8, 10, 11 )-5, 4 ,4 ))


	Value = matrix(c( Shift[10,],
1/4*RepairInvestor[10,],
1/6*RepairTrustee[10,] ),10,3)

	matplot(array(c((1:10),(1:10), (1:10)),c(10,3) ), 
	main = "A)  Irritable Investor experiencing Repair,
     $\\zeta^I = 0.5, q^T(\\zeta^I)= 2$   ",
	cex.main = 1.0, 
	array(c(Value[,1], Value[,2], Value[,3]),c(10,3)),
	xlab = ' ', 
	ylab = ' ', 
	xaxt = 'n',
	#xlab = 'At Step', ylab = 'Fraction sent', 
	xlim = c(1,10), xaxp = c(1,10,9),
	 ylim=c(0,1),yaxp=c(0,1,4),
	frame.plot = FALSE, 
	type =c("l", "l", "l"), lwd =2, lty = c(1,1,1), 
	col = c("goldenrod4", "blue", "red"), cex.axis = 0.7 , cex.lab = 0.7);
	legend(x="topright", 2, c("$\\nu^I$","$a^I$", "$a^T$" ),
      fill = c("goldenrod4", "blue", "red"), col = c("goldenrod4", "blue", "red"),
	 bty = 'n', cex = 0.9)
	axis(4, at = c(0,0.25,0.5,0.75,1), lwd = 2, cex.axis = 0.7, col = "goldenrod4", col.ticks = "goldenrod4", col.axis= "goldenrod4")
	mtext(side = 4, line = 2, "$\\nu^I$", cex = 0.7, col = "goldenrod4")
	mtext(text = " Fraction sent ", side = 2, cex = 0.7, line = 2)
	mtext(text = " At Step ", side = 1, cex = 0.7, line = 2)
	axis(1, at= (1:10),labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), col.axis="black", cex.axis = 1.0)

	Value = matrix(c( BreakShift[1,],
	                  1/4*BreakInvestor[1,],
	                  1/6*BreakTrustee[1,] ),10,3)

	matplot(array(c((1:10),(1:10), (1:10)), c(10,3) ), 
	main = "B)  Irritable Investor experiencing Break,
   $\\zeta^I = 0.5, q^T(\\zeta^I)= 0$   ",
	cex.main = 1.0, 
	array(c(Value[,1], Value[,2], Value[,3]),c(10,3)),
	xlab = ' ', 
	ylab = ' ', 
	xaxt = 'n',
	#xlab = 'At Step', ylab = 'Fraction sent', 
	xlim = c(1,10), xaxp = c(1,10,9),
	 ylim=c(0,1),yaxp=c(0,1,4),
	frame.plot = FALSE, 
	type =c("l", "l", "l"), lwd =2, lty = c(1,1,1), 
	col = c("goldenrod4", "blue", "red"), cex.axis = 0.7 , cex.lab = 0.7);
	legend(x="topright", 2, c("$\\nu^I$","$a^I$", "$a^T$" ),
      fill = c("goldenrod4", "blue", "red"), col = c("goldenrod4", "blue", "red"),
	 bty = 'n', cex = 0.9)
	axis(4, at = c(0,0.25,0.5,0.75,1), lwd = 2, cex.axis = 0.7, col = "goldenrod4", col.ticks = "goldenrod4", col.axis= "goldenrod4")
	mtext(side = 4, line = 2, "$\\nu^I$", cex = 0.7, col = "goldenrod4")
	mtext(text = " Fraction sent ", side = 2, cex = 0.7, line = 2)
	mtext(text = " At Step ", side = 1, cex = 0.7, line = 2)
	axis(1, at= (1:10),labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), col.axis="black", cex.axis = 1.0)


	Value1 =c(mean(rowSums(InvestorWithBPD)),
	          mean(rowSums(FlexiInvestor[1:55,])) ,
	          mean(rowSums(InvestorWithHC)),
	          mean(rowSums(FlexiInvestor[56:93,]))
	)
	#Value1 = Value1+60;
	Value4 =c(
	  1/sqrt(55)*sd(rowSums(InvestorWithBPD)),
	  1/sqrt(55)*sd(rowSums(FlexiInvestor[1:55,])) ,
	  1/sqrt(38)*sd(rowSums(InvestorWithHC)),
	  1/sqrt(38)*sd(rowSums(FlexiInvestor[56:93,]))	  
	)
	bar=barplot(matrix(Value1,2,2), main = "C)   Investment Reproduction,    
Full Model ",
	xaxt = 'n',
	 ylab = " ",
	 cex.axis = 1, cex.lab = 1, cex.main = 1.0,
	 col = c( "blue", "blue", "lightblue", "lightblue"),#, "cadetblue1"), 
	 density= c(-1, 12, -1, 12),
	 axisnames = TRUE,
	beside = TRUE, xaxp = c(0,5,5),#yaxt = 'n',
	 ylim = c(0,150), yaxp = c(0,150,6) )#, ylim = c(0,50), yaxp = c(0,40,6) )
	mtext(text = " Mean Investment ", side = 2, cex = 0.7, line = 2.2)
	axis(1, at= c(1.2, 2.5,  4.2, 5.5), 
	labels=c("actual", "simulated","actual","simulated"), 
	col.axis="black", lwd = 0, cex.axis = 1)
	mtext(text = c("BPD paired Investor", "HC paired Investor"), at = c(1.8,5),side = 1, cex = 0.6, line = 2.2)
	arrows(0.5+c((1:2), (4:5)), Value1, 0.5+c((1:2), (4:5)), Value1+Value4, 0.01, 90);
	arrows(0.5+c((1:2), (4:5)), Value1, 0.5+c((1:2), (4:5)), Value1-Value4, 0.01, 90);

	Value1 =c(mean(rowSums(BPDTrustee)),
	          mean(rowSums(FlexiTrustee[1:55,])) ,
	          mean(rowSums(HCTrustee)),
	          mean(rowSums(FlexiTrustee[56:93,]))
)
	#Value1 = Value1+60;
	Value4 =c(
	  1/sqrt(55)*sd(rowSums(BPDTrustee)),
	  1/sqrt(55)*sd(rowSums(FlexiTrustee[1:55,])) ,
	  1/sqrt(38)*sd(rowSums(HCTrustee)),
	  1/sqrt(38)*sd(rowSums(FlexiTrustee[56:93,]))	  
)
	bar=barplot(matrix(Value1,2,2), main = "D)   Repayment Reproduction,    
 Full Model ",
	#xlab = "Group",
	xaxt = 'n',
	  ylab = " ",
	 cex.axis = 1, cex.lab = 1, cex.main = 1.0,
	 col = c( "red", "red", "coral", "coral"),#, "cadetblue1"), 
	density= c(-1, 12, -1, 12),
	 axisnames = TRUE,
	beside = TRUE, xaxp = c(0,5,5), #yaxt = 'n',
	 ylim = c(0,150), yaxp = c(0,150,6) )#, ylim = c(0,40), yaxp = c(0,30,6), ylabels = c("80", "85", "90", "95", "100", "105", "110"));
	mtext(text = " Mean Repayment ", side = 2, cex = 0.7, line = 2.2)
	axis(1, at= c(1.2, 2.5,  4.2, 5.5), labels=c("actual", "simulated","actual","simulated"), col.axis="black", lwd = 0, cex.axis = 1)
	mtext(text = c("BPD Trustee", "HC Trustee"), at = c(2,5),side = 1, cex = 0.6, line = 2.2)
	arrows(0.5+c((1:2), (4:5)), Value1, 0.5+c((1:2), (4:5)), Value1+Value4, 0.01, 90);
	arrows(0.5+c((1:2), (4:5)), Value1, 0.5+c((1:2), (4:5)), Value1-Value4, 0.01, 90);

	Value5 = matrix(c(
	  FracInvestorWithBPD[29,], # blue
colMeans(1/4*FinalInvestor) #turq
),10,2)


	Value6 = matrix(c(
c( 0,    0,    0,    0,    0,    0,    0,
0,    0,    0),
colSds(1/4*FinalInvestor)),10,2)


	matplot(array(c((1:10),(1:10)),c(10,2) ), array(c(Value5[,1], Value5[,2]),c(10,2)),
	main= "E)     Reproduced Investor Trajectory,         
Full Model ",  
	cex.main = 1.0, 	
	#axes = TRUE,
	yaxt = 't',
	xlab = ' ',#'At Step', 
	ylab = ' ', 
	xlim = c(1,10), #xaxp = c(1,10,9),
	xaxt = 'n',
	 ylim=c(0,1),yaxp=c(0,1,4),
	frame.plot = FALSE, 
	type =c( "l", "l"), lwd = c(3,1), lty = c(1,2), #line = 8,
	col = c("blue","blue"), cex.axis = 1 , cex.lab = 1);
	polygon( c((1:10),  (10:1)), c(pmin(Value5[1:10,2]+Value6[1:10,2],1.0), 
	pmax(Value5[10:1,2]-Value6[10:1,2],0.0)), 
	col = rgb(0,0,200, alpha =30, maxColorValue=255), density = -1, 
	border = 'blue', lty = 2);
	polygon( c((1:10),  (10:1)), c(pmin(Value5[1:10,2]+Value6[1:10,2],1.0), 
	pmax(Value5[10:1,2]-Value6[10:1,2],0.0)), 
	col = rgb(0,0,200, alpha =30, maxColorValue=255), density = 40, 
	border = 'NA', lty = 1);
	mtext(text = " Fraction sent ", side = 2, cex = 0.7, line = 2.2)
	mtext(text = " At Step ", side = 1, cex = 0.7, line = 2.2)
	axis(1, at= (1:10),labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), col.axis="black")
	legend(x="topright", 2, c("Investor", "Simulated Investor"),
 lwd =c(3,1), lty = c(1,2),
      #fill =c("blue","blue",  "red","red"), 
	col = c("blue","blue"), #density = c(-1, 20, -1, 20),
	 bty = 'n', cex = 0.9)

	Value5 = matrix(c( FracBPDTrustee[29,],  # red
colMeans(1/6*FinalTrustee) # coral
),10,2)


	Value6 = matrix(c(
c( 0,    0,    0,    0,    0,    0,    0,
   0,    0,    0), 
colSds(1/6*FinalTrustee)
 ),10,2)


	matplot(array(c((1:10),(1:10)), c(10,2) ), array(c(Value5[,1], Value5[,2]),c(10,2)),
	main= "F)     Reproduced Trustee Trajectory,         
 Full Model ",  
	cex.main = 1.0, 	
	#axes = TRUE,
	yaxt = 't',
	xlab = ' ',#'At Step', 
	ylab = ' ', 
	xlim = c(1,10), #xaxp = c(1,10,9),
	xaxt = 'n',
	 ylim=c(0,1),yaxp=c(0,1,4),
	#outer = TRUE,
	frame.plot = FALSE, 
	#legend(1,2, c("LH", "HL")),
	# legend.text = c("LH", "HL"),
	# args.legend = list(x="topright", bty = 'n', cex = 2.5),
	type =c("l", "l"), lwd =c(3,1), lty = c(1,2), #line = 8,
	col = c("red", "red"), cex.axis = 1 , cex.lab = 1);
	polygon( c((1:10),  (10:1)), c(pmin(Value5[1:10,2]+Value6[1:10,2],1.0), 
	pmax(Value5[10:1,2]-Value6[10:1,2],0.0)), 
	col = rgb(200,0,0, alpha =30, maxColorValue=255), density = -1, 
	border = 'red', lty = 2);
	polygon( c((1:10),  (10:1)), c(pmin(Value5[1:10,2]+Value6[1:10,2],1.0), 
	pmax(Value5[10:1,2]-Value6[10:1,2],0.0)), 
	col = rgb(200,0,0, alpha =30, maxColorValue=255), density = 40, 
	border = 'NA', lty = 1);
	mtext(text = " Fraction sent ", side = 2, cex = 0.7, line = 2.2)
	mtext(text = " At Step ", side = 1, cex = 0.7, line = 2.2)
	axis(1, at= (1:10),labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), col.axis="black")
	legend(x="topright", 2, c("Trustee", "Simulated Trustee"), 
lwd =c(3,1), lty = c(1,2),
      #fill =c("blue","blue",  "red","red"), 
	col = c("red", "red"), #density = c(-1, 20, -1, 20),
	 bty = 'n', cex = 0.9)







dev.off();

tools::texi2pdf("Figure2.tex")
