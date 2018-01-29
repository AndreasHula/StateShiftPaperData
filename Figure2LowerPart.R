library(fields)
library(tikzDevice)
library(timeSeries)
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
    "\\usepackage{amssymb}"))
tikz("Figure2LowerPart.tex", width = 5, height = 6.6, standAlone = TRUE,
    packages = c("\\usepackage{tikz}",
                 "\\usepackage[active,tightpage,psfixbb]{preview}",
                 "\\PreviewEnvironment{pgfpicture}",
                 "\\setlength\\PreviewBorder{0pt}",
                 "\\usepackage{amssymb}"))

par(mar=c(0.5,0.5,0.5,0.5)+0.1, mai = c(0.4,0.4,0.4,0.4), #oma = c(0.1,0.1,0.1,1),
cex = 1, cex.lab = 1, cex.axis =1, cex.main = 1)#, cex.text = 1)
layout(matrix( c( 1, 1, 3, 3,  1, 1, 3, 3,  2, 2, 4, 5, 2, 2, 4, 5 ), 4 ,4 ))


	Value2 = matrix(c( colMeans(1/6*Data$NewTrustee[1:55,]), # red
colMeans(1/6*Data$NewTrustee[56:93,]),  #brown
colMeans(1/4*Data$NewInvestor[1:55,]), # blue
colMeans(1/4*Data$NewInvestor[56:93,]) #turq
),10,4)



	Value5 = matrix(c(
1/sqrt(55)*colSds(1/6*Data$NewTrustee[1:55,]),
 1/sqrt(38)*colSds(1/6*Data$NewTrustee[56:93,]),
1/sqrt(55)*colSds(1/4*Data$NewInvestor[1:55,]),
 1/sqrt(38)*colSds(1/4*Data$NewInvestor[56:93,])),10,4)

	matplot(array(c((1:10),(1:10),(1:10), (1:10) ),10,4), 
	main= "C)  Investment and Repayment Profile,  
 Real Subject Data ",  
	cex.main = 1.0, 
	array(c(Value2[,1], Value2[,2], Value2[,3], Value2[,4]),c(10,4)),
	axes = TRUE,
	xlab = ' ', 
	ylab = ' ', 
	xaxt = 'n',
	xlim = c(1,10), #xaxp = c(1,10,9),
	 ylim=c(0,1),yaxp=c(0,1,4),
	frame.plot = FALSE, 
	type =c("l", "l", "l", "l"), lwd =2, lty = c(1,1,1,1), #line = 8,
	col = c("red", "coral","blue","lightblue"), cex.axis = 1 , cex.lab = 1);
	mtext(text = " Fraction sent ", side = 2, cex = 0.7, line = 2)
	mtext(text = " At Step ", side = 1, cex = 0.7, line = 2)
	segments(6, Value2[6,2], 6+0.4, Value2[6,2], lwd =2.2);
	segments(6+0.4, Value2[6,2], 6+0.4, Value2[6,1], lwd =2.2);
	segments(6, Value2[6,1], 6+0.4, Value2[6,1], lwd =2.2);
	text(6+0.6, 
	1/2*(Value2[6,2]-Value2[6,1]) + Value2[6,1]
	 , "*", cex = 1.5);
	arrows((1:10), Value2[,1], (1:10), Value2[,1]+Value5[,1], 0.05, 90);
	arrows((1:10), Value2[,1], (1:10), Value2[,1]-Value5[,1], 0.05, 90);
	arrows((1:10), Value2[,2], (1:10), Value2[,2]+Value5[,2], 0.05, 90);
	arrows((1:10), Value2[,2], (1:10), Value2[,2]-Value5[,2], 0.05, 90);
	arrows((1:10), Value2[,3], (1:10), Value2[,3]+Value5[,3], 0.05, 90);
	arrows((1:10), Value2[,3], (1:10), Value2[,3]-Value5[,3], 0.05, 90);
	arrows((1:10), Value2[,4], (1:10), Value2[,4]+Value5[,4], 0.05, 90);
	arrows((1:10), Value2[,4], (1:10), Value2[,4]-Value5[,4], 0.05, 90);
	axis(1, at= (1:10),labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), col.axis="black")
	legend(x="topright", 2, c("HC paired Investor", "BPD paired Investor", 
"HC Trustee", "BPD Trustee"), lwd = 2, lty = c(1,1,1,1),
col = c("lightblue","blue", "coral", "red"),
	 bty = 'n', cex = 0.9)


	Value1 =c(mean(colSums(Investor[1:55,])), 
	          mean(colSums(OldInvestor[1:55,])),
	          mean(colSums(Investor[56:93,])), 
	          mean(colSums(OldInvestor[56:93,])))
	#Value1 = Value1+80;
	Value4 =c(1/sqrt(55)*sd(colSums(Investor[1:55,])), 
	          1/sqrt(55)*sd(colSums(OldInvestor[1:55,])),
	          1/sqrt(38)*sd(colSums(Investor[56:93,])), 
	          1/sqrt(38)*sd(colSums(OldInvestor[56:93,]))
)
	bar=barplot(matrix(Value1,2,2), main = "D)   Investment Reproduction,    
Classic Model ",
	xaxt = 'n',
	 ylab = " ",
	 cex.axis = 1, cex.lab = 1, cex.main =1.0,
	 col = c( "blue", "blue", "lightblue", "lightblue"),
	 density= c(-1, 12, -1, 12),
	 axisnames = TRUE,
	beside = TRUE, xaxp = c(0,5,5),#yaxt = 'n',
	 ylim = c(0,150), yaxp = c(0,150,6) )#, ylim = c(0,50), yaxp = c(0,40,6) )#, ylim = c(0,40), yaxp = c(0,30,6), ylabels = c("80", "85", "90", "95", "100", "105", "110"));
	text(bar[c(1 )]+0.5, 
	Value1[bar[c( 2 )]]+Value4[bar[c( 2 )]]
	 +30, "*", cex = 2);
	mtext(text = " Mean Investment ", side = 2, cex = 0.7, line = 2)
	segments(bar[c(1 )], Value1[bar[c( 1 )]]+Value4[bar[c( 1 )]]+10, bar[c(1 )], Value1[bar[c( 2 )]]+Value4[bar[c( 2 )]]+20, lwd =2.2);
	segments(bar[c(1 )], Value1[bar[c( 2 )]]+Value4[bar[c( 2 )]] +20,  bar[c(2 )],
	Value1[bar[c( 2 )]]+Value4[bar[c( 2 )]]+20 , lwd=2.2);
	segments(bar[c(2 )], Value1[bar[c( 2 )]]+Value4[bar[c( 2 )]] +20,  bar[c(2 )],
	Value1[bar[c( 2 )]]+Value4[bar[c( 2 )]]+10 , lwd=2.2);
	axis(1, at= c(1.2, 2.5,  4.2, 5.5), labels=c("actual", "simulated","actual","simulated"),
	 col.axis="black", lwd = 0,cex.axis =1)
	mtext(text = c("BPD paired Investor", "HC paired Investor"), at = c(2,5),side = 1, 
	cex = 0.6, line = 2.2)
	arrows(0.5+c((1:2), (4:5)), Value1, 0.5+c((1:2), (4:5)), Value1+Value4, 0.05, 90);
	arrows(0.5+c((1:2), (4:5)), Value1, 0.5+c((1:2), (4:5)), Value1-Value4, 0.05, 90);

	Value1 =c(mean(colSums(Trustee[1:55,])), 
	          mean(colSums(OldTrustee[1:55,])),
	          mean(colSums(Trustee[56:93,])), 
	          mean(colSums(OldTrustee[56:93,])))
	#Value1 = Value1+80;
	Value4 =c(1/sqrt(55)*sd(colSums(Trustee[1:55,])), 
	          1/sqrt(55)*sd(colSums(OldTrustee[1:55,])),
	          1/sqrt(38)*sd(colSums(Trustee[56:93,])), 
	          1/sqrt(38)*sd(colSums(OldTrustee[56:93,])))
	bar=barplot(matrix(Value1,2,2), main = "E)   Repayment Reproduction,    
 Classic Model ",
	xaxt = 'n',
	  ylab = " ",
	 cex.axis = 1, cex.lab = 1, cex.main =1.0,
	 col = c( "red", "red", "coral", "coral"),#, "cadetblue1"), 
	density= c(-1, 12, -1, 12),
	 axisnames = TRUE,
	beside = TRUE, xaxp = c(0,5,5), #yaxt = 'n',
	 ylim = c(0,150), yaxp = c(0,150,6) )#, ylim = c(0,40), yaxp = c(0,30,6), ylabels = c("80", "85", "90", "95", "100", "105", "110"));
	text(bar[1,2]+0.5, 
	Value1[3]+Value4[3]
	 +20, "*", cex = 2);
	mtext(text = " Mean Repayment ", side = 2, cex = 0.7, line = 2)
	segments(bar[1,2], Value1[3]+Value4[3]+5, bar[1,2], Value1[3]+Value4[3]+10, lwd =2);
	segments(bar[1,2], Value1[3]+Value4[3] +10,  bar[2,2],
	Value1[3]+Value4[3]+10 , lwd=2);
	segments(bar[2,2], Value1[3]+Value4[3] +10,  bar[2,2],
	Value1[4]+Value4[4]+5 , lwd=2);
	axis(1, at= c(1.2, 2.5,  4.2, 5.5), cex.axis =1, labels=c("actual", "simulated","actual","simulated"), 
	col.axis="black", lwd = 0)
	mtext(text = c("BPD Trustee", "HC Trustee"), at = c(2,5),side = 1, cex = 0.6, line = 2.2)
	arrows(0.5+c((1:2), (4:5)), Value1, 0.5+c((1:2), (4:5)), Value1+Value4, 0.05, 90);
	arrows(0.5+c((1:2), (4:5)), Value1, 0.5+c((1:2), (4:5)), Value1-Value4, 0.05, 90);







	Value5 = matrix(c(  
1/4*NewInvestor[29,] # blue
colMeans(1/4*ClassicInvestor) #turq
),10,2)


	Value6 = matrix(c(
c( 0,    0,    0,    0,    0,    0,    0,
0,    0,    0),
1/sqrt(200)*colSds(1/4*ClassicInvestor)),10,2)

	matplot(array(c((1:10),(1:10)),10,2), array(c(Value5[,1], Value5[,2]),c(10,2)),
	main= "F)     Reproduced Investor Trajectory,         
 Classic Model ",  
	cex.main = 1.0, 	
	yaxt = 't',
	xlab = ' ',#'At Step', 
	ylab = ' ', 
	xlim = c(1,10), #xaxp = c(1,10,9),
	xaxt = 'n',
	 ylim=c(0,1),yaxp=c(0,1,4),
	frame.plot = FALSE, 
	type =c("l", "l"), lwd =c(3,1), lty = c(1,2), #line = 8,
	col = c("blue","blue"), cex.axis = 1 , cex.lab = 1);
	polygon( c((1:10),  (10:1)), c(pmin(Value5[1:10,2]+Value6[1:10,2],1.0), 
	pmax(Value5[10:1,2]-Value6[10:1,2],0.0)), 
	col = rgb(0,0,200, alpha =30, maxColorValue=255), density = -1, 
	border = 'blue', lty = 2);
	polygon( c((1:10),  (10:1)), c(pmin(Value5[1:10,2]+Value6[1:10,2],1.0), 
	pmax(Value5[10:1,2]-Value6[10:1,2],0.0)), 
	col = rgb(0,0,200, alpha =30, maxColorValue=255), density = 40, 
	border = 'NA', lty = 1);
	mtext(text = " Fraction sent ", side = 2, cex = 0.7, line = 2)
	mtext(text = " At Step ", side = 1, cex = 0.7, line = 2)
	axis(1, at= (1:10),labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), col.axis="black",cex.axis = 1)
	legend(x="topright", 2, c("Investor", "Simulated Investor"), lwd =2, lty = c(1,2),
	col = c("blue","blue"), #density = c(-1, 20, -1, 20),
	 bty = 'n', cex = 0.9)

	Value5 = matrix(c(  
	  1/6*NewTrustee[29,] # blue
	  colMeans(1/6*ClassicTrustee) #turq
	),10,2)
	
	
	Value6 = matrix(c(
	  c( 0,    0,    0,    0,    0,    0,    0,
	     0,    0,    0),
	  1/sqrt(200)*colSds(1/6*ClassicTrustee)),10,2)


	matplot(array(c((1:10),(1:10)),10,2), array(c(Value5[,1], Value5[,2]),c(10,2)),
	main= "G)     Reproduced Trustee Trajectory,         
 Classic Model ",  
	cex.main = 1.0, 	
	yaxt = 't',
	xlab = ' ',
	ylab = ' ', 
	xlim = c(1,10), 
	xaxt = 'n',
	 ylim=c(0,1),yaxp=c(0,1,4),
	frame.plot = FALSE, 
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
	mtext(text = " Fraction sent ", side = 2, cex = 0.7, line = 2)
	mtext(text = " At Step ", side = 1, cex = 0.7, line = 2)
	axis(1, at= (1:10),labels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), col.axis="black")
	legend(x="topright", 2, c("Trustee", "Simulated Trustee"), lwd =2, lty = c(1,2),
	col = c("red", "red"), #density = c(-1, 20, -1, 20),
	 bty = 'n', cex = 0.9)




dev.off();

tools::texi2pdf("Figure2LowerPart.tex")
