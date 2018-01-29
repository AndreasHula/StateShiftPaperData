#setEPS()
library(tikzDevice)
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
    "\\usepackage{amssymb}"))
tikz("Figure3.tex", width = 6, height = 8, standAlone = TRUE,
    packages = c("\\usepackage{tikz}",
                 "\\usepackage[active,tightpage,psfixbb]{preview}",
                 "\\PreviewEnvironment{pgfpicture}",
                 "\\setlength\\PreviewBorder{0pt}",
                 "\\usepackage{amssymb}"))

par(mar=c(1,1,1,1)+0.1, mai = c(0.5,0.5, 0.5,0.5), 
cex = 1, cex.lab = 1, cex.axis =1, cex.main = 1)
layout(matrix(c(1,2,3,4,5,6), 3, 2))
	Value =c(sum(BPDInvestorAversion==0.4)/55,	
	         sum(HCInvestorAversion==0.4)/38,
	         sum(BPDInvestorAversion==0.6)/55,	
	         sum(HCInvestorAversion==0.6)/38,
	         sum(BPDInvestorAversion==0.8)/55,	
	         sum(HCInvestorAversion==0.8)/38,
	         sum(BPDInvestorAversion==1.0)/55,	
	         sum(HCInvestorAversion==1.0)/38,
	         sum(BPDInvestorAversion==1.2)/55,	
	         sum(HCInvestorAversion==1.2)/38,
	         sum(BPDInvestorAversion==1.4)/55,	
	         sum(HCInvestorAversion==1.4)/38,
	         sum(BPDInvestorAversion==1.6)/55,	
	         sum(HCInvestorAversion==1.6)/38,
	         sum(BPDInvestorAversion==1.8)/55,	
	         sum(HCInvestorAversion==1.8)/38
)
	bar=barplot(matrix(Value,2,8), main = "A)  Investor Risk Aversion ($\\omega^I$) Distribution  ",
	cex.main = 1.3,
	xaxt = 'n',
	 ylab = " ",
	 col = c("blue", "lightblue"),# "turquoise2", "turquoise"), 
	 axisnames = TRUE,
	 legend.text = c("BPD paired Investor", "HC paired Investor"),
	 args.legend = list(x="topright", bty = 'n', cex = 1),
	beside = TRUE, xaxp = c(0,7,7), ylim = c(0,0.5), yaxp = c(0,0.5,2));
	axis(1, at= (-1+3*(1:8)), labels=c("0.4","0.6", "0.8", "1.0", "1.2","1.4","1.6","1.8"),
	 col.axis="black", lwd = 0)
	mtext(text = " Relative Empirical Frequency ", side = 2, cex = 0.8, line = 2.2)
	mtext(text = " Parameter Values ", side = 1, cex = 0.8, line = 2.2)



	Value =c(sum(BPDInvestorIrritability==0)/55,	
	         sum(HCInvestorIrritability==0)/38,
	         sum(BPDInvestorIrritability==1)/55,	
	         sum(HCInvestorIrritability==1)/38,
	         sum(BPDInvestorIrritability==2)/55,	
	         sum(HCInvestorIrritability==2)/38,
	         sum(BPDInvestorIrritability==3)/55,	
	         sum(HCInvestorIrritability==3)/38,
	         sum(BPDInvestorIrritability==4)/55,	
	         sum(HCInvestorIrritability==4)/38
)
	bar=barplot(matrix(Value,2,5), main = "C)  Investor Irritability ($\\zeta^I$) Distribution  ",
	cex.main = 1.3,
	xaxt = 'n',
	 ylab = " ",
	 col = c("blue", "lightblue"),# "turquoise2", "turquoise"), 
	 axisnames = TRUE,
	 legend.text = c("BPD paired Investor", "HC paired Investor"),
	 args.legend = list(x="topright", bty = 'n', cex = 1),
	beside = TRUE, xaxp = c(0,5,5), ylim = c(0,1), yaxp = c(0,1,4));
	axis(1, at= (-1+3*(1:5)), labels=c("0","0.25", "0.5", "0.75", "1.0"), col.axis="black", lwd = 0)
	mtext(text = " Relative Empirical Frequency ", side = 2, cex = 0.8, line = 2.2)
	mtext(text = " Parameter Values ", side = 1, cex = 0.8, line = 2.2)


	Value =c(sum(BPDInvestorShift==0)/55,	
	         sum(HCInvestorShift==0)/38,
	         sum(BPDInvestorShift==1)/55,	
	         sum(HCInvestorShift==1)/38,
	         sum(BPDInvestorShift==2)/55,	
	         sum(HCInvestorShift==2)/38,
	         sum(BPDInvestorShift==3)/55,	
	         sum(HCInvestorShift==3)/38,
	         sum(BPDInvestorShift==4)/55,	
	         sum(HCInvestorShift==4)/38
)
	bar=barplot(matrix(Value,2,5), main = "E)  Investor Awareness ($q^I(\\zeta^T)$) Distribution  ",
	cex.main = 1.3,
	xaxt = 'n',
	 ylab = " ",
	 col = c("blue", "lightblue"),# "turquoise2", "turquoise"), 
	 axisnames = TRUE,
	 legend.text = c("BPD paired Investor", "HC paired Investor"),
	 args.legend = list(x="topright", bty = 'n', cex = 1),
	beside = TRUE, xaxp = c(0,5,5),ylim = c(0,0.5), yaxp = c(0,0.5,2))
	axis(1, at= (-1+3*(1:5)), labels=c("0","1", "2", "3", "4"), col.axis="black", lwd = 0)
	mtext(text = " Relative Empirical Frequency ", side = 2, cex = 0.8, line = 2.2)
	mtext(text = " Parameter Values ", side = 1, cex = 0.8, line = 2.2)


	Value =c(sum(BPDTrusteeAversion==0.4)/55,	
	         sum(HCTrusteeAversion==0.4)/38,
	         sum(BPDTrusteeAversion==0.6)/55,	
	         sum(HCTrusteeAversion==0.6)/38,
	         sum(BPDTrusteeAversion==0.8)/55,	
	         sum(HCTrusteeAversion==0.8)/38,
	         sum(BPDTrusteeAversion==1.0)/55,	
	         sum(HCTrusteeAversion==1.0)/38,
	         sum(BPDTrusteeAversion==1.2)/55,	
	         sum(HCTrusteeAversion==1.2)/38,
	         sum(BPDTrusteeAversion==1.4)/55,	
	         sum(HCTrusteeAversion==1.4)/38,
	         sum(BPDTrusteeAversion==1.6)/55,	
	         sum(HCTrusteeAversion==1.6)/38,
	         sum(BPDTrusteeAversion==1.8)/55,	
	         sum(HCTrusteeAversion==1.8)/38
)
	bar=barplot(matrix(Value,2,8), main = "B)  Trustee Aversion Belief ($b^T(\\omega^I)$) Distribution  ",
	cex.main = 1.3,
	xaxt = 'n',
	 ylab = " ",
	 col = c("red", "coral"),# "turquoise2", "turquoise"), 
	 axisnames = TRUE,
	 legend.text = c("BPD Trustee", "HC Trustee"),
	 args.legend = list(x="topright", bty = 'n', cex = 1),
	beside = TRUE, xaxp = c(0,7,7), ylim = c(0,0.5), yaxp = c(0,0.5,2))
	axis(1, at= (-1+3*(1:8)), labels=c("0.4","0.6", "0.8", "1.0", "1.2","1.4","1.6","1.8"), col.axis="black", lwd = 0);
	mtext(text = " Relative Empirical Frequency ", side = 2, cex = 0.8, line = 2.2)
	mtext(text = " Parameter Values ", side = 1, cex = 0.8, line = 2.2)


	Value =c(sum(BPDTrusteeIrritability==0)/55,	
	         sum(HCTrusteeIrritability==0)/38,
	         sum(BPDTrusteeIrritability==1)/55,	
	         sum(HCTrusteeIrritability==1)/38,
	         sum(BPDTrusteeIrritability==2)/55,	
	         sum(HCTrusteeIrritability==2)/38,
	         sum(BPDTrusteeIrritability==3)/55,	
	         sum(HCTrusteeIrritability==3)/38,
	         sum(BPDTrusteeIrritability==4)/55,	
	         sum(HCTrusteeIrritability==4)/38
)
	bar=barplot(matrix(Value,2,5), main = "D)  Trustee Irritability $(\\zeta^T)$ Distribution  ",
	cex.main = 1.3,
	xaxt = 'n',
	 ylab = " ",
	 col = c("red", "coral"),# "turquoise2", "turquoise"), 
	 axisnames = TRUE,
	 legend.text = c("BPD Trustee", "HC Trustee"),
	 args.legend = list(x="topright", bty = 'n', cex = 1),
	beside = TRUE, xaxp = c(0,5,5), ylim = c(0,1), yaxp = c(0,1,4));
	axis(1, at= (-1+3*(1:5)), labels=c("0","0.25", "0.5", "0.75", "1.0"), col.axis="black", lwd = 0)
	mtext(text = " Relative Empirical Frequency ", side = 2, cex = 0.8, line = 2.2)
	mtext(text = " Parameter Values ", side = 1, cex = 0.8, line = 2.2)


	Value =c(sum(BPDTrusteeShift==0)/55,	
	         sum(HCTrusteeShift==0)/38,
	         sum(BPDTrusteeShift==1)/55,	
	         sum(HCTrusteeShift==1)/38,
	         sum(BPDTrusteeShift==2)/55,	
	         sum(HCTrusteeShift==2)/38,
	         sum(BPDTrusteeShift==3)/55,	
	         sum(HCTrusteeShift==3)/38,
	         sum(BPDTrusteeShift==4)/55,	
	         sum(HCTrusteeShift==4)/38
)
	bar=barplot(matrix(Value,2,5), main = "F)  Trustee Awareness ($q^T(\\zeta^I)$) Distribution  ",
	cex.main = 1.3,
	xaxt = 'n',
	 ylab = " ",
	 col = c("red", "coral"),# "turquoise2", "turquoise"), 
	 axisnames = TRUE,
	 legend.text = c("BPD Trustee", "HC Trustee"),
	 args.legend = list(x="topright", bty = 'n', cex = 1),
	beside = TRUE, xaxp = c(0,5,5), ylim = c(0,0.5), yaxp = c(0,0.5,2))
	axis(1, at= (-1+3*(1:5)), labels=c("0","1", "2", "3", "4"), col.axis="black", lwd = 0)
	mtext(text = " Relative Empirical Frequency ", side = 2, cex = 0.8, line = 2.2)
	mtext(text = " Parameter Values ", side = 1, cex = 0.8, line = 2.2)
	text(bar[c(1 )]+0.5, 
	Value[bar[c( 2 )]]+0.3, "*", cex = 2);
	segments(bar[c(1 )], Value[bar[c( 1 )]], bar[c(1 )], Value[bar[c( 1 )]]+0.05, lwd =2.2);
	segments(bar[c(1 )], Value[bar[c( 1 )]]+0.05,  bar[c(2 )],
	 Value[bar[c( 1 )]]+0.05 , lwd=2.2);
	segments(bar[c(2 )],
	 Value[bar[c( 1 )]]+0.05 ,  bar[c(2 )],
	Value[bar[c( 2 )]], lwd=2.2);
dev.off();

tools::texi2pdf("Figure3.tex")
#system(paste(getOption("pdfviewer"), "BrooksDataFinalModel.pdf"))
