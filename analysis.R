## Project Starts
rm(list=ls())

#download file
download.file(url="https://spark-public.s3.amazonaws.com/dataanalysis/loansData.csv", destfile="./loansData.csv")

#load the csv file
loansData<-read.csv("./loansData.csv",as.is=TRUE, header=T)
getwd()
#summary of loans dataset before transformations
summary(loansData)
## date
date()

#transformations
loansData$Interest.Rate <- as.numeric(gsub("%$","",loansData$Interest.Rate))
loansData$Loan.Length<-as.numeric(as.factor(gsub(" months*$","",loansData$Loan.Length)))
loansData$Debt.To.Income.Ratio <- as.numeric(gsub("%$","",loansData$Debt.To.Income.Ratio))
loansData$FICO.Range<-apply(sapply(strsplit(as.character(loansData$FICO.Range), "-"), function(x) as.numeric(x)), 2, mean)
loansData$State<-as.factor(loansData$State)
str(loansData)




## exploratory graphs
hist(loansData$Interest.Rate, col="blue", breaks= 100, main= "Interest Rates Histogram", xlab="Interest Rate")
hist(loansData$FICO.Range, col="red", breaks= 100, main= "Histogram for FICO Range Means", xlab="Mean of Range")

par(mai=c(1.360000, 1.093333, 1.193333, 1.060000))

loansData$State<-as.factor(loansData$State)

#explorator graphs continued (double y axes)
par(mfrow=c(2,2))

plot(loansData$Interest.Rate,log10(loansData$Amount.Requested), axes=F, ylab="", xlab="", col="orange", pch=4, main="Graph A")
axis(2,col="black",lwd=2)
mtext(2,text="log10(Amount Requested in orange)",line=2)
par(new=T)
plot(loansData$Interest.Rate, log10(loansData$Monthly.Income), yaxt='n', ylab="", xlab="Interest Rates (%)",col="black", pch=19)
axis(4,col="black",lwd=2)
mtext(4,text="log10(Monthly Income) in black",line=2, lwd=2)


plot(loansData$Interest.Rate,loansData$Debt.To.Income.Ratio, axes=F, ylab="", xlab="", col="black", pch=8, main="Graph B")
axis(2,col="black",lwd=2)
mtext(2,text="Debt To Income Ratio in black",line=2)
par(new=T)
plot(loansData$Interest.Rate, log10(loansData$Revolving.CREDIT.Balance), yaxt='n', ylab="", xlab="Interest Rates (%)",col="blue", pch=18 ) 
axis(4,col="black",lwd=2)
mtext(4,text="log10(Revolving Credit Balance) in blue",line=2, lwd=2)



#statistical models

lm1=lm(loansData$Interest.Rate~loansData$FICO.Range)
lm2=lm(loansData$Interest.Rate ~ loansData$FICO.Range + log10(loansData$Monthly.Income))
lm3=lm(loansData$Interest.Rate ~ loansData$FICO.Range + log10(loansData$Monthly.Income) + log10(loansData$Amount.Requested))
lm4=lm(loansData$Interest.Rate ~ loansData$FICO.Range + log10(loansData$Monthly.Income) + log10(loansData$Amount.Requested) + loansData$Loan.Length)
lm5=lm(loansData$Interest.Rate ~ loansData$FICO.Range + log10(loansData$Monthly.Income) + log10(loansData$Amount.Requested) + loansData$Loan.Length + loansData$Debt.To.Income.Ratio)
lm6=lm(loansData$Interest.Rate ~ loansData$FICO.Range + log10(loansData$Monthly.Income) + log10(loansData$Amount.Requested) + loansData$Loan.Length + loansData$Debt.To.Income.Ratio + loansData$Revolving.CREDIT.Balance)
lm7=lm(loansData$Interest.Rate ~ loansData$FICO.Range + log10(loansData$Monthly.Income) + log10(loansData$Amount.Requested) + loansData$Loan.Length + loansData$Debt.To.Income.Ratio +  loansData$Revolving.CREDIT.Balance + loansData$Open.CREDIT.Lines)

lm8=lm(loansData$Interest.Rate ~ loansData$FICO.Range + log10(loansData$Monthly.Income) + log10(loansData$Amount.Requested) + log10(loansData$Amount.Funded.By.Investors + 1) + loansData$Loan.Length + loansData$Debt.To.Income.Ratio +  log10(loansData$Revolving.CREDIT.Balance + 1) + loansData$Open.CREDIT.Lines + loansData$Inquiries.in.the.Last.6.Months)

#fitted plots

plot(loansData$FICO.Range,loansData$Interest.Rate, pch=3, col=loansData$State, xlab="FICO Score Mean", ylab="Interest Rate (%)", main="Graph C")
lines(loansData$FICO.Range,lm8$fitted, col="blue", lwd=1, cex=0.5)
lines(loansData$FICO.Range,lm1$fitted, col="green", lwd=3)
##legend(745,24,legend=c("Model 1","FinalModel","Scatter"),col=c("green", "blue", "brown"),pch=c(19, 19, 19),cex=c(0.7, 0.7, 0.7))

#residuals
plot(loansData$FICO.Range, lm8$residuals, pch = 3, col=loansData$State, ylab="lm residuals", xlab="FICO SCORE (mean)", main="Graph D")


