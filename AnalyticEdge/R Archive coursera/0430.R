#Get and clean data quiz 4 question 1
f=file.path(getwd(),"commonsurvey.csv")
c=read.csv(f,header=TRUE)
k=strsplit(names(c),"wgtp")
k[123]

#question 2
url2="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
u2=url(url2)
f2=file.path(getwd(), "GDP2.csv")
s2=download.file(url2,f2)
c2=read.csv(f2,header=TRUE)
k2=c2$X.3[5:194]
d2=vector()
#d2=as.numeric(d2)
mergingi=function(x) {
    l=as.numeric(gsub(',','',x))
    if (is.numeric(l)==TRUE) {
        l
    }
}
for (i in 1:length(k2)) {
    #append(d2,merging(k2[i]),after=length(d2))
    d2[i]=mergingi(k2[i])
}
d23=na.omit(d2)
mean(d23)

#question 3
grep("*United",c2$X.2)

#question 4
url3=
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
f3=file.path(getwd(), "gdp.csv")
s3=download.file(url3,f3)
b3=read.csv(f3,header=TRUE)

url4=
    "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
f4=file.path(getwd(), "educationdata.csv")
s4=download.file(url4,f4)
b4=read.csv(f4,header=TRUE)
b5=merge(b3,b4,by.x="X",by.y="CountryCode")

#question 5
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn) 
k4=amzn[,0]


