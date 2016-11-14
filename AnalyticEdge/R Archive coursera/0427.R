#get and clean data quiz 3 question 1
url="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
u=url(url)
f=file.path(getwd(), "communitysurvey.R")
s=download.file(url,f)
a=read.csv(f,header=TRUE)
m=a[which(a$ACR==3 & a$AGS==6),]

#question 2
url2="https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
u2=url(url2)
f2=file.path(getwd(), "question2.jpg")
s2=download.file(url2,f2)
b=readJPEG(f2,native=TRUE)
b2=quantile(b,c(0.3,0.8))

#question 3 4 5
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
b6=b5[order(b5$Gross.domestic.product.2012),]
looking=subset(b5,Gross.domestic.product.2012==13)
View(looking)
#b5$Income.Group==
b7=subset(b6,Gross.domestic.product.2012!="")
#sapply(split(b5$Gross.domestic.product.2012, b5$Income.Group), mean)
#tapply(b5$Gross.domestic.product.2012, b5$Income.Group, mean)
b8=subset(b7,Income.Group=="High income: OECD")
b9=subset(b7,Income.Group=="High income: nonOECD")
q1 = lapply(b8[,2], as.numeric)
q2 = lapply(b9[,2], as.numeric)
View(q1)
View(q2)
b10=subset(b7,Income.Group=="Lower middle income" 
           & as.numeric(as.character(b7$Gross.domestic.product.2012))<=38 )
#as.numeric(as.character(b7$Gross.domestic.product.2012))


