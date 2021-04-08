
#get residuals of mito.cn
data$age.sq<-data$age_blood_draw^2
data$bloodyear<-as.factor(data$bloodyear)
data$sex<-as.factor(data$sex)
data2<-data[!is.na(data$mito.cn)&!is.na(data$bloodyear)&!is.na(data$sex)&!is.na(data$age_blood_draw),]
outc<-"mito.cn"
cov<-"age_blood_draw+age.sq+sex+bloodyear"
model  <- as.formula(paste(outc, "~", cov))
fit    <- lm(model,data=data2)
data2$mtDNA_resid<-resid(fit)
data2$mtDNA_stresid<-scale(data2$mtDNA_resid)

#Forestplot

dat<-read.table("E:\\study\\BU\\sequencing\\mtdna_traits_summary\\forest01262021\\EABMI.csv", sep=',', header=T)

meta_beta      <- dat[,2]

meta_se        <- dat[,3]

label          <- as.character(dat[,1])

pdf(file="path/bmiv2.pdf",width=7,height=6)

forest(meta_beta,sei=meta_se,slab=label,pch=18,psize=1.5, ilab=dat$num,ilab.xpos=-0.6,digits=2,refline=0,annotate=T,header="Cohort",xlab='BMI')
text(-0.6,10,"N",cex=1,font=2)
dev.off()

#beta comparison
t<-read.csv("/topmedvsukb.csv")

pdf(file="/eavsukb.pdf",width=7,height=7)
ggplot2.scatterplot(data=t,xName='X',yName='Y',
                    size=1.2,backgroundColor="white",
                    removePanelBorder=T,xShowTitle=T,ytitle="EA", xtitle="UKB",#setShapeByGroupName=T,
                    axisLine=c(0.5,"solid","black"),xlim=c(-0.2,0.7) ,ylim=c(-0.2,0.7))+geom_abline(color="red")+ geom_text(aes(label=Traits),hjust=-0.2,vjust=0)
dev.off()
