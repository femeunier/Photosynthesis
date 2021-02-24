# meteo variables
# meteo<-list()
# meteo$TA<-18
# meteo$PA<-1.2
# meteo$RH<-60
# meteo$SW<-700
# meteo$CO2<-410
#
# varlist<-c("TIMESTAMP_START","TA_F","VPD_F","RH","SW_IN_F","LW_IN_F","PA","NETRAD","LE_F_MDS","H_F_MDS","G_F_MDS","NEE_VUT_USTAR50","RECO_NT_VUT_USTAR50","GPP_NT_VUT_USTAR50")
# noms<-c("TIMESTAMP", "TA","SW","LW","VPD","PA","RH","NETRAD","G","LE","H","NEE","RECO","GPP")
# # http://sites.fluxdata.org/FI-Hyy/
# fic<-read.table(file = "scripts/FLX_FI-Hyy_FLUXNET2015_FULLSET_HH_1996-2014_1-4.csv",header=T,sep=",")
# fic<-fic[,names(fic)%in%varlist]
# names(fic)<-noms
# YEAR<-as.numeric(substr(as.character(fic$TIMESTAMP),start = 1,stop=4))
# subfic<-fic[YEAR%in%c(2005:2014),]
# write.table(x = subfic,file = "scripts/FLUXNET_FI-HYY_2005-2014.csv",col.names = TRUE,sep=',')
