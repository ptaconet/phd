interv_irs_ctrlequalite_cone_l0<-read.csv("miscellaneous_data/cones_react_BF_CI.csv",sep=";")
colnames(interv_irs_ctrlequalite_cone_l0)[1:6]<-c("date","codepays","status","time","month","codevillage")
colnames(interv_irs_ctrlequalite_cone_l0)<-tolower(colnames(interv_irs_ctrlequalite_cone_l0))
interv_irs_ctrlequalite_cone_l0$date<-as.character(as.Date(interv_irs_ctrlequalite_cone_l0$date,format="%d/%m/%Y"))
