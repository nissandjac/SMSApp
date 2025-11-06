
# call to the OP program
do_OP<-function(readResSimple=TRUE,readResDetails=FALSE,readResStom=FALSE,writeOption=FALSE,writeExplPat=FALSE,source='',
                data_dir='Data') {
  op.n<<-op.n+1

  #cat(op.n, "call source:", source, "  ,data_dir:",data_dir,"  ,readResSimple:",readResSimple," ,readResDetails:",readResDetails," ,readResStom:",readResStom," ,writeOption:",writeOption,"  writeExplPat:",writeExplPat,'\n')
   # write the F values
  Fvalues<-OP.trigger@Ftarget['init',]
  cat("1\n",Fvalues,"\n",file=file.path(data_dir,"op_multargetf.in")) # write F values
  
  if (writeOption) {  #write option files
    write.FLOP.control(OP,file="op.dat",path=data_dir,nice=TRUE,writeSpNames=FALSE)   
    write.FLOPtrigger.control(OP.trigger,file="op_trigger.dat",path=data_dir,nice=FALSE,writeSpNames=FALSE)
    doWriteOptions<<-FALSE
  } 
  
  if (writeExplPat){
    explPat<<-updateExplPatttern(explPat)
    
    out<-file.path(data_dir,'op_exploitation.in')
    cat("# exploitation pattern\n",file=out)
    for (q in (1:n.seasons)) {
      cat(paste("# quarter",q,'\n'),file=out,append=TRUE)
      write.table(t(explPat[,,q]),col.names=FALSE, row.names=FALSE,file=out,append=TRUE)
    }
    doWriteExplPattern<<-FALSE
  }  
  
  # run the script
  if (interactive() && OS == "windows") {
    shell(cmd)
  } else {
    if (OS == "unix") system(cmd)
    
    #message("Skipping system command in hosted environment")
  }
  
  doRunModel<<-FALSE
  
  #read the results
  if (readResSimple) {
    a<-read.table(file.path(data_dir,'op_condensed.out'),header=T)
    a<-data.frame(Species.n=a$Species.n,Yield=a$yield*plotUnits['Yield'],Fbar=a$Fbar*plotUnits['Fbar'], SSB=a$SSB*plotUnits['SSB'], TSB=a$TSB*plotUnits['TSB'],Recruits=a$recruit*plotUnits['Recruits'])
    
    
    #format for ggradar
    b<-t(a)
    colnames(b)<-VPA.spNames
    b<-mutate(as_tibble(b),variable=rownames(b)) %>%select(c("variable",all_of(VPA.spNames))) %>%subset(variable!="Species.n") 
    a$Species<-VPA.spNames
    a<-select(as_tibble(a),Species,Yield,Fbar,SSB,TSB,Recruits)
  } 
  
  if (readResDetails)  {
    d<-read.table(file.path(data_dir,'op_condensed_long.out'),header=T)
    d1<-data.frame(Year=d$Year, Species=spNames[d$Species.n], Species.n=d$Species.n,
                   Yield=d$yield*plotUnits['Yield'],yield.core=d$CWsum.core*plotUnits['Yield'],
                   Fbar=d$Fbar*plotUnits['Fbar'], 
                   SSB=d$SSB*plotUnits['SSB'], TSB=d$TSB*plotUnits['SSB'],
                   Recruits=d$recruit*plotUnits['Recruits'], 
                   DeadM1=(d$DeadM-d$DeadM2)*plotUnits['DeadM'], DeadM2=d$DeadM2*plotUnits['DeadM'],
                   DeadM1_core=(d$DeadM_core-d$DeadM2_core)*plotUnits['DeadM'], DeadM2_core=d$DeadM2_core*plotUnits['DeadM'])
    if (recruitMode=='Stochastic'){
      a<-subset(d1,Year>=max(termYear+1,(termYear-10)),select= -Species)
      a<-aggregate(cbind(Yield,yield.core,Fbar,SSB,TSB,Recruits,DeadM1,DeadM2)~Species.n,data=d1,FUN=mean)
      #format for ggradar
      b<-t(a)
      colnames(b)<-VPA.spNames
      b<-mutate(as_tibble(b),variable=rownames(b)) %>%select(c("variable",all_of(VPA.spNames))) %>%subset(variable!="Species.n") 
      a$Species<-VPA.spNames
      a<-select(as_tibble(a),Species,Yield,yield.core,Fbar,SSB,TSB,Recruits)
    }
    d<-read.table(file.path(data_dir,'op_anno_M.out'),header=T)
    d2<-data.frame(Year=d$Year, Species=spNames[d$Species.n], Species.n=d$Species.n,Age=d$Age, M2=d$M2)
  } else {d1<-'no data'; d2<-'No data'}
  
  if (readResStom){
    s<-read.table(file.path(data_dir,'op_summary.out'),header=T)
    s$Species<-spNames[s$Species.n]
    s<-subset(s, select=c(Species,Year,Quarter,Species.n,Age,M1,M2,Nbar,N_prop_M2,west,CWsum.core))
    s<-data.frame(s,deadM1_core=s$M1*s$Nbar*s$west*s$N_prop_M2, 
                  deadM2_core=s$M2*s$Nbar*s$west*s$N_prop_M2, 
                  yield=s$CWsum.core)
    s<-subset(s,select=c(Species, Year, Quarter, Species.n, Age, M2,deadM1_core,deadM2_core,yield))
    
    #predator Residual mortality within model area 
    r<-data.frame(Predator=predPreyFormat[1], Year=s$Year,Predator.no=-1,Prey=s$Species,Prey.no=s$Species.n,eatenW=s$deadM1_core, stringsAsFactors = FALSE)
    r<-aggregate(r$eatenW,list(r$Predator,r$Predator.no,r$Year,r$Prey,r$Prey.no),sum)
    names(r)<-c("Predator","Predator.no","Year","Prey","Prey.no","eatenW")
    r$eatenW<-r$eatenW*plotUnits['DeadM']
    
    # predator humans  within model area 
    h<-data.frame(Predator=predPreyFormat[2], Year=s$Year,Predator.no=0,Prey=s$Species,Prey.no=s$Species.n,eatenW=s$yield, stringsAsFactors = FALSE)
    h<-aggregate(h$eatenW,list(h$Predator,h$Predator.no,h$Year,h$Prey,h$Prey.no),sum)
    names(h)<-c("Predator","Predator.no","Year","Prey","Prey.no","eatenW")
    h$eatenW<-h$eatenW*plotUnits['Yield']
    
    r<-bind_rows(r,h)
    
    M2<-read.table(file.path(data_dir,'op_part_m2.out'),header=T)
    M2$Area<-NULL
    M2<-data.frame(Predator=spNames[M2$Predator.no],Prey=spOtherNames[M2$Prey.no+1],M2) 
    
    M2<-merge(x=s,y=M2, by.x = c("Year","Quarter","Species","Age"), by.y = c("Year","Quarter","Prey","Prey.age"))
    M2$eatenW<- M2$deadM2*M2$Part.M2/M2$M2
    
    M2$Prey<-M2$Species
    M2$Prey.age<-M2$Age
    M2$tot.M2.prey<-M2$M2
    
    bbb<-droplevels(aggregate(list(eatenW=M2$eatenW),list(Year=M2$Year, Predator=M2$Predator,Prey=M2$Prey,Prey.no=M2$Prey.no),sum))
    bbb$eatenW<-bbb$eatenW*plotUnits['DeadM']
    
    s<-merge(x=bbb,y=pred_format,by.x='Prey',by.y='old',all.x=TRUE)
    s$Prey<-s$new; s$new<-NULL
    s$Prey.no<-s$new_no; s$new_no<-NULL
    s<-merge(x=s,y=pred_format,by.x='Predator',by.y='old',all.x=TRUE)
    s<-aggregate(s$eatenW,list(s$new,s$Year,s$new_no,s$Prey,s$Prey.no),sum)
    names(s)<-c("Predator","Year","Predator.no","Prey","Prey.no","eatenW")
    
    s<-bind_rows(s,r)
    
    # make unique format/factors  for predator and preys
    prey<-unique(data.frame(no=s$Prey.no,Species=s$Prey, stringsAsFactors = FALSE))
    pred<-unique(data.frame(no=s$Predator.no,Species=s$Predator, stringsAsFactors = FALSE))
    
    
    prey<-prey[order(prey$no,decreasing = FALSE),]
    prey<-prey$Species
    
    pred<-pred[order(pred$no,decreasing = FALSE),]
    pred<-pred$Species
    
    
    s<- mutate(as_tibble(s),Predator=parse_factor(Predator,levels=predPreyFormat),Prey=parse_factor(Prey,levels=predPreyFormat))
    
    predPrey<-lapply(pred,function(x) {a<-filter(s,Predator==x) %>% distinct(Prey);as.character(unlist(a))})
    names(predPrey)<-pred
    
    
  } else  { s<-'No data';pred<-'No data'; prey<-'No data'; predPrey<-'No data'}
  return(list(options=list(readResSimple=readResSimple,readResDetails=readResDetails,readResStom=readResStom,source=source),
              a=a,b=b,detail_sum=d1,detail_M2=d2,detail_eaten=s,pred=pred,prey=prey,predPrey=predPrey))
}
