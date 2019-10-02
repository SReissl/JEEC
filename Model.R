runModel<-function(seed=1,nB=12,Time = 1200,nrun=4,params=params,inits=inits,bankinits=bankinits,foldername="Rundata/"){
  library(dyn)
  library(abind)
  #library(tictoc)
  #tic()
  ###Argument list
  args<-as.data.frame(array(data=0,dim=c(1,5),dimnames=list(NULL,c("Timer","i","nB","Time","nrun"))))
  args$Timer=1
  args$i=0
  args$nB=nB
  args$Time=Time
  args$nrun=nrun

  #####Sectoral Structure####
  
  # HH column names
  HNames<-c("sav_h","V_h","D_h","YD","M","YD_e","c_d","rr_h","rr_he","alpha1","alpha2","M_d","H_dn","H_def","p_h","H","lev_h","iM","iD_h","M_sup","rep_m","M_np","TAA_h","W","u_nw","bal_h","MH","deltaM","V_he","r_mave","gb_h","gb_d","YD_tax","r_mavn","yd_e","v_he","pi_eh","LTV","LTVt","LTVgap")
  Households<-array(data = NA, dim=c(2,length(HNames)),dimnames = list(NULL,HNames))
  
  # Firm column names
  FNames<-c("sav_f","V_f","D_f","K","k","L","iD_f","p","theta","UC","WB","y","c","C","N_d","Y","I","i_d","i_des","lev_f","i","L_d","u","u_e","u_e2","Pr_f","sav_ft","div_f","rep_L","L_np","iL","bal_f","yfc","meanlev","delta_k","r_Lave","replacegap","u_n","r_Lavn","gk_des","UIC")
  Firms<-array(data = NA, dim=c(2,length(FNames)),dimnames = list(NULL,FNames))
  
  # Bank column names (abm)
  bNames<-c("sav_b","pr_b","v_b","v_bb","a_b","r_b","iM_b","iL_b","iD_hb","iD_fb","D_fb","iIB_b","M_b","M_supb","rep_mb","M_npb","L_b","L_supb","rep_Lb","L_npb", "clear_b","clearalt_b","R_pb","R_paltb","R_tb","R_anb","IBL_sb","IBA_db","IBL_b","IBA_b","R_lb","div_bt","div_b","bb_b","r_Lb","r_Mb","r_db","CARb","LCRb","v_bbt","bal_bb","ir_b","ia_b","D_hb","carshare","rdshare","rlshare","rmshare","M_db","L_db","bust","IBSshare","IBDshare","rationM","rationL","rLrel","Llag","rMrel","Mlag","rDrel","Dhlag","Dflag","defrandh","defrandf","detshareL","detshareM","detshareDh","detshareDf","randshareL","randshareM","randshareDh","randshareDf","defrateM","defrateL","markup_M","markup_L","RWA_gap","clear","default_M","default_L","v_bbe","v_bbe1","v_bbe2","v_bbe3","v_bbe4","fitvbb1","fitvbb2","fitvbb3","fitvbb4","maxfitvbb")
  # Bank column names (macro)
  BNames<-c("sav_B","Pr_b","V_b","V_bb","IBS","IBD","IBD_p","IBS_p","CAR","L_sup","Div_b","r_dav","r_IB","bal_b1","bal_b2","r_mav","r_Lav","Bust","r_mavr","r_Lavr")
  banks<-array(data = NA,dim=c(2,length(bNames),args$nB),dimnames = list(NULL,bNames,NULL))
  Banks<-array(data = NA, dim=c(2,length(BNames)),dimnames = list(NULL,BNames))
  
  # gov column names
  GNames<-c("PSBR","sav_g","V_g","r_gb","Tax","g","g_des","G","gb_s","gb","rep_gb","rep_gbcb","rep_gbh","bal_g","iGB","iGB_h","iGB_cb","tau","FTax","gap","gapr")
  Gov<-array(data = NA, dim=c(2,length(GNames)),dimnames = list(NULL,GNames))
  
  # CB column names
  CBNames<-c("PCB","sav_cb","V_cb","A","pi_a","pi_m","pi_q","pi_e","r_cbd","r_cbl","R","gb_rcb","gb_cb","R_t","gb_dcb","gb_scb","buff_h","R_pd","cbint","CARt","bal_cb","u_ecb","iA","iR","pi_sa","targetCAR","CARgap")
  CB<-array(data = NA, dim=c(2,length(CBNames)),dimnames = list(NULL,CBNames))
  
  # auxiliary stuff
  AuxNames<-c("porthgb","SFCcheck1","SFCcheck2","SFCcheck3","sdev_deff","sdev_defh","SFCcheck4")
  Aux<-array(data = NA, dim=c(2,length(AuxNames)),dimnames = list(NULL,AuxNames))
  
  #TS needed for estimations
  Estnames<-c("defrateM","defrateL","r_Lb","r_Mb","iL_b","iM_b","L_b","M_b","v_bb","v_bbe1","v_bbe2","v_bbe3","v_bbe4","CARb_e1","CARb_e2","CARb_e3","CARb_e4","v_bbe","clear_b","v_bbt","r_Lav","r_mav")
  Est<-array(data=NA,dim=c(args$Time+48,length(Estnames),args$nB),dimnames=list(NULL,Estnames,NULL))
  
  #TS needed for lags
  Lagnames<-c("iL","iD_f","y","k","Pr_f","sav_ft","c_d","YD_tax","PSBR","p","L_np","L","M_np","M","W","UIC","i_d","r_cbl","r_cbd","r_IB","lev_f","p_h","YD","V_h","r_mavr","rr_h","pi_sa","u","r_Lavr")
  Lags<-array(data=NA,dim=c(args$Time+48,length(Lagnames)),dimnames=list(NULL,Lagnames))
  
  if(params[1,"fulloutput"]==1){
  #####Data Frames#####
  datanames<-c(HNames,FNames,GNames,CBNames,BNames,AuxNames)
  #weekly data
  weekoutput<-as.data.frame(array(data = NA, dim=c((args$Time/args$nrun+48),length(datanames)),dimnames = list(NULL,datanames)))
  bankoutput<-array(data = NA,dim=c((args$Time/args$nrun+48),length(bNames),args$nB),dimnames = list(NULL,bNames,NULL))
  }else{
  datanames<-c("y", "c", "i", "p")
  output<-as.data.frame(array(data = NA, dim=c((args$Time/args$nrun+48),length(datanames)),dimnames = list(NULL,datanames)))
  }
  
  ######THIS IS THE MONTE CARLO REPETITION#####
    #setting the seed for reproducibility
    set.seed(seed)
    if(args$nB>1){
    drawBank<-c(rep(NA,args$nB/4))
    }else{
    drawBank<-1
    }
    rLrel<-c(rep(NA,args$nB))
    rDrel<-c(rep(NA,args$nB))
    rMrel<-c(rep(NA,args$nB))
    Dhlag<-c(rep(NA,args$nB))
    Dflag<-c(rep(NA,args$nB))
    Mlag<-c(rep(NA,args$nB))
    Llag<-c(rep(NA,args$nB))
    Hrandm<-rnorm(args$nB,1,params[[1,"sdev_dis"]])
    Hrandd<-rnorm(args$nB,1,params[[1,"sdev_dis"]])
    Frand<-rnorm(args$nB,1,params[[1,"sdev_dis"]])
    Frandd<-rnorm(args$nB,1,params[[1,"sdev_dis"]])
    
    library(MASS)

    mu <- rep(0,args$nB)
    Sigma <- matrix(params[1,"CC_def"], nrow=args$nB, ncol=args$nB) + diag(args$nB)*params[1,"CC_def"]
 
    rawvarsh <- mvrnorm(n=Time+48, mu=mu, Sigma=Sigma)
    rawvarsf <- mvrnorm(n=Time+48, mu=mu, Sigma=Sigma)
    
    pvarsh <- pnorm(rawvarsh)
    pvarsf <- pnorm(rawvarsf)
    
    
  #######Initial values####
  CB[1,"CARt"]<-params[1,"CARt0"]
  Households[1,"D_h"]<-inits[[1,"D_h"]]
  Firms[1,"D_f"]<-inits[1,"D_f"]
  Households[1,"M"]<-inits[[1,"M"]]
  Households[1,"p_h"]<-inits[[1,"p_h"]]
  Firms[1,"L"]<-inits[[1,"L"]]
  Firms[1,"k"]<-inits[[1,"k"]]
  Firms[1,"p"]<-inits[[1,"p"]]
  Households[1,"V_h"]<-inits[[1,"V_h"]]
  Households[1,"c_d"]<-inits[[1,"c_d"]]
  Households[1,"alpha1"]<-inits[[1,"alpha1"]]
  Households[1,"alpha2"]<-inits[[1,"alpha2"]]
  Households[1,"H_dn"]<-inits[[1,"H_dn"]]
  Firms[1,"u"]<-inits[[1,"u"]]
  Households[1,"W"]<-inits[[1,"W"]]
  Firms[1,"theta"]<-inits[[1,"theta"]]
  Firms[1,"lev_f"]<-inits[[1,"lev_f"]]
  Firms[1,"yfc"]<-inits[[1,"yfc"]]
  Firms[1,"y"]<-inits[[1,"y"]]
  Firms[1,"Pr_f"]<-inits[[1,"Pr_f"]]
  Firms[1,"div_f"]<-inits[[1,"div_f"]]
  Firms[1,"sav_ft"]<-inits[[1,"sav_ft"]]
  Banks[1,"r_dav"]<-inits[[1,"r_dav"]]
  Households[1,"TAA_h"]<-inits[[1,"TAA_h"]]
  Aux[1,"porthgb"]<-inits[[1,"porthgb"]]
  Households[1,"gb_h"]<-inits[[1,"gb_h"]]
  Gov[1,"g_des"]<-params[1,"g0"]/48
  Gov[1,"Tax"]<-inits[[1,"Tax"]]
  Gov[1,"tau"]<-params[[1,"tau"]]
  CB[1,"gb_cb"]<-inits[[1,"gb_cb"]]
  Gov[1,"gb"]<-inits[[1,"gb"]]
  Gov[1,"r_gb"]<-inits[[1,"r_gb"]]
  CB[1,"pi_sa"]<-inits[[1,"pi_q"]]
  CB[1,"r_cbd"]<-inits[[1,"r_cbd"]]
  CB[1,"r_cbl"]<-inits[[1,"r_cbl"]]
  CB[1,"buff_h"]<-inits[[1,"buff_h"]]
  Banks[1,"r_IB"]<-inits[[1,"r_IB"]]
  Firms[1,"L_d"]<-inits[[1,"L_sup"]]
  Households[1,"M_d"]<-inits[[1,"M_sup"]]
  Households[1,"YD_e"]<-inits[[1,"YD_e"]]
  Households[1,"yd_e"]<-inits[[1,"YD_e"]]
  Households[1,"v_he"]<-inits[[1,"V_h"]]
  Households[1,"YD"]<-inits[[1,"YD"]]
  Households[1,"rr_he"]<-inits[[1,"rr_he"]]
  Households[1,"rr_h"]<-inits[[1,"rr_h"]]
  Firms[1,"c"]<-inits[[1,"c"]]
  Firms[1,"u_e"]<-inits[[1,"u_e"]]
  Firms[1,"u_e2"]<-inits[[1,"u_e"]]
  CB[1,"u_ecb"]<-inits[[1,"u_ecb"]]
  CB[1,"pi_e"]<-inits[[1,"pi_e"]]
  Banks[1,"r_mav"]<-bankinits[[1,"r_Mb"]]
  Banks[1,"r_Lav"]<-bankinits[[1,"r_Lb"]]
  Banks[1,"r_mavr"]<-bankinits[[1,"r_Mb"]]
  Banks[1,"r_Lavr"]<-bankinits[[1,"r_Lb"]]
  Households[1,"V_he"]<-inits[[1,"V_h"]]
  Households[1,"r_mave"]<-bankinits[[1,"r_Mb"]]
  Firms[1,"r_Lave"]<-bankinits[[1,"r_Lb"]]
  Households[1,"YD_tax"]<-inits[[1,"YD_tax"]]
  Firms[1,"u_n"]<-inits[1,"u"]
  Gov[1,"PSBR"]<-0
  Firms[1,"replacegap"]<-0
  Firms[1,"r_Lavn"]<-bankinits[1,"r_Lb"]
  Households[1,"r_mavn"]<-bankinits[1,"r_Mb"]
  Firms[1,"gk_des"]<-0
  Firms[1,"UIC"]<-inits[1,"UIC"]
  Firms[1,"iL"]<-inits[1,"iL"]
  Firms[1,"iD_f"]<-bankinits[1,"r_db"]*inits[1,"D_f"]/48
  Households[1,"pi_eh"]<-0
  Households[1,"LTV"]<-params[1,"LTV"]
  CB[1,"targetCAR"]<-0.1
  Gov[1,"g"]<-params[1,"g0"]/48
  Households[1,"LTVt"]<-params[1,"LTV"]
  Gov[1,"gap"]<-0
  Gov[1,"gapr"]<-0
  Firms[1,"i_d"]<-params[1,"delta_k"]*inits[1,"k"]
  Households[1,"LTVgap"]<-0
  CB[1,"CARgap"]<-0
  Firms[1,"K"]<-inits[[1,"K"]]
  Lags[1,"iL"]<-inits[1,"iL"]
  Lags[1,"iD_f"]<-bankinits[1,"r_db"]*inits[1,"D_f"]/48
  Lags[1,"y"]<-inits[1,"y"]
  Lags[1,"k"]<-inits[1,"k"]
  Lags[1,"Pr_f"]<-inits[1,"Pr_f"]
  Lags[1,"sav_ft"]<-inits[1,"sav_ft"]
  Lags[1,"c_d"]<-inits[1,"c_d"]
  Lags[1,"YD_tax"]<-inits[1,"YD_tax"]
  Lags[1,"PSBR"]<-0
  Lags[1,"p"]<-inits[1,"p"]
  Lags[1,"L"]<-inits[1,"L"]
  Lags[1,"M"]<-inits[1,"M"]
  Lags[1,"W"]<-inits[1,"W"]
  Lags[1,"UIC"]<-inits[1,"UIC"]
  Lags[1,"i_d"]<-params[1,"delta_k"]*inits[1,"k"]
  Lags[1,"r_cbl"]<-inits[1,"r_cbl"]
  Lags[1,"r_cbd"]<-inits[1,"r_cbd"]
  Lags[1,"r_IB"]<-inits[1,"r_IB"]
  Lags[1,"lev_f"]<-inits[1,"lev_f"]
  Lags[1,"p_h"]<-inits[1,"p_h"]
  Lags[1,"YD"]<-inits[1,"YD"]
  Lags[1,"V_h"]<-inits[1,"V_h"]
  Lags[1,"r_mavr"]<-bankinits[1,"r_Mb"]
  Lags[1,"rr_h"]<-inits[1,"rr_h"]
  Lags[1,"pi_sa"]<-inits[1,"pi_q"]
  Lags[1,"u"]<-inits[1,"u"]
  Lags[1,"r_Lavr"]<-bankinits[1,"r_Lb"]
  
  
  ##bankinits
  banks[1,"D_fb",]<-bankinits[,"D_fb"]
  banks[1,"r_Lb",]<-bankinits[,"r_Lb"]
  banks[1,"r_Mb",]<-bankinits[,"r_Mb"]
  banks[1,"r_db",]<-bankinits[,"r_db"]
  banks[1,"D_hb",]<-bankinits[,"D_hb"]
  banks[1,"L_b",]<-bankinits[,"L_b"]
  banks[1,"M_b",]<-bankinits[,"M_b"]
  banks[1,"a_b",]<-bankinits[,"a_b"]
  banks[1,"r_b",]<-bankinits[,"r_b"]
  banks[1,"IBL_b",]<-bankinits[,"IBL_b"]
  banks[1,"IBA_b",]<-bankinits[,"IBA_b"]
  banks[1,"L_supb",]<-bankinits[,"L_supb"]
  banks[1,"L_db",]<-bankinits[,"L_db"]
  banks[1,"M_supb",]<-bankinits[,"M_supb"]
  banks[1,"M_db",]<-bankinits[,"M_db"]
  banks[1,"v_bb",]<-bankinits[,"v_bb"]
  banks[1,"v_bbt",]<-bankinits[,"v_bb"]
  banks[1,"defrandh",]<-1
  banks[1,"defrandf",]<-1
  banks[1,"v_bb",]<-bankinits[,"v_bb"]
  banks[1,"div_bt",]<-bankinits[,"div_b"]
  banks[1,"bb_b",]<-bankinits[,"bb_b"]
  banks[1,"clear_b",]<-bankinits[,"clear_b"]
  banks[1,"defrateM",]<-bankinits[,"defrateM"]
  banks[1,"defrateL",]<-bankinits[,"defrateL"]
  banks[1,"markup_M",]<-bankinits[,"markup_M"]
  banks[1,"markup_L",]<-bankinits[,"markup_L"]
  banks[1,"v_bbe",]<-bankinits[,"v_bbe"]
  banks[1,"RWA_gap",]<-0
  banks[1,"v_bbe1",]<-bankinits[,"v_bbe1"]
  banks[1,"v_bbe2",]<-bankinits[,"v_bbe2"]
  banks[1,"v_bbe3",]<-bankinits[,"v_bbe3"]
  banks[1,"v_bbe4",]<-bankinits[,"v_bbe4"]
  
  Est[1,"defrateM",]<-bankinits[,"defrateM"]
  Est[1,"defrateL",]<-bankinits[,"defrateL"]
  Est[1,"v_bb",]<-bankinits[,"v_bb"]
  Est[1,"v_bbe1",]<-bankinits[,"v_bbe1"]
  Est[1,"v_bbe2",]<-bankinits[,"v_bbe2"]
  Est[1,"v_bbe3",]<-bankinits[,"v_bbe3"]
  Est[1,"v_bbe4",]<-bankinits[,"v_bbe4"]
  Est[1,"v_bbe",]<-bankinits[,"v_bbe1"]
  Est[1,"v_bbt",]<-bankinits[,"v_bb"]
  Est[1,"r_Lb",]<-bankinits[,"r_Lb"]
  Est[1,"r_Mb",]<-bankinits[,"r_Mb"]
  Est[1,"r_Lav",]<-bankinits[,"r_Lb"]
  Est[1,"r_mav",]<-bankinits[,"r_Mb"]
  Est[1,"iL_b",]<-bankinits[1,"r_Lb"]*bankinits[1,"L_b"]/48
  Est[1,"iM_b",]<-bankinits[1,"r_Mb"]*bankinits[1,"M_b"]/48
  Est[1,"clear_b",]<-bankinits[,"clear_b"]
  Est[1,"L_b",]<-bankinits[,"L_b"]
  Est[1,"M_b",]<-bankinits[,"M_b"]
  
  
    #####THIS IS THE PERIOD REPETITION WITHIN 1 MONTE CARLO SIMULATION#####
    r=1
    args$Timer=1
    for(r in 1:args$nrun){
    if(r>1){
    marker=48
    datatimer=48
    if(params[1,"fulloutput"]==1){
    weekoutput[1:48,]<-weekoutput[(args$Time/args$nrun+1):(args$Time/args$nrun+48),]
    weekoutput[49:(args$Time/args$nrun+48),]<-NA
    bankoutput[1:48,,]<-bankoutput[(args$Time/args$nrun+1):(args$Time/args$nrun+48),,]
    bankoutput[49:(args$Time/args$nrun+48),,]<-NA
    }else{
    output[1:48,]<-output[(args$Time/args$nrun+1):(args$Time/args$nrun+48),]
    output[49:(args$Time/args$nrun+48),]<-NA
    }
    }else{
    marker=1
    datatimer=1
    }
    for(t in marker:(args$Time/args$nrun+47)){
      args$Timer=args$Timer+1
      #print(args$Timer)
      datatimer=datatimer+1
      banks<-bankreps(banks=banks,params=params,args=args)
      indreturns<-defindicators(Firms=Firms,Households=Households,params=params,args=args,Lags=Lags)
      Households<-indreturns$Households
      Firms<-indreturns$Firms
      defreturns<-defaults(banks=banks,Firms=Firms,Households=Households,Aux=Aux,params=params,args=args,pvarsh=pvarsh,pvarsf=pvarsf)
      banks<-defreturns$banks
      Aux<-defreturns$Aux
      banks<-rationindicators(banks=banks,Firms=Firms,Households=Households,params=params,args=args)
      banks<-bankinterest(banks=banks,CB=CB,Banks=Banks,params=params,args=args)
      CB<-cbinterest(banks=banks,CB=CB,params=params,args=args)
      Firms<-fbankpayments(Firms=Firms,banks=banks,params=params,args=args)
      Firms<-Fexp(Firms=Firms,Banks=Banks,params=params,args=args,Lags=Lags)
      Firms<-fullcapy(Firms=Firms,params=params,args=args,Lags=Lags)
      banks<-bankexp(banks=banks,Est=Est,CB=CB,params=params,args=args)
      Households<-Hexp(Firms=Firms,CB=CB,Households=Households,Banks=Banks,params=params,args=args,Lags=Lags)
      Households<-setwage(Firms=Firms,CB=CB,Households=Households,params=params,args=args)
      pricingreturn<-setpricemkup(Firms=Firms,Aux=Aux,Households=Households,params=params,args=args,Lags=Lags)
      Firms<-pricingreturn$Firms
      Aux<-pricingreturn$Aux
      Lags<-pricingreturn$Lags
      CB<-infmeasures(Firms=Firms,CB=CB,params=params,args=args,Lags=Lags)
      avrates<-averagerates(banks=banks,Banks=Banks,CB=CB,params=params,args=args)
      Banks<-avrates$Banks
      banks<-avrates$banks
      Households<-updateMPC(Households=Households,params=params,args=args)
      CB<-CBexp(CB=CB,Firms=Firms,params=params,args=args,Lags=Lags)
      Households<-decideConsumption(Households=Households,Firms=Firms,params=params,args=args)
      Households<-ndemandhouse(Households=Households,Banks=Banks,params=params,args=args,Lags=Lags)
      Firms<-decideInvestment(Firms=Firms,Banks=Banks,params=params,args=args)
      Households<-mortdeppayments(Households=Households,banks=banks,params=params,args=args)
      Gov<-gbint(Gov=Gov,CB=CB,Households=Households,params=params,args=args)
      Gov<-decidetax(Gov=Gov,Firms=Firms,Households=Households,Banks=Banks,params=params,args=args,Lags=Lags)
      CB<-CBprofit(Gov=Gov,CB=CB,params=params,args=args)
      Gov<-decidegovspend(Households=Households,Firms=Firms,CB=CB,Gov=Gov,params=params,args=args,Lags=Lags)
      CB<-setcbrate(CB=CB,Firms=Firms,Households=Households,params=params,args=args,Lags=Lags)
      CB<-targetratios(CB=CB,Lags=Lags,params=params,args=args)
      estreturn<-bankestimates1(banks=banks,Est=Est,Firms=Firms,Households=Households,params=params,args=args,drawBank=drawBank)
      banks<-estreturn$banks
      drawBank<-estreturn$drawBank
      Est<-estreturn$Est
      est2return<-bankestimates2(banks=banks,CB=CB,Est=Est,Firms=Firms,Households=Households,params=params,args=args,drawBank=drawBank,Lags=Lags)
      banks<-est2return$banks
      Lags<-est2return$Lags
      ratereturn<-setbankrates(banks=banks,CB=CB,Banks=Banks,Firms=Firms,Households=Households,params=params,args=args,drawBank=drawBank,Lags=Lags,Est=Est)
      banks<-ratereturn$banks
      Banks<-ratereturn$Banks
      Firms<-tsave(Firms=Firms,params=params,args=args)
      Firms<-floandemand(Firms=Firms,params=params,args=args)
      mdisreturn<-Mdistr(banks=banks,Households=Households,params=params,args=args,Hrandm=Hrandm)
      banks<-mdisreturn$banks
      Hrandm<-mdisreturn$Hrandm
      ldisreturn<-Ldistr(banks=banks,Firms=Firms,Banks=Banks,params=params,args=args,Frand=Frand)
      banks<-ldisreturn$banks
      Frand<-ldisreturn$Frand
      banks<-rationcredit(banks=banks,Firms=Firms,CB=CB,params=params,args=args)
      banks<-loanstocks(banks=banks,CB=CB,params=params,args=args)
      Banks<-lsupply(banks=banks,Banks=Banks,params=params,args=args)
      Households<-newmort(Households=Households,banks=banks,params=params,args=args)
      Firms<-actuali(Firms=Firms,Banks=Banks,params=params,args=args)
      Firms<-decidey(Firms=Firms,Gov=Gov,Households=Households,params=params,args=args)
      returnnominal<-nominalcigy(Firms=Firms,Gov=Gov,params=params,args=args)
      Firms<-returnnominal$Firms
      Gov<-returnnominal$Gov
      Firms<-Fprofit(Firms=Firms,Gov=Gov,params=params,args=args)
      fdivreturns<-divf(Firms=Firms,params=params,args=args,Lags=Lags)
      Firms<-fdivreturns$Firms
      Lags<-fdivreturns$Lags
      Firms<-FSave(Firms=Firms,Gov=Gov,params=params,args=args)
      Firms<-Capital(Firms=Firms,params=params,args=args)
      Firms<-Floanswealth(Firms=Firms,Banks=Banks,params=params,args=args)
      Dfdisreturn<-Dfdistr(banks=banks,Firms=Firms,params=params,args=args,Frandd=Frandd)
      banks<-Dfdisreturn$banks
      Frandd<-Dfdisreturn$Frandd
      Households<-efdemandhouse(Households=Households,params=params,args=args)
      banks<-bprofit(CB=CB,banks=banks,Banks=Banks,params=params,args=args)
      Banks<-Bprofit(banks=banks,Banks=Banks,params=params,args=args)
      divreturn<-bankdivs(banks=banks,Banks=Banks,CB=CB,params=params,args=args,Est=Est)
      Banks<-divreturn$Banks
      banks<-divreturn$banks
      Est<-divreturn$Est
      Banks<-Bankdivs(banks=banks,Banks=Banks,params=params,args=args)
      Gov<-Borrowreq(Gov=Gov,CB=CB,params=params,args=args)
      Gov<-supplygb(Gov=Gov,CB=CB,params=params,args=args)
      Gov<-setgbrate(Gov=Gov,CB=CB,Households=Households,Aux=Aux,Banks=Banks,params=params,args=args)
      hportreturn<-Hportfolio(Households=Households,Gov=Gov,Banks=Banks,Aux=Aux,params=params,args=args)
      Households<-hportreturn$Households
      Aux<-hportreturn$Aux
      CB<-monetisedef(CB=CB,Gov=Gov,Households=Households,params=params,args=args)
      Gov<-gbstock(Gov=Gov,Households=Households,CB=CB,params=params,args=args)
      Gov<-Gwealth(Gov=Gov,params=params,args=args)
      banks<-bsave(CB=CB,banks=banks,params=params,args=args)
      Banks<-Bsave(banks=banks,Banks=Banks,params=params,args=args)
      banks<-IB1(banks=banks,params=params,args=args)
      CB<-Rtarget(CB=CB,banks=banks,params=params,args=args)
      Households<-Hidentities1(Households=Households,Banks=Banks,CB=CB,Firms=Firms,Gov=Gov,params=params,args=args)
      CB<-CBsolvecyc(CB=CB,banks=banks,Banks=Banks,Households=Households,Gov=Gov,params=params,args=args)
      CB<-intervenegb(CB=CB,params=params,args=args)
      CB<-gbcbstock(CB=CB,Gov=Gov,params=params,args=args)
      Households<-hbonds(Households=Households,Gov=Gov,CB=CB,params=params,args=args)
      Households<-Hidentities2(Households=Households,Banks=Banks,CB=CB,Firms=Firms,Gov=Gov,params=params,args=args)
      dhdisreturn<-Dhdistr(banks=banks,Households=Households,params=params,args=args,Hrandd=Hrandd)
      banks<-dhdisreturn$banks
      Hrandd<-dhdisreturn$Hrandd
      Households<-TAAlevh(Households=Households,Banks=Banks,params=params,args=args)
      banks<-IB2(banks=banks,params=params,args=args)
      IB3return<-IB3(banks=banks,Banks=Banks,CB=CB,params=params,args=args)
      banks<-IB3return$banks
      Banks<-IB3return$Banks
      banks<-badv(banks=banks,params=params,args=args)
      banks<-bres(banks=banks,params=params,args=args)
      banks<-bwealth(banks=banks,params=params,args=args)
      Banks<-Bwealth(banks=banks,Banks=Banks,params=params,args=args)
      banks<-regratios(banks=banks,params=params,args=args)
      Banks<-aggregateCAR(banks=banks,CB=CB,Banks=Banks,params=params,args=args)
      CB<-RAstock(CB=CB,banks=banks,params=params,args=args)
      CB<-CBwealth(CB=CB,banks=banks,params=params,args=args)
      Households<-hhreturns(Households=Households,Gov=Gov,Banks=Banks,CB=CB,params=params,args=args)
      SFBreturns<-balances(Households=Households,Firms=Firms,Gov=Gov,CB=CB,banks=banks,Banks=Banks,Aux=Aux,params=params,args=args)
      Households<-SFBreturns$Households
      Firms<-SFBreturns$Firms
      Gov<-SFBreturns$Gov
      CB<-SFBreturns$CB
      banks<-SFBreturns$banks
      Banks<-SFBreturns$Banks
      Aux<-SFBreturns$Aux
      datareturn<-collectdata(Est=Est,Lags=Lags,CB=CB,Aux=Aux,Firms=Firms,Banks=Banks,banks=banks,Households=Households,Gov=Gov,params=params,args=args)
      Est<-datareturn$Est
      Lags<-datareturn$Lags
      shiftreturn<-shiftindex(CB=CB,Aux=Aux,Firms=Firms,Banks=Banks,banks=banks,Households=Households,Gov=Gov)
      CB<-shiftreturn$CB
      Aux<-shiftreturn$Aux
      Firms<-shiftreturn$Firms
      Banks<-shiftreturn$Banks
      banks<-shiftreturn$banks
      Households<-shiftreturn$Households
      Gov<-shiftreturn$Gov
      if(params[1,"fulloutput"]==1){
      storereturn<-storedata(CB=CB,Aux=Aux,Firms=Firms,Banks=Banks,banks=banks,Households=Households,Gov=Gov,weekoutput=weekoutput,bankoutput=bankoutput,datatimer=datatimer)
      weekoutput<-storereturn$weekoutput
      bankoutput<-storereturn$bankoutput
      }else{
      output<-reduceddata(Firms=Firms,output=output,datatimer=datatimer)
      }
    }
    if(r==1){
      args$Timer=(args$Time/args$nrun+48)
      if(params[1,"fulloutput"]==1){
      weekoutputsel<-weekoutput
      bankoutputsel<-bankoutput
      }else{
      outputsel<-output
      }
    }else{
      if(params[1,"fulloutput"]==1){
      weekoutputsel<-rbind(weekoutputsel,weekoutput[49:(args$Time/args$nrun+48),])
      if(args$nB>1){
      bankoutputsel<-abind(bankoutputsel,bankoutput[49:(args$Time/args$nrun+48),,],along=1)
      }else{
      bankoutputsel<-rbind(bankoutputsel,bankoutput[49:(args$Time/args$nrun+48),,])
      }
      }else{
      outputsel<-rbind(outputsel,output[49:(args$Time/args$nrun+48),])
      }
    }
    }
  if(params[1,"fulloutput"]==1){
  Modelreturns<-list(weekoutput=weekoutputsel,bankoutput=bankoutputsel)
  }else{
  output<-outputsel
  Modelreturns<-output
  }
  filename<-paste("data","mc",seed,sep="")
  save(Modelreturns,file = paste(foldername,filename, ".Rdata", sep=''))
  #toc()
}



######Define functions here#####

######Accounting identities#####
#####Households####
Hidentities1<-function(Households=stop("need to have households defined!"),CB=stop("Need to have CB defined!"),Banks=stop("need to have Banks (agg) defined!"),Firms=stop("need to have firms defined!"),Gov=stop("need to have gov. defined!"),params=params,args=args){
  Households[[2,"YD"]]=Firms[[2,"WB"]]+Households[[2,"iD_h"]]+Firms[[2,"div_f"]]+Banks[[2,"Div_b"]]+Gov[[2,"iGB_h"]]-Gov[[2,"Tax"]]-Households[[2,"iM"]]+Households[[2,"M_np"]]
  Households[[2,"YD_tax"]]=Firms[[2,"WB"]]+Households[[2,"iD_h"]]+Firms[[2,"div_f"]]+Gov[[2,"iGB_h"]]+Banks[[2,"Div_b"]]
  Households[[2,"sav_h"]]=Firms[[2,"WB"]]+Households[[2,"iD_h"]]+Firms[[2,"div_f"]]+Banks[[2,"Div_b"]]+Gov[[2,"iGB_h"]]-Gov[[2,"Tax"]]-Households[[2,"iM"]]-Firms[[2,"C"]]
  Households[[2,"deltaM"]]=-Households[[2,"rep_m"]]-Households[[2,"M_np"]]+Households[[2,"M_sup"]]
  Households[[2,"M"]]=Households[[(1),"M"]]+Households[[2,"deltaM"]]
  Households[[2,"H"]]=Households[[2,"p_h"]]*params[[1,"h"]]
  return(Households)
}


Hidentities2<-function(Households=stop("need to have households defined!"),CB=stop("Need to have CB defined!"),Banks=stop("need to have Banks (agg) defined!"),Firms=stop("need to have firms defined!"),Gov=stop("need to have gov. defined!"),params=params,args=args){
  Households[[2,"D_h"]]=Households[[(1),"D_h"]]+Households[[2,"sav_h"]]+Households[[2,"M_sup"]]-Households[[2,"rep_m"]]-(Households[[2,"gb_h"]]-Households[[(1),"gb_h"]])
  Households[[2,"V_h"]]=Households[[2,"D_h"]]+Households[[2,"p_h"]]*params[[1,"h"]]-Households[[2,"M"]]+Households[[2,"gb_h"]]+params[[1,"bb"]]+params[[1,"E_f"]]
  return(Households)
}



#####Firms####
Fprofit<-function(Firms=stop("need to have firms defined!"),Gov=stop("need to have gov. defined!"),params=params,args=args){
  Firms[[2,"Pr_f"]]=Firms[[2,"C"]]+Firms[[2,"I"]]+Gov[[2,"G"]]+Firms[[2,"iD_f"]]-Firms[[2,"WB"]]-Firms[[2,"iL"]]+Firms[[2,"L_np"]]-Firms[[2,"delta_k"]]*Firms[[(1),"k"]]*Firms[[(1),"p"]]
  return(Firms)
}

FSave<-function(Firms=stop("need to have firms defined!"),Gov=stop("need to have gov. defined!"),params=params,args=args){
  Firms[[2,"sav_f"]]=Firms[[2,"C"]]+Gov[[2,"G"]]+Firms[[2,"I"]]+Firms[[2,"iD_f"]]-Firms[[2,"WB"]]-Firms[[2,"div_f"]]-Firms[[2,"iL"]]-Gov[[2,"FTax"]]
  return(Firms)
}

Floanswealth<-function(Firms=stop("need to have firms defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  Firms[[2,"D_f"]]=Firms[[(1),"D_f"]]+Firms[[2,"sav_f"]]+Banks[[2,"L_sup"]]-Firms[[2,"rep_L"]]-Firms[[2,"I"]]
  Firms[[2,"L"]]=Firms[[(1),"L"]]+Banks[[2,"L_sup"]]-Firms[[2,"rep_L"]]-Firms[[2,"L_np"]]
  Firms[[2,"V_f"]]=Firms[[2,"K"]]+Firms[[2,"D_f"]]-Firms[[2,"L"]]-params[[1,"E_f"]]
  Firms[[2,"lev_f"]]=Firms[[2,"L"]]/Firms[[2,"K"]]
  return(Firms)
}

Capital<-function(Firms=stop("need to have firms defined!"),params=params,args=args){
  Firms[[2,"k"]]=Firms[[(1),"k"]]+Firms[[2,"i"]]-Firms[[2,"delta_k"]]*Firms[[(1),"k"]]
  Firms[[2,"K"]]=Firms[[(1),"k"]]*Firms[[(1),"p"]]-Firms[[2,"delta_k"]]*Firms[[(1),"k"]]*Firms[[(1),"p"]]+(Firms[[2,"p"]]-Firms[[(1),"p"]])*(Firms[[(1),"k"]]-Firms[[2,"delta_k"]]*Firms[[(1),"k"]])+Firms[[2,"p"]]*Firms[[2,"i"]]
  return(Firms)
}

######Gov####
Borrowreq<-function(Gov=stop("need to have gov. defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
  Gov[[2,"PSBR"]]=Gov[[2,"G"]]+Gov[[2,"iGB"]]-CB[[2,"PCB"]]-Gov[[2,"Tax"]]-Gov[[2,"FTax"]]
  Gov[[2,"sav_g"]]=-Gov[[2,"PSBR"]]
  return(Gov)
}

Gwealth<-function(Gov=stop("need to have gov. defined!"),params=params,args=args){
  Gov[[2,"V_g"]]=-Gov[[2,"gb"]]
  return(Gov)
}


######CB####
CBprofit<-function(Gov=stop("need to have gov. defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
  CB[[2,"PCB"]]=Gov[[2,"iGB_cb"]]+CB[[2,"iA"]]-CB[[2,"iR"]]
  CB[[2,"sav_cb"]]=Gov[[2,"iGB_cb"]]+CB[[2,"iA"]]-CB[[2,"iR"]]-CB[[2,"PCB"]]
  return(CB)
}

CBwealth<-function(CB=stop("need to have CB defined!"),banks=stop("need to have banks defined!"),params=params,args=args){
  CB[[2,"V_cb"]]=CB[[2,"gb_cb"]]+CB[[2,"A"]]-CB[[2,"R"]]
  return(CB)
}


######Banks####
#ABM#####
bsave<-function(CB=stop("need to have CB defined!"),banks=stop("need to have banks defined!"),params=params,args=args){
    banks[2,"sav_b",]=banks[2,"iM_b",]+banks[2,"iL_b",]+banks[2,"iIB_b",]+banks[2,"ir_b",]-banks[2,"iD_hb",]-banks[2,"iD_fb",]-banks[2,"ia_b",]-banks[2,"div_b",]
  return(banks)
}

bprofit<-function(CB=stop("need to have CB defined!"),banks=stop("need to have banks defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
    banks[2,"pr_b",]=banks[2,"iM_b",]+banks[2,"iL_b",]+banks[2,"iIB_b",]+banks[2,"ir_b",]-banks[2,"iD_hb",]-banks[2,"iD_fb",]-banks[2,"ia_b",]-banks[2,"L_npb",]-banks[2,"M_npb",]
  return(banks)
}

bwealth<-function(banks=stop("need to have banks defined!"),params=params,args=args){
    banks[2,"v_b",]=banks[2,"r_b",]+banks[2,"L_b",]+banks[2,"M_b",]+banks[2,"IBL_b",]-banks[2,"D_hb",]-banks[2,"D_fb",]-banks[2,"IBA_b",]-banks[2,"a_b",]-banks[2,"bb_b",]
    banks[2,"v_bb",]=banks[2,"r_b",]+banks[2,"L_b",]+banks[2,"M_b",]+banks[2,"IBL_b",]-banks[2,"D_hb",]-banks[2,"D_fb",]-banks[2,"IBA_b",]-banks[2,"a_b",]
    banks[2,"bust",]=ifelse(banks[2,"v_bb",]<0,1,0)
  return(banks)
}


badv<-function(banks=stop("need to have banks defined!"),params=params,args=args){
    banks[2,"a_b",]=pmax(banks[2,"R_tb",]-banks[2,"R_lb",],0)
  return(banks)
}

bres<-function(banks=stop("need to have banks defined!"),params=params,args=args){
    banks[2,"r_b",]=banks[2,"R_lb",]+banks[2,"a_b",]
  return(banks)
}

#aggregate#####
Bsave<-function(banks=stop("need to have banks defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  Banks[[2,"sav_B"]]=sum(banks[2,"sav_b",])
  return(Banks)
}

Bprofit<-function(banks=stop("need to have banks defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  Banks[[2,"Pr_b"]]=sum(banks[2,"pr_b",])
  return(Banks)
}

Bwealth<-function(banks=stop("need to have banks defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  Banks[[2,"V_b"]]=sum(banks[2,"v_b",])
  Banks[[2,"V_bb"]]=sum(banks[2,"v_bb",])
  Banks[[2,"Bust"]]=sum(banks[2,"bust",])
  return(Banks)
}

#####sectoral balances####
balances<-function(Households=stop("need to have households defined!"),Firms=stop("need to have Firms defined!"),Gov=stop("need to have Gov defined!"),CB=stop("need to have CB defined!"),banks=stop("need to have banks (ABM) defined!"),Banks=stop("need to have Banks (aggregate) defined!"),Aux=stop("need to have auxiliary array defined!"),params=params,args=args){
  banks[2,"bal_bb",]=banks[2,"iM_b",]+banks[2,"iL_b",]+banks[2,"iIB_b",]+banks[2,"ir_b",]-banks[2,"iD_hb",]-banks[2,"iD_fb",]-banks[2,"ia_b",]-banks[2,"div_b",]
  Households[[2,"bal_h"]]=Firms[[2,"WB"]]+Households[[2,"iD_h"]]+Firms[[2,"div_f"]]-Firms[[2,"C"]]-Gov[[2,"Tax"]]-Households[[2,"iM"]]+Banks[[2,"Div_b"]]++Gov[[2,"iGB_h"]]
  Firms[[2,"bal_f"]]=Firms[[2,"C"]]+Gov[[2,"G"]]+Firms[[2,"iD_f"]]-Firms[[2,"WB"]]-Firms[[2,"div_f"]]-Firms[[2,"iL"]]
  Banks[[2,"bal_b1"]]=sum(banks[2,"bal_bb",])
  Banks[[2,"bal_b2"]]=Households[[2,"iM"]]+Firms[[2,"iL"]]-Banks[[2,"Div_b"]]-CB[[2,"iA"]]+CB[[2,"iR"]]-Households[[2,"iD_h"]]-Firms[[2,"iD_f"]]
  CB[[2,"bal_cb"]]=Gov[[2,"iGB_cb"]]+CB[[2,"iA"]]-CB[[2,"iR"]]-CB[[2,"PCB"]]
  Gov[[2,"bal_g"]]=Gov[[2,"Tax"]]+CB[[2,"PCB"]]-Gov[[2,"G"]]-Gov[[2,"iGB"]]
  Aux[[2,"SFCcheck1"]]=Households[[2,"bal_h"]]+Firms[[2,"bal_f"]]+Banks[[2,"bal_b1"]]+CB[[2,"bal_cb"]]+Gov[[2,"bal_g"]]
  Aux[[2,"SFCcheck2"]]=Households[[2,"V_h"]]+Firms[[2,"V_f"]]+Banks[[2,"V_b"]]+Gov[[2,"V_g"]]+CB[[2,"V_cb"]]-Households[[2,"p_h"]]*params[[1,"h"]]-Firms[[2,"p"]]*Firms[[2,"k"]]
  Aux[[2,"SFCcheck4"]]=Households[[2,"V_h"]]+Firms[[2,"V_f"]]+sum(banks[2,"v_b",])+Gov[[2,"V_g"]]+CB[[2,"V_cb"]]-Households[[2,"p_h"]]*params[[1,"h"]]-Firms[[2,"p"]]*Firms[[2,"k"]]
  Aux[[2,"SFCcheck3"]]=Banks[[2,"bal_b1"]]-Banks[[2,"bal_b2"]]
  return(list(Households=Households,Firms=Firms,banks=banks,Banks=Banks,Gov=Gov,CB=CB,Aux=Aux))
}

######
####Equations
####Households####
decideConsumption<-function(Households=stop("need to have households defined!"),Firms=stop("need to have Firms defined!"),params=params,args=args){
  Households[[2,"c_d"]]=Households[[(1),"c_d"]]+(Households[[2,"alpha1"]]*Households[[2,"yd_e"]]+Households[[2,"alpha2"]]/48*(Households[[(1),"V_h"]]/Firms[1,"p"])-Households[[(1),"c_d"]])/12
  return(Households)
}

updateMPC<-function(Households=stop("need to have households defined!"),params=params,args=args){
    Households[[2,"alpha1"]]=Households[[(1),"alpha1"]]+(params[[1,"alpha1L"]]+((params[[1,"alpha1U"]]-params[[1,"alpha1L"]])/(1+exp(params[[1,"sigmampc1"]]*Households[[2,"rr_he"]]-params[[1,"sigmampc2"]])))-Households[[(1),"alpha1"]])/24
    Households[[2,"alpha2"]]=Households[[(1),"alpha2"]]+(params[[1,"alpha2L"]]+((params[[1,"alpha2U"]]-params[[1,"alpha2L"]])/(1+exp(params[[1,"sigmampc1"]]*Households[[2,"rr_he"]]-params[[1,"sigmampc2"]])))-Households[[(1),"alpha2"]])/24
  return(Households)
}


hhreturns<-function(Households=stop("Need to have households defined!"),Gov=stop("Need to have Gov defined!"),Banks=stop("Need to have banks (aggregate) defined!"),CB=stop("Need to have CB defined!"),params=params,args=args){
  Households[[2,"rr_h"]]=((1+Banks[[2,"r_dav"]])/(1+CB[[2,"pi_q"]])-1)*Households[[2,"D_h"]]/(Households[[2,"TAA_h"]]+Households[[2,"p_h"]]*params[1,"h"])+((1+Gov[[2,"r_gb"]])/(1+CB[[2,"pi_q"]])-1)*Households[[2,"gb_h"]]/(Households[[2,"TAA_h"]]+Households[[2,"p_h"]]*params[1,"h"])+(((1+(Households[2,"p_h"]-Households[(1),"p_h"])/Households[(1),"p_h"])^48)/(1+CB[2,"pi_q"])-1)*(Households[[2,"p_h"]]*params[1,"h"])/(Households[[2,"TAA_h"]]+Households[[2,"p_h"]]*params[1,"h"])
  return(Households)
}

ndemandhouse<-function(Households=stop("need to have households defined!"),Banks=stop("Need to have banks (aggregate) defined!"),params=params,args=args,Lags=Lags){
  if(params[1,"polLTV"]==1){   
  if(args$Timer/4==round(args$Timer/4) && args$Timer>4){
    gph=(Lags[(args$Timer-1),"p_h"]-Lags[(args$Timer-12),"p_h"]/Lags[(args$Timer-12),"p_h"])^4-1
    gph=mean(Lags[(args$Timer-1):(args$Timer-4),"p_h"])-1
    Households[2,"LTVt"]=params[[1,"LTV"]]*(2/(1+exp(5*gph)))
    Households[2,"LTVgap"]=Households[2,"LTVt"]-Households[(1),"LTV"]
    Households[2,"LTV"]=Households[1,"LTV"]+(Households[2,"LTVgap"])/4
     }else{
    Households[2,"LTVt"]=Households[1,"LTVt"]
    Households[2,"LTVgap"]=Households[1,"LTVgap"]
    Households[2,"LTV"]=Households[1,"LTV"]+(Households[2,"LTVgap"])/4
     }}
  else{
  Households[2,"LTV"]=params[[1,"LTV"]]
  }
  Households[2,"r_mavn"]=Households[(1),"r_mavn"]
  Households[[2,"H_dn"]]=Households[[(1),"H_dn"]]+(((params[[1,"rho0"]]+params[[1,"rho2"]]*Households[2,"LTV"])/48+params[[1,"rho1"]]*(Households[[(1),"V_h"]]-((1-((Households[2,"alpha1"])))/((Households[2,"alpha2"]))*Households[[(2),"YD_e"]]*48))+params[[1,"rho3"]]*(Households[[2,"r_mavn"]]-Households[[2,"r_mave"]]))-Households[[(1),"H_dn"]])/12
  Households[[2,"M_d"]]=Households[2,"LTV"]*Households[[2,"H_dn"]]
  return(Households)
}

efdemandhouse<-function(Households=stop("need to have households defined!"),params=params,args=args){
  Households[[2,"H_def"]]=ifelse(Households[[2,"M_d"]]>Households[[2,"M_sup"]],Households[[2,"M_sup"]]+(1-Households[2,"LTV"])*Households[[2,"H_dn"]],Households[[2,"H_dn"]])
  Households[[2,"p_h"]]=Households[[2,"H_def"]]/((params[[1,"eta"]]/48)*params[[1,"h"]])
  return(Households)
}

TAAlevh<-function(Households=stop("need to have households defined!"),Banks=stop("Need to have Banks defined!"),params=params,args=args){
  Households[[2,"lev_h"]]=Households[[2,"M"]]/(Households[[2,"p_h"]]*params[[1,"h"]]+Households[[2,"D_h"]]+Households[[2,"gb_h"]])
  Households[[2,"TAA_h"]]=Households[[2,"D_h"]]+Households[[2,"gb_h"]]
  return(Households)
}

Hportfolio<-function(Households=stop("need to have households defined!"),Gov=stop("Need to have Gov defined!"),Banks=stop("Need to have banks (aggregate) defined!"),Aux=stop("Need to have auxiliary array defined!"),params=params,args=args){
    Aux[[2,"porthgb"]]=Aux[[(1),"porthgb"]]+(params[[1,"lambdah10"]]+params[[1,"lambdah11"]]*Gov[[2,"r_gb"]]-params[[1,"lambdah12"]]*Banks[[2,"r_dav"]]-Aux[[(1),"porthgb"]])/12
    Households[[2,"gb_d"]]=Aux[[2,"porthgb"]]*Households[[(1),"TAA_h"]]
  return(list(Households=Households,Aux=Aux))
}


hbonds<-function(Households=stop("need to have households defined!"),Gov=stop("Need to have Gov defined!"),CB=stop("Need to have CB defined!"),params=params,args=args){
  Households[[2,"gb_h"]]=Households[[(1),"gb_h"]]+Gov[[2,"gb_s"]]-CB[[2,"gb_rcb"]]-Gov[[2,"rep_gbh"]]-CB[[2,"gb_dcb"]]+CB[[2,"gb_scb"]]
  return(Households)
}

mortdeppayments<-function(Households=stop("Need to have households defined!"),banks=stop("Need to have banks (ABM) defined!"),params=params,args=args){
  Households[[2,"iM"]]<-sum(banks[2,"iM_b",])
  Households[[2,"M_np"]]<-sum(banks[2,"M_npb",])
  Households[[2,"rep_m"]]=sum(banks[2,"rep_mb",])
  Households[[2,"iD_h"]]=sum(banks[2,"iD_hb",])
  return(Households)
}

newmort<-function(Households=stop("Need to have households defined!"),banks=stop("Need to have banks defined!"),params=params,args=args){
  Households[[2,"M_sup"]]=sum(banks[2,"M_supb",])
  return(Households)
}

setwage<-function(Firms=stop("need to have firms defined!"),CB=stop("need to have CB defined!"),Households=stop("need to have households defined!"),params=params,args=args){
    Households[[2,"u_nw"]]=Firms[[2,"u_n"]]
    Households[[2,"W"]]=Households[[(1),"W"]]+((1+Households[(2),"pi_eh"])*(params[1,"W_n"]+(params[[1,"beta"]]*(Firms[[2,"u_e"]]-Households[[2,"u_nw"]])))-Households[[(1),"W"]])/24
    return(Households)
}


#####Firms#####

setpricemkup<-function(Firms=stop("need to have firms defined!"),Aux=stop("Need to have auxiliary array defined!"),Households=stop("Need to have Households defined!"),params=params,args=args,Lags=Lags){
    Lags[args$Timer,"iL"]<-Firms[2,"iL"]
    Lags[args$Timer,"iD_f"]<-Firms[2,"iD_f"]
    if(args$Timer<48){
    Firms[[2,"UIC"]]=Firms[[(1),"UIC"]]
    }else{
    Firms[[2,"UIC"]]=mean(Lags[(args$Timer-47):(args$Timer),"iL"]-Lags[(args$Timer-47):(args$Timer),"iD_f"])/((mean(Lags[(args$Timer-48):(args$Timer-1),"y"])))
    }
    Firms[[2,"theta"]]=Firms[[(1),"theta"]]
    Firms[[2,"p"]]=Firms[[(1),"p"]]+(((1+Firms[[2,"theta"]])*(Households[[2,"W"]]/params[[1,"alpha"]]+Firms[2,"UIC"]))-Firms[[(1),"p"]])/24
    return(list(Firms=Firms,Aux=Aux,Lags=Lags))
}


decideInvestment<-function(Firms=stop("need to have firms defined!"),Banks=stop("Need to have banks (aggregate) defined!"),params=params,args=args){
  Firms[2,"r_Lavn"]=Firms[(1),"r_Lavn"]
  Firms[2,"gk_des"]=Firms[(1),"gk_des"]+(params[1,"gamma1"]*(Firms[[2,"u_e2"]]-Firms[[2,"u_n"]])/Firms[[2,"u_n"]]+params[1,"gamma2"]*((Firms[[2,"r_Lavn"]])-(Firms[[2,"r_Lave"]]))/(Firms[[2,"r_Lavn"]])-Firms[(1),"gk_des"])/12
  Firms[[2,"i_des"]]=(1+Firms[2,"gk_des"])*Firms[[(1),"k"]]-Firms[[(1),"k"]]
  Firms[[2,"i_d"]]=max(0,Firms[[2,"delta_k"]]*Firms[[(1),"k"]]+Firms[[2,"i_des"]])
  return(Firms)
}

floandemand<-function(Firms=stop("need to have firms defined!"),params=params,args=args){
  Firms[[2,"L_d"]]=max(0,Firms[[2,"p"]]*Firms[[2,"i_d"]]-Firms[[2,"sav_ft"]])
  Firms[[2,"replacegap"]]=max(0,min(Firms[[2,"p"]]*Firms[[2,"i_d"]]-Firms[[2,"sav_ft"]],Firms[[2,"p"]]*Firms[[2,"delta_k"]]*Firms[[(1),"k"]]-Firms[[2,"sav_ft"]]))
  return(Firms)
}

actuali<-function(Firms=stop("need to have firms defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  Firms[[2,"i"]]=ifelse(Firms[[2,"L_d"]]>Banks[[2,"L_sup"]],max(0,(Firms[[2,"sav_ft"]]+Banks[[2,"L_sup"]])/Firms[[2,"p"]]),max(0,Firms[[2,"i_d"]]))
  return(Firms)
}

tsave<-function(Firms=stop("Need to have firms defined!"),params=params,args=args){
  Firms[[2,"sav_ft"]]=(Firms[[(1),"L"]]+Firms[[2,"p"]]*Firms[[2,"i_d"]]-Firms[[2,"L_np"]]-Firms[[2,"rep_L"]]-params[[1,"lev_ft"]]*(Firms[[(1),"K"]]-Firms[[(1),"p"]]*Firms[[2,"delta_k"]]*Firms[[(1),"k"]]+(Firms[[2,"p"]]-Firms[[(1),"p"]])*(Firms[[(1),"k"]]-Firms[[2,"delta_k"]]*Firms[[(1),"k"]])+Firms[[2,"p"]]*Firms[[2,"i_d"]]))/48
  return(Firms)
}

decidey<-function(Firms=stop("need to have firms defined!"),Gov=stop("need to have gov defined!"),Households=stop("need to have households defined!"),params=params,args=args){
  Firms[[2,"y"]]=min(Firms[[2,"yfc"]]/48,Households[[2,"c_d"]]+Firms[[2,"i"]]+Gov[[2,"g"]])
  Firms[[2,"N_d"]]=Firms[[2,"y"]]/params[[1,"alpha"]]
  Firms[[2,"WB"]]=Households[[2,"W"]]*Firms[[2,"N_d"]]
  Firms[[2,"u"]]=Firms[[2,"y"]]/(Firms[[2,"yfc"]]/48)
  Firms[[2,"UC"]]=Firms[[2,"WB"]]/Firms[[2,"y"]]
  Firms[[2,"c"]]=ifelse(Households[[2,"c_d"]]+Firms[[2,"i"]]+Gov[[2,"g"]]<=Firms[[2,"yfc"]]/48,Households[[2,"c_d"]],Firms[[2,"y"]]-Firms[[2,"i"]]-Gov[[2,"g"]])
  return(Firms)
}

fullcapy<-function(Firms=stop("need to have firms defined!"),params=params,args=args,Lags=Lags){
  if(args$Timer<4){
    Firms[[2,"yfc"]]=Firms[[(1),"yfc"]]
  }
  else{
    Firms[[2,"yfc"]]=Lags[[(args$Timer-3),"k"]]/params[[1,"kappa"]]
  }
  return(Firms)
}

nominalcigy<-function(Firms=stop("Need to have firms defined!"),Gov=stop("need to have gov defined!"),params=params,args=args){
  Firms[[2,"C"]]=Firms[[2,"c"]]*Firms[[2,"p"]]
  Firms[[2,"I"]]=Firms[[2,"i"]]*Firms[[2,"p"]]
  Firms[[2,"Y"]]=Firms[[2,"y"]]*Firms[[2,"p"]]
  Gov[[2,"G"]]=Gov[[2,"g"]]*Firms[[2,"p"]]
  return(list(Firms=Firms,Gov=Gov))
}

fbankpayments<-function(Firms=stop("Need to have firms defined!"),banks=stop("Need to have banks (ABM) defined!"),params=params,args=args){
  Firms[[2,"iD_f"]]=sum(banks[2,"iD_fb",])
  Firms[[2,"iL"]]=sum(banks[2,"iL_b",])
  Firms[[2,"L_np"]]=sum(banks[2,"L_npb",])
  Firms[[2,"rep_L"]]=sum(banks[2,"rep_Lb",])
  return(Firms)
}


divf<-function(Firms=stop("Need to have firms defined!"),params=params,args=args,Lags=Lags){
  Lags[args$Timer,"Pr_f"]<-Firms[2,"Pr_f"]
  Lags[args$Timer,"sav_ft"]<-Firms[2,"sav_ft"]
  if(args$Timer>23){
    Firms[[2,"div_f"]]=max(0,Firms[[(1),"div_f"]]+(sum(Lags[(args$Timer-23):args$Timer,"Pr_f"])/length(Lags[(args$Timer-23):args$Timer,"Pr_f"])-sum(Lags[(args$Timer-23):args$Timer,"sav_ft"])/length(Lags[(args$Timer-23):args$Timer,"sav_ft"])-Firms[[(1),"div_f"]])/24)
  }
  else{
    Firms[[2,"div_f"]]=max(0,Firms[[(1),"div_f"]]+(sum(Lags[(1):args$Timer,"Pr_f"])/length(Lags[(1):args$Timer,"Pr_f"])-sum(Lags[(1):args$Timer,"sav_ft"])/length(Lags[(1):args$Timer,"sav_ft"])-Firms[[(1),"div_f"]])/24)
  }
  return(list(Firms=Firms,Lags=Lags))
}

#####Government#####

decidegovspend<-function(Households=stop("need to have Households defined!"),Firms=stop("need to have firms defined!"),CB=stop("need to have CB defined!"),Gov=stop("need to have gov defined!"),params=params,args=args,Lags=Lags){
  if(args$Timer/12==round(args$Timer/12)){
    gcons=(1+(Lags[(args$Timer-1),"c_d"]-Lags[(args$Timer-11),"c_d"])/Lags[(args$Timer-11),"c_d"])^4-1
    gcons=1-gcons
    Gov[[2,"g_des"]]=params[[1,"g0"]]/48*gcons^params[[1,"mu"]]
    Gov[[2,"gap"]]=Gov[[2,"g_des"]]-Gov[[(1),"g"]]
    Gov[[2,"gapr"]]=Gov[[2,"g_des"]]-Gov[[(1),"g"]]
  }
  else{
    Gov[[2,"g_des"]]=Gov[[(1),"g_des"]]
    Gov[[2,"gap"]]=Gov[[(1),"gap"]]
    Gov[[2,"gapr"]]=Gov[[(1),"gapr"]]-(Gov[[2,"gap"]])*1/12
  }
  if(Gov[[2,"gapr"]]>0 && Gov[[2,"gap"]]>0){
  Gov[[2,"g"]]=Gov[[(1),"g"]]+(Gov[[2,"gap"]])*1/12
  }else{
  if(Gov[[2,"gapr"]]<0 && Gov[[2,"gap"]]<0){
  Gov[[2,"g"]]=Gov[[(1),"g"]]+(Gov[[2,"gap"]])*1/12
  }else{
  Gov[[2,"g"]]=Gov[[(1),"g"]]
  }}
  return(Gov)
}

decidetax<-function(Gov=stop("need to have gov defined!"),Firms=stop("Need to have firms defined!"),Households=stop("need to have Households defined!"),Banks=stop("need to have Banks (agg) defined!"),params=params,args=args,Lags=Lags){
  if(args$Timer<49){
    Gov[[2,"Tax"]]=Gov[[(1),"Tax"]]
    Gov[[2,"tau"]]=Gov[[(1),"tau"]]
    Gov[[2,"FTax"]]=0
  }else{
    Gov[[2,"tau"]]=params[[1,"tau"]]
    Gov[[2,"Tax"]]=Gov[[2,"tau"]]*(sum(Lags[(args$Timer-48):(args$Timer-1),"YD_tax"]))/48
    if(mean(Lags[(args$Timer-48):(args$Timer-1),"PSBR"])>0){
      Gov[[2,"FTax"]]=(mean(Lags[(args$Timer-48):(args$Timer-1),"PSBR"]))
    }else{
      Gov[[2,"FTax"]]=0
    }
  }
  return(Gov)
}

supplygb<-function(Gov=stop("need to have gov defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
  Gov[[2,"gb_s"]]=ifelse(Gov[[2,"PSBR"]]>=0,Gov[[2,"PSBR"]],0)
  Gov[[2,"rep_gb"]]=ifelse(Gov[[2,"PSBR"]]<0,abs(Gov[[2,"PSBR"]]),0)
  Gov[[2,"rep_gbcb"]]=ifelse(CB[[(1),"gb_cb"]]>Gov[[2,"rep_gb"]]*CB[[(1),"gb_cb"]]/Gov[[(1),"gb"]],Gov[[2,"rep_gb"]]*CB[[(1),"gb_cb"]]/Gov[[(1),"gb"]],ifelse(CB[[(1),"gb_cb"]]>0,CB[[(1),"gb_cb"]],0))
  Gov[[2,"rep_gbh"]]=Gov[[2,"rep_gb"]]-Gov[[2,"rep_gbcb"]]
  return(Gov)
}

gbstock<-function(Gov=stop("need to have gov defined!"),Households=stop("need to have Households defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
  Gov[[2,"gb"]]=CB[[(1),"gb_cb"]]+Households[[(1),"gb_h"]]+Gov[[2,"gb_s"]]-Gov[[2,"rep_gbcb"]]-Gov[[2,"rep_gbh"]]
  return(Gov)
}

gbint<-function(Gov=stop("need to have gov defined!"),CB=stop("need to have CB defined!"),Households=stop("need to have Households defined"),params=params,args=args){
  Gov[[2,"iGB_h"]]=(Gov[[(1),"r_gb"]]*Households[[(1),"gb_h"]])/48
  Gov[[2,"iGB_cb"]]=(Gov[[(1),"r_gb"]]*CB[[(1),"gb_cb"]])/48
  Gov[[2,"iGB"]]=Gov[[2,"iGB_h"]]+Gov[[2,"iGB_cb"]]
  return(Gov)
}

setgbrate<-function(Gov=stop("need to have gov defined!"),CB=stop("need to have CB defined!"),Households=stop("need to have Households defined"),Aux=stop("need to have auxiliary array defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  Gov[[2,"r_gb"]]=(((Households[[(1),"gb_h"]]+Gov[[2,"gb_s"]]-Gov[[2,"rep_gbh"]])/Households[[(1),"TAA_h"]]-Aux[[(1),"porthgb"]])*12+Aux[[(1),"porthgb"]]-params[[1,"lambdah10"]]+params[[1,"lambdah12"]]*Banks[[2,"r_dav"]])/params[[1,"lambdah11"]]
  return(Gov)
}


#####Central Bank#####
infmeasures<-function(Firms=stop("need to have firms defined!"),CB=stop("need to have CB defined!"),params=params,args=args,Lags=Lags){
    if(args$Timer<48){
    CB[[2,"pi_a"]]=((Firms[[2,"p"]]-sum(Lags[1:(args$Timer-1),"p"])/length(Lags[1:(args$Timer-1),"p"]))/(sum(Lags[1:(args$Timer-1),"p"])/length(Lags[1:(args$Timer-1),"p"])))
    }else{
    CB[[2,"pi_a"]]=(Firms[[2,"p"]]-Lags[[(args$Timer-47),"p"]])/Lags[[(args$Timer-47),"p"]]
    }
    if(args$Timer<24){
    CB[[2,"pi_sa"]]=(1+(Firms[[2,"p"]]-sum(Lags[1:(args$Timer-1),"p"])/length(Lags[1:(args$Timer-1),"p"]))/(sum(Lags[1:(args$Timer-1),"p"])/length(Lags[1:(args$Timer-1),"p"])))^2-1  
    }else{
    CB[[2,"pi_sa"]]=(1+(Firms[[2,"p"]]-Lags[[(args$Timer-23),"p"]])/Lags[[(args$Timer-23),"p"]])^2-1
  }
    if(args$Timer<12){
    CB[[2,"pi_q"]]=(1+(Firms[[2,"p"]]-sum(Lags[1:(args$Timer-1),"p"])/length(Lags[1:(args$Timer-1),"p"]))/(sum(Lags[1:(args$Timer-1),"p"])/length(Lags[1:(args$Timer-1),"p"])))^4-1  
    }else{
    CB[[2,"pi_q"]]=(1+(Firms[[2,"p"]]-Lags[[(args$Timer-11),"p"]])/Lags[[(args$Timer-11),"p"]])^4-1
  }
    if(args$Timer<4){
    CB[[2,"pi_m"]]=(1+(Firms[[2,"p"]]-sum(Lags[1:(args$Timer-1),"p"])/length(Lags[1:(args$Timer-1),"p"]))/(sum(Lags[1:(args$Timer-1),"p"])/length(Lags[1:(args$Timer-1),"p"])))^12-1
    }
    else{
    CB[[2,"pi_m"]]=(1+(Firms[[2,"p"]]-Lags[[(args$Timer-3),"p"]])/Lags[[(args$Timer-3),"p"]])^12-1
  }
  return(CB)
}


setcbrate<-function(CB=stop("need to have CB defined!"),Households=stop("need to have households defined!"),Firms=stop("need to have firms defined!"),params=params,args=args,Lags=Lags){
  if(args$Timer/4==round(args$Timer/4) && args$Timer>4){
   if(params[1,"altMP"]==1){
   gi<-(1+(Lags[(args$Timer-1),"i_d"]-Lags[(args$Timer-4),"i_d"])/Lags[(args$Timer-4),"i_d"])^12-1
   CB[2,"r_cbd"]=max(0,params[[1,"r0"]]+CB[[2,"pi_e"]]+params[[1,"phipi"]]*(CB[[2,"pi_e"]]-params[[1,"pit"]])+0.15*gi)  
   }else{
   CB[2,"r_cbd"]=max(0,params[[1,"r0"]]+CB[[2,"pi_e"]]+params[[1,"phiu"]]*(CB[[2,"u_ecb"]]-Firms[[2,"u_n"]])+params[[1,"phipi"]]*(CB[[2,"pi_e"]]-params[[1,"pit"]]))
   }}else{
    CB[2,"r_cbd"]=CB[(1),"r_cbd"]
  }
  CB[2,"r_cbl"]=CB[2,"r_cbd"]+params[[1,"r1"]]
  return(CB)
}

RAstock<-function(CB=stop("need to have CB defined!"),banks=stop("need to have banks (ABM) defined!"),params=params,args=args){
  CB[[2,"A"]]=sum(banks[2,"a_b",])
  CB[[2,"R"]]=sum(banks[2,"r_b",])
  return(CB)
}

monetisedef<-function(CB=stop("need to have CB defined!"),Gov=stop("need to have gov defined!"),Households=stop("need to have Households defined!"),params=params,args=args){
  CB[[2,"gb_rcb"]]=max(0,min(Gov[[2,"PSBR"]],Gov[[2,"PSBR"]]-(Households[[2,"gb_d"]]-Households[[(1),"gb_h"]])))
  return(CB)
}

gbcbstock<-function(CB=stop("need to have CB defined!"),Gov=stop("need to have gov defined!"),params=params,args=args){
  CB[[2,"gb_cb"]]=CB[[(1),"gb_cb"]]+CB[[2,"gb_rcb"]]-CB[[2,"gb_scb"]]+CB[[2,"gb_dcb"]]-Gov[[2,"rep_gbcb"]]
  return(CB)
}

Rtarget<-function(CB=stop("need to have CB defined!"),banks=stop("need to have banks (ABM) defined!"),params=params,args=args){
  CB[[2,"R_t"]]=sum(banks[2,"R_tb",])
  return(CB)
}

intervenegb<-function(CB=stop("need to have CB defined!"),params=params,args=args){
  CB[[2,"gb_dcb"]]=max(0,CB[[2,"R_t"]]-CB[[2,"R_pd"]])
  CB[[2,"gb_scb"]]=max(0,CB[[2,"R_pd"]]-CB[[2,"R_t"]])
  return(CB)
}

CBsolvecyc<-function(CB=stop("need to have CB defined!"),banks=stop("need to have banks (ABM) defined!"),Banks=stop("need to have banks (aggregate) defined!"),Households=stop("need to have Households defined!"),Gov=Gov,params=params,args=args){
  CB[[2,"buff_h"]]=-(sum(banks[2,"R_paltb",])-CB[[(1),"buff_h"]])+CB[[2,"R_t"]]
  CB[[2,"R_pd"]]=Households[[2,"sav_h"]]+Households[[2,"M_sup"]]-Households[[2,"rep_m"]]+Gov[[2,"rep_gbh"]]-(Gov[[2,"gb_s"]]-CB[[2,"gb_rcb"]])+(sum(banks[2,"R_paltb",]))
  CB[[2,"cbint"]]=-(CB[[2,"R_pd"]]-CB[[2,"R_t"]])
  return(CB)
}


targetratios<-function(CB=stop("need to have CB defined!"),Lags=stop("need to have lagged values defined!"),params=params,args=args){
  if(params[1,"polCAR"]==1){ 
  if(args$Timer/4==round(args$Timer/4) && args$Timer>4){
    Lev=mean(Lags[(args$Timer-4):(args$Timer-1),"lev_f"])-params[1,"lev_ft"]
    CB[2,"targetCAR"]=params[[1,"CARt0"]]*(2/(1+exp(8*(-Lev))))
    CB[2,"CARgap"]=CB[2,"targetCAR"]-CB[(1),"CARt"]
   }else{
    CB[2,"CARgap"]=CB[1,"CARgap"]
    CB[2,"CARt"]=CB[1,"CARt"]+CB[2,"CARgap"]/4
   }}
  else{
  CB[[2,"CARt"]]=params[[1,"CARt0"]]
  }
  return(CB)
}

cbinterest<-function(banks=stop("need to have banks (ABM) defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
  CB[[2,"iA"]]=sum(banks[2,"ia_b",])
  CB[[2,"iR"]]=sum(banks[2,"ir_b",])
  return(CB)
}

###BANKS#####

lsupply<-function(banks=stop("need to have banks (ABM) defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  Banks[[2,"L_sup"]]=sum(banks[2,"L_supb",])
  return(Banks)
}

Bankdivs<-function(banks=stop("need to have banks (ABM) defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  Banks[[2,"Div_b"]]=sum(banks[2,"div_b",])
  return(Banks)
}	

aggregateCAR<-function(banks=stop("need to have banks (ABM) defined!"),Banks=stop("need to have banks (aggregate) defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
  Banks[[2,"CAR"]]=sum(banks[2,"carshare",])
  return(Banks)
}

averagerates<-function(banks=stop("need to have banks (ABM) defined!"),Banks=stop("need to have banks (aggregate) defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
  banks[2,"rdshare",]=banks[(1),"r_db",]*banks[(1),"D_hb",]/sum(banks[(1),"D_hb",])
  banks[2,"rlshare",]=banks[(1),"r_Lb",]*banks[(1),"L_b",]/sum(banks[(1),"L_b",])
  banks[2,"rmshare",]=banks[(1),"r_Mb",]*banks[(1),"M_b",]/sum(banks[(1),"M_b",])
  Banks[[2,"r_dav"]]=sum(banks[2,"rdshare",])
  Banks[[2,"r_Lav"]]=sum(banks[2,"rlshare",])
  Banks[[2,"r_mav"]]=sum(banks[2,"rmshare",])
  Banks[[2,"r_Lavr"]]=(1+Banks[[2,"r_Lav"]])/(1+CB[[2,"pi_a"]])-1
  Banks[[2,"r_mavr"]]=(1+Banks[[2,"r_mav"]])/(1+CB[[2,"pi_a"]])-1
  return(list(Banks=Banks,banks=banks))
}

#####Agent-based banks####

bankinterest<-function(banks=stop("need to have banks (ABM) defined!"),CB=stop("need to have CB defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
    banks[2,"iD_fb",]=(banks[(1),"r_db",]*banks[(1),"D_fb",])/48
    banks[2,"iD_hb",]=(banks[(1),"r_db",]*banks[(1),"D_hb",])/48
    banks[2,"ia_b",]=(CB[[(1),"r_cbl"]]*banks[(1),"a_b",])/48
    banks[2,"ir_b",]=(CB[[(1),"r_cbd"]]*banks[(1),"r_b",])/48
    banks[2,"iIB_b",]=(Banks[[(1),"r_IB"]]*(banks[(1),"IBL_b",]-banks[(1),"IBA_b",]))/48
    banks[2,"iL_b",]=(banks[(1),"r_Lb",]*(banks[(1),"L_b",]-banks[2,"L_npb",]))/48
    banks[2,"iM_b",]=(banks[(1),"r_Mb",]*(banks[(1),"M_b",]-banks[2,"M_npb",]))/48
    return(banks)
}

rationindicators<-function(banks=stop("need to have banks (ABM) defined!"),Firms=stop("need to have Firms defined!"),Households=stop("need to have Households defined!"),params=params,args=args){
  for(i in 1:args$nB){
  banks[2,"rationL",i]<-(banks[(1),"L_db",i]-params[1,"omega2"]/(params[1,"omega1"]+params[1,"omega2"])*banks[(1),"RWA_gap",i])/banks[(1),"L_db",i]
  banks[2,"rationM",i]<-banks[(1),"M_supb",i]/banks[(1),"M_db",i]
  banks[2,"rationL",i]=(params[1,"xi_1"]/(1+exp(params[1,"xi_2"]*(-banks[2,"rationL",i]+1))))
  banks[2,"rationM",i]=(params[1,"xi_1"]/(1+exp(params[1,"xi_2"]*(-banks[2,"rationM",i]+1))))
  }
  ifelse(sum(banks[(2),"rationL",])>0,banks[(2),"rationL",]<-(banks[(2),"rationL",]/(sum(banks[(2),"rationL",])/args$nB)),banks[(2),"rationL",]<-1)
  ifelse(sum(banks[(2),"rationM",])>0,banks[(2),"rationM",]<-(banks[(2),"rationM",]/(sum(banks[(2),"rationM",])/args$nB)),banks[(2),"rationM",]<-1)
  return(banks)
}

  

Ldistr<-function(banks=stop("need to have banks (ABM) defined!"),Firms=stop("need to have Firms defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args,Frand=Frand){
  banks[2,"rLrel",]<-1+banks[2,"r_Lb",]
  banks[2,"rLrel",]=1/(banks[2,"rLrel",]/(sum(banks[2,"rLrel",])/args$nB))
  ifelse(Firms[[(1),"L_d"]]>0,banks[2,"Llag",]<-(banks[(1),"L_db",]/Firms[[(1),"L_d"]]),banks[2,"Llag",]<-1/args$nB)
  for(i in 1:args$nB){
  banks[2,"detshareL",i]<-max(0.01,banks[2,"Llag",i]*banks[2,"rLrel",i]^params[[1,"iota1"]]*banks[(2),"rationL",i]^params[[1,"iota2"]])
  }
  banks[2,"detshareL",]<-banks[2,"detshareL",]/sum(banks[2,"detshareL",])
  if(params[[1,"randdis"]]==1){
    Frand<-params[[1,"AR_dis"]]*Frand+(1-params[[1,"AR_dis"]])*rnorm(args$nB,1,params[[1,"sdev_dis"]])
    for(i in 1:args$nB){
    banks[2,"randshareL",i]<-max(0.01,banks[2,"Llag",i]*banks[2,"rLrel",i]^params[[1,"iota1"]]*banks[(2),"rationL",i]^params[[1,"iota2"]]*Frand[i])
    }
    banks[2,"randshareL",]=banks[2,"randshareL",]/sum(banks[2,"randshareL",])
      banks[2,"L_db",]=banks[2,"randshareL",]*Firms[2,"L_d"]
  }else{
      banks[2,"randshareL",]=banks[2,"detshareL",]
      banks[2,"L_db",]=banks[2,"detshareL",]*Firms[2,"L_d"]
  }
  return(list(banks=banks,Frand=Frand))
}


Mdistr<-function(banks=stop("need to have banks (ABM) defined!"),Households=stop("need to have Households defined!"),params=params,args=args,Hrandm=Hrandm){
  banks[2,"rMrel",]<-1+banks[2,"r_Mb",]
  banks[2,"rMrel",]=1/(banks[2,"rMrel",]/(sum(banks[2,"rMrel",])/args$nB))
  ifelse(Households[[(1),"M_d"]]>0,banks[2,"Mlag",]<-(banks[(1),"M_db",]/Households[[(1),"M_d"]]),banks[2,"Mlag",]<-1/args$nB)
  for(i in 1:args$nB){
  banks[2,"detshareM",i]<-max(0.01,banks[2,"Mlag",i]*banks[2,"rMrel",i]^params[[1,"iota1"]]*banks[(2),"rationM",i]^params[[1,"iota2"]])
  }
  banks[2,"detshareM",]<-banks[2,"detshareM",]/sum(banks[2,"detshareM",])
  if(params[[1,"randdis"]]==1){
    Hrandm<-params[[1,"AR_dis"]]*Hrandm+(1-params[[1,"AR_dis"]])*rnorm(args$nB,1,params[[1,"sdev_dis"]])
    for(i in 1:args$nB){
    banks[2,"randshareM",i]<-max(0.01,banks[2,"Mlag",i]*banks[2,"rMrel",i]^params[[1,"iota1"]]*banks[(2),"rationM",i]^params[[1,"iota2"]]*Hrandm[i])
    }
    banks[2,"randshareM",]<-banks[2,"randshareM",]/sum(banks[2,"randshareM",])
      banks[2,"M_db",]=banks[2,"randshareM",]*Households[2,"M_d"]
  }else{
      banks[2,"randshareM",]=banks[2,"detshareM",]
      banks[2,"M_db",]=banks[2,"detshareM",]*Households[2,"M_d"]
  }
  return(list(banks=banks,Hrandm=Hrandm))
}


Dhdistr<-function(banks=stop("need to have banks (ABM) defined!"),Households=stop("need to have Households defined!"),params=params,args=args,Hrandd=Hrandd){
  ifelse(Households[[(1),"D_h"]]>0,banks[2,"Dhlag",]<-(banks[(1),"D_hb",]/Households[[(1),"D_h"]]),banks[2,"Dhlag",]<-1/args$nB)
  banks[2,"detshareDh",]<-banks[2,"Dhlag",]*banks[2,"rDrel",]^params[[1,"iota1"]]
  banks[2,"detshareDh",]<-banks[2,"detshareDh",]/sum(banks[2,"detshareDh",])
  if(params[[1,"randdis"]]==1){
    Hrandd<-params[[1,"AR_dis"]]*Hrandd+(1-params[[1,"AR_dis"]])*rnorm(args$nB,1,params[[1,"sdev_dis"]])
    banks[2,"randshareDh",]<-banks[2,"Dhlag",]*banks[2,"rDrel",]^params[[1,"iota1"]]*Hrandd
    banks[2,"randshareDh",]=banks[2,"randshareDh",]/sum(banks[2,"randshareDh",])
      banks[2,"D_hb",]=banks[2,"randshareDh",]*Households[2,"D_h"]
  }else{
      banks[2,"randshareDh",]=banks[2,"detshareDh",]
      banks[2,"D_hb",]=banks[2,"detshareDh",]*Households[2,"D_h"]
  }
  return(list(banks=banks,Hrandd=Hrandd))
}

Dfdistr<-function(banks=stop("need to have banks (ABM) defined!"),Firms=stop("need to have Firms defined!"),params=params,args=args,Frandd=Frandd){
  banks[2,"rDrel",]<-1+banks[2,"r_db",]
  banks[2,"rDrel",]=banks[2,"rDrel",]/(sum(banks[2,"rDrel",])/args$nB)
  ifelse(Firms[[(1),"D_f"]]>0,banks[2,"Dflag",]<-(banks[(1),"D_fb",]/Firms[[(1),"D_f"]]),banks[2,"Dflag",]<-1/args$nB)
  banks[2,"detshareDf",]<-banks[2,"Dflag",]*banks[2,"rDrel",]^params[[1,"iota1"]]
  banks[2,"detshareDf",]<-banks[2,"detshareDf",]/sum(banks[2,"detshareDf",])
  if(params[[1,"randdis"]]==1){
    Frandd<-params[[1,"AR_dis"]]*Frandd+(1-params[[1,"AR_dis"]])*rnorm(args$nB,1,params[[1,"sdev_dis"]])
    banks[2,"randshareDf",]<-banks[2,"Dflag",]*banks[2,"rDrel",]^params[[1,"iota1"]]*Frandd
    banks[2,"randshareDf",]=banks[2,"randshareDf",]/sum(banks[2,"randshareDf",])
      banks[2,"D_fb",]=banks[2,"randshareDf",]*Firms[2,"D_f"]
  }else{
      banks[2,"randshareDf",]=banks[2,"detshareDf",]
      banks[2,"D_fb",]=banks[2,"detshareDf",]*Firms[2,"D_f"]
  }
  return(list(banks=banks,Frandd=Frandd))
}

bankestimates1<-function(banks=stop("need to have banks (ABM) defined!"),Est=stop("need to have est. array defined!"),Firms=stop("Need to have firms defined!"),Households=stop("Need to have households defined!"),params=params,args=args,drawBank=drawBank){
  Est[args$Timer,"defrateM",]<-banks[2,"defrateM",]
  Est[args$Timer,"defrateL",]<-banks[2,"defrateL",]
  if(args$nB>1){
  drawBank<-c(sample(1:args$nB,args$nB/4,replace=FALSE))
  }else{
  drawBank<-1
  }
  if(args$Timer<144){
  banks[2,"default_M",]=banks[(2),"defrateM",]
  banks[2,"default_L",]=banks[(2),"defrateL",]
  }else{
  if(params[1,"est1"]==1){
  for(i in 1:args$nB){
  defrateM<-ts(Est[1:(args$Timer),"defrateM",i])
  defMest<-dyn$lm(defrateM ~ lag(defrateM,1))
  banks[2,"default_M",i] = defMest$coefficients[1]+defMest$coefficients[2]*banks[(1),"defrateM",i]
  defrateL<-ts(Est[1:(args$Timer),"defrateL",i])
  defLest<-dyn$lm(defrateL ~ lag(defrateL,1))
  banks[2,"default_L",i] = defLest$coefficients[1]+defLest$coefficients[2]*banks[(1),"defrateL",i]
  }}else{
  banks[2,"default_M",] = banks[(2),"defrateM",]
  banks[2,"default_L",] = banks[(2),"defrateL",]
  }}
  return(list(banks=banks,drawBank=drawBank,Est=Est))
}


bankestimates2<-function(banks=stop("need to have banks (ABM) defined!"),CB=stop("need to have CB defined!"),Est=stop("need to have est. array defined!"),Firms=stop("Need to have firms defined!"),Households=stop("Need to have households defined!"),params=params,args=args,drawBank=drawBank,Lags=Lags){
  Lags[args$Timer,"r_cbl"]<-CB[2,"r_cbl"]
  Lags[args$Timer,"r_cbd"]<-CB[2,"r_cbd"]
  if(args$Timer<144){
  if(args$Timer<12){
  banks[2,"markup_M",]=banks[(1),"markup_M",]
  banks[2,"markup_L",]=banks[(1),"markup_L",]
  }else{
  for(i in 1:args$nB){
  if(is.element(i,drawBank)){
  banks[2,"markup_M",i]=ifelse(sum(Est[(args$Timer-4):(args$Timer-1),"iM_b",i])-sum(Est[(args$Timer-8):(args$Timer-5),"iM_b",i])>0 && banks[1,"r_Mb",i]/(mean(banks[1,"r_Mb",])) <= 1,banks[[2,"markup_M",i]] <- max(1,banks[[1,"markup_M",i]]+max(0,rnorm(1,params[1,"step"],params[1,"sdev_step"]))),ifelse(sum(Est[(args$Timer-4):(args$Timer-1),"iM_b",i])-sum(Est[(args$Timer-8):(args$Timer-5),"iM_b",i])<0 && banks[1,"r_Mb",i]/(mean(banks[1,"r_Mb",])) >= 1,banks[[2,"markup_M",i]] <- max(1,banks[[1,"markup_M",i]]-max(0,rnorm(1,params[1,"step"],params[1,"sdev_step"]))),banks[[2,"markup_M",i]] <- banks[[1,"markup_M",i]]))
  banks[2,"markup_L",i]=ifelse(sum(Est[(args$Timer-4):(args$Timer-1),"iL_b",i])-sum(Est[(args$Timer-8):(args$Timer-5),"iL_b",i])>0 && banks[1,"r_Lb",i]/(mean(banks[1,"r_Lb",])) <= 1,banks[[2,"markup_L",i]] <- max(1,banks[[1,"markup_L",i]]+max(0,rnorm(1,params[1,"step"],params[1,"sdev_step"]))),ifelse(sum(Est[(args$Timer-4):(args$Timer-1),"iL_b",i])-sum(Est[(args$Timer-8):(args$Timer-5),"iL_b",i])<0 && banks[1,"r_Lb",i]/(mean(banks[1,"r_Lb",])) >= 1,banks[[2,"markup_L",i]] <- max(1,banks[[1,"markup_L",i]]-max(0,rnorm(1,params[1,"step"],params[1,"sdev_step"]))),banks[[2,"markup_L",i]] <- banks[[1,"markup_L",i]]))
  }else{
  banks[2,"markup_M",i]=banks[1,"markup_M",i]
  banks[2,"markup_L",i]=banks[1,"markup_L",i]
  }}}}
  else{
  if(params[1,"est1"]==1){
  for(i in 1:args$nB){
    if(is.element(i,drawBank)){
    iL<-ts(Est[1:(args$Timer-1),"iL_b",i])
    iM<-ts(Est[1:(args$Timer-1),"iM_b",i])
    Ldef<-ts((Est[1:(args$Timer-1),"defrateL",i])*(Est[1:(args$Timer-1),"L_b",i]))
    Mdef<-ts((Est[1:(args$Timer-1),"defrateM",i])*(Est[1:(args$Timer-1),"M_b",i]))
    Lrate<-ts(Est[1:(args$Timer-1),"r_Lb",i])
    Lratesq<-ts(Est[1:(args$Timer-1),"r_Lb",i]^2)
    Mrate<-ts(Est[1:(args$Timer-1),"r_Mb",i])
    Mratesq<-ts(Est[1:(args$Timer-1),"r_Mb",i]^2)
    Lavrate<-ts(Est[1:(args$Timer-1),"r_mav",i])
    Mavrate<-ts(Est[1:(args$Timer-1),"r_Lav",i])
    Mstep<-max(0,rnorm(1,params[1,"step"],params[1,"sdev_step"]))
    Lstep<-max(0,rnorm(1,params[1,"step"],params[1,"sdev_step"]))
    Lnewu<-max(mean(Lags[(args$Timer-12):(args$Timer),"r_cbl"]+Lags[(args$Timer-12):(args$Timer),"r_cbd"])/2,(mean(Lags[(args$Timer-12):(args$Timer),"r_cbl"]+Lags[(args$Timer-12):(args$Timer),"r_cbd"])/2+banks[[2,"default_L",i]])*(banks[[1,"markup_L",i]]+Lstep))
    Mnewu<-max(mean(Lags[(args$Timer-12):(args$Timer),"r_cbl"]+Lags[(args$Timer-12):(args$Timer),"r_cbd"])/2,(mean(Lags[(args$Timer-12):(args$Timer),"r_cbl"]+Lags[(args$Timer-12):(args$Timer),"r_cbd"])/2+banks[[2,"default_M",i]])*(banks[[(1),"markup_M",i]]+Mstep))
    Lnewl<-max(mean(Lags[(args$Timer-12):(args$Timer),"r_cbl"]+Lags[(args$Timer-12):(args$Timer),"r_cbd"])/2,(mean(Lags[(args$Timer-12):(args$Timer),"r_cbl"]+Lags[(args$Timer-12):(args$Timer),"r_cbd"])/2+banks[[2,"default_L",i]])*(banks[[(1),"markup_L",i]]-Lstep))
    Mnewl<-max(mean(Lags[(args$Timer-12):(args$Timer),"r_cbl"]+Lags[(args$Timer-12):(args$Timer),"r_cbd"])/2,(mean(Lags[(args$Timer-12):(args$Timer),"r_cbl"]+Lags[(args$Timer-12):(args$Timer),"r_cbd"])/2+banks[[2,"default_M",i]])*(banks[[(1),"markup_M",i]]-Mstep))
    Lnewnc<-max(mean(Lags[(args$Timer-12):(args$Timer),"r_cbl"]+Lags[(args$Timer-12):(args$Timer),"r_cbd"])/2,(mean(Lags[(args$Timer-12):(args$Timer),"r_cbl"]+Lags[(args$Timer-12):(args$Timer),"r_cbd"])/2+banks[[2,"default_L",i]])*(banks[[(1),"markup_L",i]]))
    Mnewnc<-max(mean(Lags[(args$Timer-12):(args$Timer),"r_cbl"]+Lags[(args$Timer-12):(args$Timer),"r_cbd"])/2,(mean(Lags[(args$Timer-12):(args$Timer),"r_cbl"]+Lags[(args$Timer-12):(args$Timer),"r_cbd"])/2+banks[[2,"default_M",i]])*(banks[[(1),"markup_M",i]]))
    iLest<-dyn$lm(iL ~ lag(iL,1) +lag(Lavrate,1)+ lag(Lrate,1)+lag(Lratesq,1))
    iLestu<-(iLest$coefficients[4])*Lnewu+(iLest$coefficients[5])*Lnewu^2
    iLestl<-(iLest$coefficients[4])*Lnewl+(iLest$coefficients[5])*Lnewl^2
    iLestnc<-(iLest$coefficients[4])*Lnewnc+(iLest$coefficients[5])*Lnewnc^2
    iMest<-dyn$lm(iM ~ lag(iM,1)  +lag(Mavrate,1)+lag(Mrate,1)+lag(Mratesq,1))
    iMestu<-(iMest$coefficients[4])*Mnewu+(iMest$coefficients[5])*Mnewu^2
    iMestl<-(iMest$coefficients[4])*Mnewl+(iMest$coefficients[5])*Mnewl^2
    iMestnc<-(iMest$coefficients[4])*Mnewnc+(iMest$coefficients[5])*Mnewnc^2
    ifelse(iLestu>iLestl && iLestu>iLestnc ,banks[[2,"markup_L",i]] <- max(1,banks[[1,"markup_L",i]]+Lstep),ifelse(iLestl>iLestu && iLestl>iLestnc , banks[[2,"markup_L",i]] <- max(1,banks[[1,"markup_L",i]]-Lstep),banks[[2,"markup_L",i]]<-banks[[1,"markup_L",i]]))
    ifelse(iMestu>iMestl && iMestu>iMestnc ,banks[[2,"markup_M",i]] <- max(1,banks[[1,"markup_M",i]]+Mstep),ifelse(iMestl>iMestu && iMestl>iMestnc , banks[[2,"markup_M",i]] <- max(1,banks[[1,"markup_M",i]]-Mstep),banks[[2,"markup_M",i]]<-banks[[1,"markup_M",i]]))
    }else{
    banks[2,"markup_M",i]=banks[(1),"markup_M",i]
    banks[2,"markup_L",i]=banks[(1),"markup_L",i]
    }
  }}else{
  for(i in 1:args$nB){
  if(is.element(i,drawBank)){
  ifelse(sum(Est[(args$Timer-4):(args$Timer-1),"iM_b",i])-sum(Est[(args$Timer-8):(args$Timer-5),"iM_b",i])>0 && banks[1,"r_Mb",i]/(mean(banks[1,"r_Mb",])) <= 1,banks[[2,"markup_M",i]] <- max(1,banks[[1,"markup_M",i]]+max(0,rnorm(1,params[1,"step"],params[1,"sdev_step"]))),ifelse(sum(Est[(args$Timer-4):(args$Timer-1),"iM_b",i])-sum(Est[(args$Timer-8):(args$Timer-5),"iM_b",i])<0 && banks[1,"r_Mb",i]/(mean(banks[1,"r_Mb",])) >= 1,banks[[2,"markup_M",i]] <- max(1,banks[[1,"markup_M",i]]-max(0,rnorm(1,params[1,"step"],params[1,"sdev_step"]))),banks[[2,"markup_M",i]] <- banks[[1,"markup_M",i]]))
  ifelse(sum(Est[(args$Timer-4):(args$Timer-1),"iL_b",i])-sum(Est[(args$Timer-8):(args$Timer-5),"iL_b",i])>0 && banks[1,"r_Lb",i]/(mean(banks[1,"r_Lb",])) <= 1,banks[[2,"markup_L",i]] <- max(1,banks[[1,"markup_L",i]]+max(0,rnorm(1,params[1,"step"],params[1,"sdev_step"]))),ifelse(sum(Est[(args$Timer-4):(args$Timer-1),"iL_b",i])-sum(Est[(args$Timer-8):(args$Timer-5),"iL_b",i])<0 && banks[1,"r_Lb",i]/(mean(banks[1,"r_Lb",])) >= 1,banks[[2,"markup_L",i]] <- max(1,banks[[1,"markup_L",i]]-max(0,rnorm(1,params[1,"step"],params[1,"sdev_step"]))),banks[[2,"markup_L",i]] <- banks[[1,"markup_L",i]]))
  }else{
  banks[2,"markup_M",i]=banks[(1),"markup_M",i]
  banks[2,"markup_L",i]=banks[(1),"markup_L",i]
  }}}}
  return(list(banks=banks,Lags=Lags))
}


setbankrates<-function(banks=stop("need to have banks (ABM) defined!"),CB=stop("need to have CB defined!"),Banks=stop("need to have banks (aggregate) defined!"),Firms=stop("Need to have firms defined!"),Households=stop("Need to have households defined!"),params=params,args=args,drawBank=drawBank,Lags=Lags,Est=Est){
  if(args$Timer<13){
  for(i in 1:args$nB){
  banks[[2,"clear",i]]=1+tanh(params[1,"epsilond2"]*sum(Est[1:(args$Timer-1),"clear_b",i])/(args$Timer-1))
  }
  }else{
  for(i in 1:args$nB){
  banks[[2,"clear",i]]=1+tanh(params[1,"epsilond2"]*sum(Est[(args$Timer-12):(args$Timer-1),"clear_b",i])/12)
  }
  }
  if(args$Timer<12){
    banks[2,"r_db",]=banks[(1),"r_db",]
    banks[2,"r_Lb",]=banks[(1),"r_Lb",]
    banks[2,"r_Mb",]=banks[(1),"r_Mb",]
  }else{
  for(i in 1:args$nB){
    if(is.element(i,drawBank)){
      banks[[2,"r_db",i]]=max(0,min(mean(Lags[(args$Timer-12):(args$Timer),"r_cbl"]+Lags[(args$Timer-12):(args$Timer),"r_cbd"])/2+params[1,"epsilond1"]*banks[[2,"clear",i]],mean(Lags[(args$Timer-12):(args$Timer),"r_cbl"]+Lags[(args$Timer-12):(args$Timer),"r_cbd"])/2))
      banks[[2,"r_Lb",i]]=max(mean(Lags[(args$Timer-12):(args$Timer),"r_cbl"]+Lags[(args$Timer-12):(args$Timer),"r_cbd"])/2,(mean(Lags[(args$Timer-12):(args$Timer),"r_cbl"]+Lags[(args$Timer-12):(args$Timer),"r_cbd"])/2+banks[[2,"default_L",i]])*banks[[2,"markup_L",i]])
      banks[[2,"r_Mb",i]]=max(mean(Lags[(args$Timer-12):(args$Timer),"r_cbl"]+Lags[(args$Timer-12):(args$Timer),"r_cbd"])/2,(mean(Lags[(args$Timer-12):(args$Timer),"r_cbl"]+Lags[(args$Timer-12):(args$Timer),"r_cbd"])/2+banks[[2,"default_M",i]])*banks[[2,"markup_M",i]])
    }else{
      banks[[2,"r_db",i]]=banks[[(1),"r_db",i]]
      banks[[2,"r_Lb",i]]=banks[[(1),"r_Lb",i]]
      banks[[2,"r_Mb",i]]=banks[[(1),"r_Mb",i]]
    }
  }}
  return(list(banks=banks,Banks=Banks))
}


rationcredit<-function(banks=stop("need to have banks (ABM) defined!"),Firms=stop("need to have firms defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
    for(i in 1:args$nB){
    banks[2,"RWA_gap",i]=((params[1,"omega1"]*(banks[(1),"M_b",i]+banks[2,"M_db",i]-banks[2,"rep_mb",i]-banks[2,"M_npb",i])+params[1,"omega2"]*(banks[(1),"L_b",i]+banks[2,"L_db",i]-banks[2,"rep_Lb",i]-banks[2,"L_npb",i]))-banks[(2),"v_bbe",i]/CB[[2,"CARt"]])/48
    if(params[[1,"randdis"]]==1){
    banks[2,"L_supb",i]=banks[2,"randshareL",i]*Firms[[2,"replacegap"]]+max(0,min(banks[2,"L_db",i]-banks[2,"randshareL",i]*Firms[[2,"replacegap"]],banks[2,"L_db",i]-banks[2,"randshareL",i]*Firms[[2,"replacegap"]]-params[1,"omega2"]/(params[1,"omega1"]+params[1,"omega2"])*banks[2,"RWA_gap",i]))
    }else{
    banks[2,"L_supb",i]=banks[2,"detshareL",i]*Firms[[2,"replacegap"]]+max(0,min(banks[2,"L_db",i]-banks[2,"detshareL",i]*Firms[[2,"replacegap"]],banks[2,"L_db",i]-banks[2,"detshareL",i]*Firms[[2,"replacegap"]]-params[1,"omega2"]/(params[1,"omega1"]+params[1,"omega2"])*banks[2,"RWA_gap",i]))
    }
    banks[2,"M_supb",i]=max(0,min(banks[2,"M_db",i],banks[2,"M_db",i]-params[1,"omega1"]/(params[1,"omega1"]+params[1,"omega2"])*banks[2,"RWA_gap",i]))
    }
    return(banks)
}

defindicators<-function(Households=stop("need to have households defined!"),Firms=stop("need to have Firms defined!"),params=params,args=args,Lags=Lags){
    if(args$Timer<48){
    Firms[[2,"meanlev"]]=sum(Lags[1:(args$Timer-1),"lev_f"])/length(Lags[1:(args$Timer-1),"lev_f"])
    Households[[2,"MH"]]=(sum(Lags[1:(args$Timer-1),"M"])/length(Lags[1:(args$Timer-1),"M"]))/((sum(Lags[1:(args$Timer-1),"p_h"])/length(Lags[1:(args$Timer-1),"p_h"]))*params[[1,"h"]])
  }else{
    Firms[[2,"meanlev"]]=sum(Lags[(args$Timer-47):(args$Timer-1),"lev_f"])/length(Lags[(args$Timer-47):(args$Timer-1),"lev_f"])
    Households[[2,"MH"]]=(sum(Lags[(args$Timer-47):(args$Timer-1),"M"])/length(Lags[(args$Timer-47):(args$Timer-1),"M"]))/((sum(Lags[(args$Timer-47):(args$Timer-1),"p_h"])/length(Lags[(args$Timer-47):(args$Timer-1),"p_h"]))*params[[1,"h"]])
  }
  return(list(Firms=Firms,Households=Households))
}

defaults<-function(banks=stop("need to have banks (ABM) defined!"),Households=stop("need to have households defined!"),Firms=stop("need to have Firms defined!"),Aux=stop("need to have auxiliary array defined!"),params=params,args=args,pvarsh=pvarsh,pvarsf=pvarsf){
      if(params[1,"randdef"]==1){
      Aux[[2,"sdev_deff"]]=params[[1,"sdev_def"]]*0.5
      Aux[[2,"sdev_defh"]]=params[[1,"sdev_def"]]
      }else{
      Aux[[2,"sdev_deff"]]=0
      Aux[[2,"sdev_defh"]]=0
      }
      if(params[1,"randdef"]==1){
      for(i in 1:args$nB){
      logvarsh <- qlogis(pvarsh[args$Timer,i],(banks[(1),"r_Mb",i]/(mean(banks[(1),"r_Mb",]))),Aux[[2,"sdev_defh"]])
      logvarsf <- qlogis(pvarsf[args$Timer,i],(banks[(1),"r_Lb",i]/(mean(banks[(1),"r_Lb",]))),Aux[[2,"sdev_deff"]])
      banks[2,"defrandf",i]=max(0,params[1,"AR_def"]*banks[(1),"defrandf",i]+(1-params[1,"AR_def"])*logvarsf)
      banks[2,"defrandh",i]=max(0,params[1,"AR_def"]*banks[(1),"defrandh",i]+(1-params[1,"AR_def"])*logvarsh)
      }}else{
      banks[2,"defrandf",]=1
      banks[2,"defrandh",]=1
      }
      banks[2,"defrateL",]=params[[1,"zeta_L"]]*Firms[[2,"meanlev"]]*banks[2,"defrandf",]
      banks[2,"defrateM",]=params[[1,"zeta_M"]]*Households[[2,"MH"]]*banks[2,"defrandh",]
      for(i in 1:args$nB){
      banks[[2,"L_npb",i]]=max(0,banks[[(1),"L_b",i]]/48*banks[[2,"defrateL",i]])
      banks[[2,"M_npb",i]]=max(0,banks[[(1),"M_b",i]]/48*banks[[2,"defrateM",i]])
      }
  return(list(banks=banks,Aux=Aux))
}


bankreps<-function(banks=stop("need to have banks (ABM) defined!"),params=params,args=args){
    for(i in 1:args$nB){
    banks[2,"rep_mb",i]=params[[1,"chi_M"]]*banks[(1),"M_b",i]/48
    banks[2,"rep_Lb",i]=params[[1,"chi_L"]]*banks[(1),"L_b",i]/48
    }
  return(banks)
}


loanstocks<-function(banks=stop("need to have banks (ABM) defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
    banks[2,"M_b",]=banks[(1),"M_b",]+banks[2,"M_supb",]-banks[2,"rep_mb",]-banks[2,"M_npb",]
    banks[2,"L_b",]=banks[(1),"L_b",]+banks[2,"L_supb",]-banks[2,"rep_Lb",]-banks[2,"L_npb",]
  return(banks)
}


IB1<-function(banks=stop("need to have banks (ABM) defined!"),params=params,args=args){
  for(i in 1:args$nB){
    banks[[2,"R_tb",i]]=params[[1,"LCRt"]]*params[[1,"omega3"]]*(banks[[(1),"D_hb",i]]+banks[[(1),"D_fb",i]])
    banks[[2,"clearalt_b",i]]=(banks[[(2),"D_fb",i]]-banks[[(1),"D_fb",i]])+banks[[2,"rep_Lb",i]]+banks[[2,"rep_mb",i]]-banks[[2,"L_supb",i]]-(banks[[2,"M_supb",i]])+banks[[2,"iL_b",i]]+banks[[2,"iM_b",i]]-banks[[2,"iD_hb",i]]-banks[[2,"iD_fb",i]]-banks[[2,"div_b",i]]-banks[[2,"ia_b",i]]+banks[[2,"ir_b",i]]
    banks[[2,"R_paltb",i]]=banks[[2,"clearalt_b",i]]+banks[[(1),"r_b",i]]+banks[[(1),"IBL_b",i]]-banks[[(1),"IBA_b",i]]-banks[[(1),"a_b",i]]+banks[[2,"iIB_b",i]]
  }
  return(banks)
}

IB2<-function(banks=stop("need to have banks (ABM) defined!"),params=params,args=args){
  for(i in 1:args$nB){
    banks[[2,"clear_b",i]]=(banks[[2,"D_hb",i]]+banks[[(2),"D_fb",i]]-banks[[(1),"D_hb",i]]-banks[[(1),"D_fb",i]])+banks[[2,"rep_Lb",i]]+banks[[2,"rep_mb",i]]-banks[[2,"L_supb",i]]-(banks[[2,"M_supb",i]])+banks[[2,"iL_b",i]]+banks[[2,"iM_b",i]]-banks[[2,"iD_hb",i]]-banks[[2,"iD_fb",i]]-banks[[2,"div_b",i]]-banks[[2,"ia_b",i]]+banks[[2,"ir_b",i]]
    banks[[2,"R_pb",i]]=banks[[2,"clear_b",i]]+banks[[(1),"r_b",i]]+banks[[(1),"IBL_b",i]]-banks[[(1),"IBA_b",i]]-banks[[(1),"a_b",i]]+banks[[2,"iIB_b",i]]
    banks[[2,"R_anb",i]]=banks[[2,"R_pb",i]]-banks[[2,"R_tb",i]]
    banks[[2,"IBL_sb",i]]=ifelse(banks[[2,"R_anb",i]]>0,banks[[2,"R_anb",i]],0)
    banks[[2,"IBA_db",i]]=ifelse(banks[[2,"R_anb",i]]<0,abs(banks[[2,"R_anb",i]]),0)
  }
  return(banks)
}

IB3<-function(banks=stop("need to have banks (ABM) defined!"),CB=stop("need to have CB defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args){
  Banks[[2,"IBS_p"]]=0
  Banks[[2,"IBD_p"]]=0
  Banks[[2,"IBS"]]=sum(banks[2,"IBL_sb",])
  Banks[[2,"IBD"]]=sum(banks[2,"IBA_db",])
  for(i in 1:args$nB){
    banks[[2,"IBSshare",i]]=ifelse(Banks[[2,"IBS"]]>0,banks[[2,"IBL_sb",i]]/Banks[[2,"IBS"]],1/args$nB)
    banks[[2,"IBDshare",i]]=ifelse(Banks[[2,"IBD"]]>0,banks[[2,"IBA_db",i]]/Banks[[2,"IBD"]],1/args$nB)
    if(banks[[2,"R_paltb",i]]-banks[[2,"R_tb",i]]>0){
    Banks[[2,"IBS_p"]]=Banks[2,"IBS_p"]+banks[[2,"R_paltb",i]]-banks[[2,"R_tb",i]]
    }else{
    Banks[[2,"IBD_p"]]=Banks[2,"IBD_p"]+abs(banks[[2,"R_paltb",i]]-banks[[2,"R_tb",i]])
    }
  }
    banks[2,"IBA_b",]=pmin(banks[2,"IBA_db",],banks[2,"IBDshare",]*Banks[[2,"IBS"]])
    banks[2,"IBL_b",]=pmin(banks[2,"IBL_sb",],banks[2,"IBSshare",]*Banks[[2,"IBD"]])
    banks[2,"R_lb",]=banks[2,"R_pb",]+banks[2,"IBA_b",]-banks[2,"IBL_b",]
    Banks[[2,"r_IB"]]=CB[[2,"r_cbd"]]+(CB[[2,"r_cbl"]]-CB[[2,"r_cbd"]])/(1+exp(-params[[1,"sigmaIB"]]*(Banks[2,"IBD_p"]-Banks[2,"IBS_p"])))
  return(list(Banks=Banks, banks=banks))
}

bankdivs<-function(banks=stop("need to have banks (ABM) defined!"),Banks=stop("need to have Banks (agg) defined!"),CB=stop("need to have CB defined!"),params=params,args=args,Est=Est){
    banks[2,"v_bbt",]=CB[[2,"CARt"]]*(params[[1,"omega1"]]*banks[2,"M_b",]+params[[1,"omega2"]]*banks[2,"L_b",])
    Est[args$Timer,"v_bbt",]<-banks[2,"v_bbt",]
    Est[args$Timer,"v_bbe",]<-banks[2,"v_bbe",]
    if(args$Timer<12){
    banks[2,"div_bt",]=0
    }else{
    if(args$Timer/12==round(args$Timer/12)){
    for(i in 1:args$nB){
    banks[2,"div_bt",i]=mean(Est[(args$Timer-11):args$Timer,"v_bbe",i]-Est[(args$Timer-11):args$Timer,"v_bbt",i])
    #banks[2,"div_bt",]=banks[2,"v_bbe",]-banks[2,"v_bbt",]
    }}else{
    banks[2,"div_bt",]=banks[(1),"div_bt",]
    }}
    banks[2,"div_b",]=pmax(0,banks[2,"pr_b",]+((banks[2,"div_bt",]))/12)
    #banks[2,"div_b",]=pmax(0,banks[2,"pr_b",]+(banks[2,"v_bbe",]-banks[2,"v_bbt",]))
    banks[2,"bb_b",]=banks[(1),"bb_b",]
  return(list(banks=banks,Banks=Banks,Est=Est))
}

regratios<-function(banks=stop("need to have banks (ABM) defined!"),params=params,args=args){
    banks[2,"CARb",]=pmax(0,banks[2,"v_bb",]/(params[[1,"omega1"]]*banks[2,"M_b",]+params[[1,"omega2"]]*banks[2,"L_b",]))
    banks[2,"LCRb",]=banks[2,"r_b",]/(params[[1,"omega3"]]*(banks[2,"D_hb",]+banks[2,"D_fb",]))
    banks[2,"carshare",]=banks[2,"CARb",]/sum(banks[2,"CARb",])*banks[2,"CARb",]
  return(banks)
}

rshares<-function(banks=stop("need to have banks (ABM) defined!"),params=params,args=args){
    banks[2,"rdshare",]=banks[(1),"r_db",]*banks[(1),"D_hb",]/sum(banks[(1),"D_hb",])
    banks[2,"rlshare",]=banks[(1),"r_Lb",]*banks[(1),"L_b",]/sum(banks[(1),"L_b",])
    banks[2,"rmshare",]=banks[(1),"r_Mb",]*banks[(1),"M_b",]/sum(banks[(1),"M_b",])
  return(banks)
}

######EXPECTATIONS etc.#####

bankexp<-function(banks=stop("need to have banks (ABM) defined!"),Est=stop("need to have est. array defined!"),CB=stop("need to have CB defined!"),params=params,args=args){
    if(params[[1,"Hommes"]]==1){
    if(args$Timer<3){
    banks[2,"v_bbe",]=banks[(1),"v_bbe",]+params[[1,"psi_ad"]]*(banks[(1),"v_bb",]-banks[(1),"v_bbe",])
    banks[2,"v_bbe1",]=banks[(1),"v_bbe1",]
    banks[2,"v_bbe2",]=banks[(1),"v_bbe2",]
    banks[2,"v_bbe3",]=banks[(1),"v_bbe3",]
    banks[2,"v_bbe4",]=banks[(1),"v_bbe4",]
    }else{
    for(i in 1:args$nB){
    banks[[2,"v_bbe1",i]]=banks[[(1),"v_bbe1",i]]+params[[1,"psi_ad"]]*(banks[[(1),"v_bb",i]]-banks[[(1),"v_bbe1",i]])
    banks[[2,"v_bbe2",i]]=banks[[(1),"v_bb",i]]+params[[1,"psi_tf1"]]*(banks[[(1),"v_bb",i]]-Est[[(args$Timer-2),"v_bb",i]])
    banks[[2,"v_bbe3",i]]=banks[[(1),"v_bb",i]]+params[[1,"psi_tf2"]]*(banks[[(1),"v_bb",i]]-Est[[(args$Timer-2),"v_bb",i]])
    banks[[2,"v_bbe4",i]]=params[[1,"psi_aa"]]*(sum(Est[1:(args$Timer-1),"v_bb",i])/length(Est[1:(args$Timer-1),"v_bb",i]))+(1-params[[1,"psi_aa"]])*(banks[[(1),"v_bb",i]])+(banks[[(1),"v_bb",i]]-Est[[(args$Timer-2),"v_bb",i]])
    #banks[[2,"v_bbe4",i]]=banks[(1),"v_bbt",i]
    banks[[2,"fitvbb1",i]]=exp(params[[1,"intensity"]]*(-(banks[[(1),"v_bb",i]]-banks[[(1),"v_bbe1",i]])^2-params[[1,"memory"]]*(Est[[(args$Timer-2),"v_bb",i]]-Est[[(args$Timer-2),"v_bbe1",i]])^2))
    banks[[2,"fitvbb2",i]]=exp(params[[1,"intensity"]]*(-(banks[[(1),"v_bb",i]]-banks[[(1),"v_bbe2",i]])^2-params[[1,"memory"]]*(Est[[(args$Timer-2),"v_bb",i]]-Est[[(args$Timer-2),"v_bbe2",i]])^2))
    banks[[2,"fitvbb3",i]]=exp(params[[1,"intensity"]]*(-(banks[[(1),"v_bb",i]]-banks[[(1),"v_bbe3",i]])^2-params[[1,"memory"]]*(Est[[(args$Timer-2),"v_bb",i]]-Est[[(args$Timer-2),"v_bbe3",i]])^2))
    banks[[2,"fitvbb4",i]]=exp(params[[1,"intensity"]]*(-(banks[[(1),"v_bb",i]]-banks[[(1),"v_bbe4",i]])^2-params[[1,"memory"]]*(Est[[(args$Timer-2),"v_bb",i]]-Est[[(args$Timer-2),"v_bbe4",i]])^2))
    ifelse(max(banks[[2,"fitvbb1",i]],banks[[2,"fitvbb2",i]],banks[[2,"fitvbb3",i]],banks[[2,"fitvbb4",i]])==banks[[2,"fitvbb1",i]],banks[[2,"maxfitvbb",i]]<-1,ifelse(max(banks[[2,"fitvbb1",i]],banks[[2,"fitvbb2",i]],banks[[2,"fitvbb3",i]],banks[[2,"fitvbb4",i]])==banks[[2,"fitvbb2",i]],banks[[2,"maxfitvbb",i]]<-2,ifelse(max(banks[[2,"fitvbb1",i]],banks[[2,"fitvbb2",i]],banks[[2,"fitvbb3",i]],banks[[2,"fitvbb4",i]])==banks[[2,"fitvbb3",i]],banks[[2,"maxfitvbb",i]]<-3,banks[[2,"maxfitvbb",i]]<-4)))
    ifelse(banks[[2,"maxfitvbb",i]]==1,banks[[2,"v_bbe",i]]<-banks[[2,"v_bbe1",i]],ifelse(banks[[2,"maxfitvbb",i]]==2,banks[[2,"v_bbe",i]]<-banks[[2,"v_bbe2",i]],ifelse(banks[[2,"maxfitvbb",i]]==3,banks[[2,"v_bbe",i]]<-banks[[2,"v_bbe3",i]],banks[[2,"v_bbe",i]]<-banks[[2,"v_bbe4",i]])))
    #banks[[2,"v_bbe",i]]=banks[[2,"v_bbe4",i]]
    }
    }}else{
    if(params[1,"est2"]==1 && args$Timer>48){
    for(i in 1:args$nB){
    vbb<-ts(Est[1:(args$Timer-1),"v_bb",i])
    vbbest<-dyn$lm(vbb ~ lag(vbb,1))
    banks[2,"v_bbe",i]=vbbest$coefficients[1]+vbbest$coefficients[2]*banks[(1),"v_bb",i]
    }  
    }else{
    banks[2,"v_bbe",]=banks[(1),"v_bbe",]+params[[1,"psi_ad"]]*(banks[(1),"v_bb",]-banks[(1),"v_bbe",])
    }}
  return(banks)
}

Hexp<-function(Firms=stop("need to have Firms defined!"),CB=stop("need to have CB defined!"),Households=stop("need to have Households defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args,Lags=Lags){
  if(args$Timer<13){
    Households[[2,"YD_e"]]=Households[[(1),"YD_e"]]+params[[1,"psi_ad"]]*((sum(Lags[1:(args$Timer-1),"YD"])/length(Lags[1:(args$Timer-1),"YD"]))-Households[[1,"YD_e"]])
    Households[[2,"yd_e"]]=Households[[1,"YD_e"]]+params[[1,"psi_ad"]]*(mean(Lags[1:(args$Timer-1),"YD"]/Lags[1:(args$Timer-1),"p"])-Households[[1,"yd_e"]])
    Households[[2,"V_he"]]=Households[[1,"V_he"]]+params[[1,"psi_ad"]]*((sum(Lags[1:(args$Timer-1),"V_h"])/length(Lags[1:(args$Timer-1),"V_h"]))-Households[[1,"V_he"]])
    Households[[2,"v_he"]]=Households[[1,"v_he"]]+params[[1,"psi_ad"]]*(mean(Lags[1:(args$Timer-1),"V_h"]/Lags[1:(args$Timer-1),"p"])-Households[[1,"v_he"]])
  }else{
    Households[[2,"YD_e"]]=Households[[1,"YD_e"]]+params[[1,"psi_ad"]]*((sum(Lags[(args$Timer-12):(args$Timer-1),"YD"])/length(Lags[(args$Timer-12):(args$Timer-1),"YD"]))-Households[[1,"YD_e"]])
    Households[[2,"V_he"]]=Households[[1,"V_he"]]+params[[1,"psi_ad"]]*((sum(Lags[(args$Timer-12):(args$Timer-1),"V_h"])/length(Lags[(args$Timer-12):(args$Timer-1),"V_h"]))-Households[[1,"V_he"]])
    Households[[2,"yd_e"]]=Households[[1,"YD_e"]]+params[[1,"psi_ad"]]*(mean(Lags[(args$Timer-12):(args$Timer-1),"YD"]/Lags[(args$Timer-12):(args$Timer-1),"p"])-Households[[1,"yd_e"]])
    Households[[2,"v_he"]]=Households[[1,"v_he"]]+params[[1,"psi_ad"]]*(mean(Lags[(args$Timer-12):(args$Timer-1),"V_h"]/Lags[(args$Timer-12):(args$Timer-1),"p"])-Households[[1,"v_he"]])
  }
    if(args$Timer<13){
    Households[[2,"r_mave"]]=Households[[1,"r_mave"]]+params[[1,"psi_ad"]]*((sum(Lags[1:(args$Timer-1),"r_mavr"])/length(Lags[1:(args$Timer-1),"r_mavr"]))-Households[[1,"r_mave"]])
    }else{
    Households[[2,"r_mave"]]=Households[[1,"r_mave"]]+params[[1,"psi_ad"]]*((sum(Lags[(args$Timer-12):(args$Timer-1),"r_mavr"])/length(Lags[(args$Timer-12):(args$Timer-1),"r_mavr"]))-Households[[1,"r_mave"]])
    }
  if(args$Timer<25){
    Households[[2,"rr_he"]]=Households[[1,"rr_he"]]+params[[1,"psi_ad"]]*(sum(Lags[1:(args$Timer-1),"rr_h"])/length(Lags[1:(args$Timer-1),"rr_h"])-Households[[1,"rr_he"]])
    Households[[2,"pi_eh"]]=Households[[1,"pi_eh"]]+params[[1,"psi_ad"]]*(sum(Lags[1:(args$Timer-1),"pi_sa"])/length(Lags[1:(args$Timer-1),"pi_sa"])-Households[[1,"pi_eh"]])
    }else{
    Households[[2,"pi_eh"]]=Households[[1,"pi_eh"]]+params[[1,"psi_ad"]]*(CB[2,"pi_sa"]-Households[[(1),"pi_eh"]])
    Households[[2,"rr_he"]]=Households[[(1),"rr_he"]]+params[[1,"psi_ad"]]*(sum(Lags[(args$Timer-24):(args$Timer-1),"rr_h"])/length(Lags[(args$Timer-24):(args$Timer-1),"rr_h"])-Households[[(1),"rr_he"]])
    }
  return(Households)
}

Fexp<-function(Firms=stop("need to have Firms defined!"),Banks=stop("need to have banks (aggregate) defined!"),params=params,args=args,Lags=Lags){
    Firms[2,"u_n"]=Firms[1,"u_n"]
    if(args$Timer<13){
    Firms[[2,"u_e2"]]=Firms[[1,"u_e2"]]+params[[1,"psi_ad"]]*(sum(Lags[1:(args$Timer-1),"u"])/length(Lags[1:(args$Timer-1),"u"])-Firms[[1,"u_e2"]])
    }else{
    Firms[[2,"u_e2"]]=Firms[[(1),"u_e2"]]+params[[1,"psi_ad"]]*(sum(Lags[(args$Timer-12):(args$Timer-1),"u"])/length(Lags[(args$Timer-12):(args$Timer-1),"u"])-Firms[[1,"u_e2"]])
    }
    if(args$Timer<13){
    Firms[[2,"r_Lave"]]=Firms[[1,"r_Lave"]]+params[[1,"psi_ad"]]*(sum(Lags[1:(args$Timer-1),"r_Lavr"])/length(Lags[1:(args$Timer-1),"r_Lavr"])-Firms[[1,"r_Lave"]])
    }else{
    Firms[[2,"r_Lave"]]=Firms[[1,"r_Lave"]]+params[[1,"psi_ad"]]*(sum(Lags[(args$Timer-12):(args$Timer-1),"r_Lavr"])/length(Lags[(args$Timer-12):(args$Timer-1),"r_Lavr"])-Firms[[1,"r_Lave"]])
    }
  if(args$Timer<25){
    Firms[[2,"u_e"]]=Firms[[1,"u_e"]]+params[[1,"psi_ad"]]*(sum(Lags[1:(args$Timer-1),"u"])/length(Lags[1:(args$Timer-1),"u"])-Firms[[1,"u_e"]])
  }else{
    Firms[[2,"u_e"]]=Firms[[1,"u_e"]]+params[[1,"psi_ad"]]*(sum(Lags[(args$Timer-24):(args$Timer-1),"u"])/length(Lags[(args$Timer-24):(args$Timer-1),"u"])-Firms[[1,"u_e"]])
  }
  Firms[[2,"delta_k"]]=params[[1,"delta_k"]]
  return(Firms)
}


CBexp<-function(CB=stop("need to have CB defined!"),Firms=stop("need to have Firms defined!"),params=params,args=args,Lags=Lags){
  if(args$Timer<5){
    CB[[2,"u_ecb"]]=CB[[1,"u_ecb"]]+params[[1,"psi_ad"]]*(sum(Lags[1:(args$Timer-1),"u"])/length(Lags[1:(args$Timer-1),"u"])-CB[[1,"u_ecb"]])
    CB[[2,"pi_e"]]=CB[[1,"pi_e"]]
  }else{
    CB[[2,"u_ecb"]]=CB[[1,"u_ecb"]]+params[[1,"psi_ad"]]*(sum(Lags[(args$Timer-4):(args$Timer-1),"u"])/length(Lags[(args$Timer-4):(args$Timer-1),"u"])-CB[[1,"u_ecb"]])
    CB[[2,"pi_e"]]=CB[[1,"pi_e"]]+params[[1,"psi_ad"]]*(CB[[2,"pi_m"]]-CB[[1,"pi_e"]])
  }
  return(CB)
}

shiftindex<-function(CB=stop("need to have CB defined!"),Aux=stop("need to have Aux array defined!"),Firms=stop("need to have Firms defined!"),Banks=stop("need to have banks (aggregate) defined!"),banks=stop("need to have banks (ABM) defined!"),Households=stop("need to have Households defined!"),Gov=stop("need to have gov defined!")){
  CB[1,]<-CB[2,]
  Firms[1,]<-Firms[2,]
  Banks[1,]<-Banks[2,]
  Households[1,]<-Households[2,]
  Gov[1,]<-Gov[2,]
  Aux[1,]<-Aux[2,]
  banks[1,,]<-banks[2,,]
  return(list(CB=CB,Aux=Aux,Banks=Banks,banks=banks,Households=Households,Firms=Firms,Gov=Gov))
}

collectdata<-function(Est=stop("need to have est. array defined!"),Lags=stop("need to have Lag array defined!"),CB=stop("need to have CB defined!"),Aux=stop("need to have Aux array defined!"),Firms=stop("need to have Firms defined!"),Banks=stop("need to have banks (aggregate) defined!"),banks=stop("need to have banks (ABM) defined!"),Households=stop("need to have Households defined!"),Gov=stop("need to have gov defined!"),params=params,args=args){
  Est[args$Timer,"r_Mb",]<-banks[2,"r_Mb",]
  Est[args$Timer,"r_Lb",]<-banks[2,"r_Lb",]
  Est[args$Timer,"iL_b",]<-banks[2,"iL_b",]
  Est[args$Timer,"iM_b",]<-banks[2,"iM_b",]
  Est[args$Timer,"L_b",]<-mean(banks[2,"L_b",])
  Est[args$Timer,"M_b",]<-mean(banks[2,"M_b",])
  Est[args$Timer,"v_bb",]<-banks[2,"v_bb",]
  Est[args$Timer,"v_bbe1",]<-banks[2,"v_bbe1",]
  Est[args$Timer,"v_bbe2",]<-banks[2,"v_bbe2",]
  Est[args$Timer,"v_bbe3",]<-banks[2,"v_bbe3",]
  Est[args$Timer,"v_bbe4",]<-banks[2,"v_bbe4",]
  Est[args$Timer,"clear_b",]<-banks[2,"clear_b",]
  Est[args$Timer,"r_mav",]<-Banks[2,"r_mav"]
  Est[args$Timer,"r_Lav",]<-Banks[2,"r_Lav"]
  Lags[args$Timer,"y"]<-Firms[2,"y"]
  Lags[args$Timer,"k"]<-Firms[2,"k"]
  Lags[args$Timer,"p"]<-Firms[2,"p"]
  Lags[args$Timer,"L_np"]<-Firms[2,"L_np"]
  Lags[args$Timer,"L"]<-Firms[2,"L"]
  Lags[args$Timer,"UIC"]<-Firms[2,"UIC"]
  Lags[args$Timer,"i_d"]<-Firms[2,"i_d"]
  Lags[args$Timer,"lev_f"]<-Firms[2,"lev_f"]
  Lags[args$Timer,"u"]<-Firms[2,"u"]
  Lags[args$Timer,"c_d"]<-Households[2,"c_d"]
  Lags[args$Timer,"YD_tax"]<-Households[2,"YD_tax"]
  Lags[args$Timer,"PSBR"]<-Gov[2,"PSBR"]
  Lags[args$Timer,"M_np"]<-Households[2,"M_np"]
  Lags[args$Timer,"M"]<-Households[2,"M"]
  Lags[args$Timer,"W"]<-Households[2,"W"]
  Lags[args$Timer,"p_h"]<-Households[2,"p_h"]
  Lags[args$Timer,"YD"]<-Households[2,"YD"]
  Lags[args$Timer,"V_h"]<-Households[2,"V_h"]
  Lags[args$Timer,"rr_h"]<-Households[2,"rr_h"]
  Lags[args$Timer,"pi_sa"]<-CB[2,"pi_sa"]
  Lags[args$Timer,"r_IB"]<-Banks[2,"r_IB"]
  Lags[args$Timer,"r_mavr"]<-Banks[2,"r_mavr"]
  Lags[args$Timer,"r_Lavr"]<-Banks[2,"r_Lavr"]
  return(list(Est=Est,Lags=Lags))
}
  

storedata<-function(CB=stop("need to have CB defined!"),Aux=stop("need to have Aux array defined!"),Firms=stop("need to have Firms defined!"),Banks=stop("need to have banks (aggregate) defined!"),banks=stop("need to have banks (ABM) defined!"),Households=stop("need to have Households defined!"),Gov=stop("need to have gov defined!"),weekoutput=weekoutput,bankoutput=bankoutput,args=args,datatimer=datatimer){
  datavec<-c(Households[2,],Firms[2,],Gov[2,],CB[2,],Banks[2,],Aux[2,])
  weekoutput[datatimer,]<-datavec
  bankoutput[datatimer,,]<-banks[2,,]
  return(list(weekoutput=weekoutput,bankoutput=bankoutput))
}

reduceddata<-function(Firms=stop("need to have Firms defined!"),output=output,datatimer=datatimer){
  output[datatimer,"y"]<-Firms[2,"y"]
  output[datatimer,"c"]<-Firms[2,"c"]
  output[datatimer,"i"]<-Firms[2,"i"]
  output[datatimer,"p"]<-Firms[2,"p"]
  return(output)
  }
  
  
  
  
  