      FUNCTION ATA(RANG)
*pdb      
*pdb  Edits to make ATA a callable function are labeled with *pdb
*pdb  Peter D. Barnes, Jr. <pdbarnes@llnl.gov>
*pdb      
      
        implicit real*8(A-H,O-Z)
*gh       940915: The change to double precision is required in
*gh         only a few places, but to make certain we have imposed
*gh         it everywhere.
*GH    PROGRAM ATA(INPUT,OUTPUT)       
*ANSI PROGRAM STATEMENT IS NON-ANSI   
****  850606 CORRECTI0NS INCLUDED     
****  850402 CORRECTIONS INCLUDED     
*     POWER AVAILABLE, POWER DENSITY, OR TRANSMISSION LOSS        
*     VERSUS PATH GREAT-CIRCLE DISTANCE OR CENTRAL ANGLE FOR      
*     TIME AVAILABILITIES OF 5, 50, AND 95 PERCENT  
C     ROUTINE FOR MODEL APR 77        
C     READ STATEMENTS MUST HAVE IN SET TO CORRECT UNIT            
C     WRITE STATEMENTS MUST HAVE IOT SET TO CORRECT UNIT          
*     IN AND IOT ARE SET IN THIS PROGRAM            
          
    2 FORMAT('   PROGRAM IS FINISHED. ')            
    4 FORMAT(1H1)       
    5 FORMAT(1H )       
    6 FORMAT(20X,'INPUT',21X,'WORKING VALUE')       
    7 FORMAT(F6.0,I3,2I2,3F5.0,I2,F3.0,2I2,3F6.0,3I3,2F6.0,I2)    
    8 FORMAT(F6.0,I3,2I2,2F5.0,F4.0,3F6.0,I2,2F5.0,5I2,4I3)       
    9 FORMAT(5I2,I3,A32,F5.0,2I2)     
   22 FORMAT(5X,'BEYOND THE 100 MILE LIMIT DOING DIFFRACTION')    
  140 FORMAT(I3)        
  141 FORMAT(' -',I1)                 
  172 FORMAT(32X,'STATE:',I3, /40X,A17,/40X,F5.2,A3,' RMS WAVE HEIGHT',         
     X   /32X,'TEMPERATURE: ',F3.0,' DEG CELSIUS')  
  175 FORMAT(40X,'3.6 PERCENT SALINITY')            
  176 FORMAT(40X,F5.2,A3,' RMS TERRAIN DEVIATION')  
          
C           FORMAT STATEMENTS FOR PARAMETER SHEET AND WORK SHEET  
          
  700 FORMAT(23X,'PARAMETERS FOR ITS PROPAGATION MODEL ',A7)      
  701 FORMAT(32X,'REQUIRED OR FIXED',/32X,'----------------- ',/15X,'AIR        
     1CRAFT (OR HIGHER) ANTENNA ALTITUDE:',F8.0,A3,' ABOVE MSL')  
  702 FORMAT(15X,'FACILITY (OR LOWER) ANTENNA HEIGHT:',F9.1,A3,' ABOVE S        
     2ITE SURFACE')     
  703 FORMAT(15X,'FREQUENCY:',F6.0,' MHZ')          
  704 FORMAT(29X,'SPECIFICATION OPTIONAL',/29X,'----------------------')        
  706 FORMAT(20X,'COUNTERPOISE DIAMETER:',F5.0,A3   ,/25X,'HEIGHT:',F5.0        
     6,A3,' ABOVE SITE SURFACE ',/25X,'SURFACE:',A15)             
  707 FORMAT(20X,'POLARIZATION:',A11)               
  708 FORMAT(15X,'HORIZON OBSTACLE DISTANCE:',F7.2,A5,  ' FROM FACILITY'        
     8,A2,/20X,'ELEVATION ANGLE:  ',A3,'/',I2,'/',I2,' DEG/MIN/SEC ABOVE        
     8 HORIZONTAL',A2,/20X,'HEIGHT:',F6.0,A3,' ABOVE MSL',A2)     
  709 FORMAT(15X,'REFRACTIVITY:',/20X,'EFFECTIVE EARTH RADIUS:', F6.0,A5        
     X,1H',/20X,'MINIMUM MONTHLY MEAN: ',F4.0,' N-UNITS AT SEA LEVEL')          
  710 FORMAT(15X,'TERRAIN ELEVATION AT SITE:',F6.0,A3,' ABOVE MSL',/15X,        
     X'TERRAIN PARAMETER:',F5.0,A3)   
  712 FORMAT(20X,'ANTENNA HEIGHT IS HIGH,  IONOSPHERIC EFFECTS',/25X,           
     2'MAY BE IMPORTANT')             
  713 FORMAT(20X,'AIRCRAFT TOO LOW, TERRAIN BEYOND FACILITY ',/25X,'HORI        
     3ZON MAY BE IMPORTANT')          
  714 FORMAT(20X,'IN ADDITION, SURFACE WAVE CONTRIBUTIONS SHOULD',/15X,'        
     4BE CONSIDERED')                 
  715 FORMAT(15X,'EFFECTIVE REFLECTION SURFACE ELEVATION ABOVE MSL:',F7.        
     50,A3)             
  716 FORMAT(20X,'ANTENNNA TOO LOW, SURFACE WAVE SHOULD BE',/25X,'CONSID        
     6ERED')            
  717 FORMAT(20X,'FREQUENCY TOO LOW, IONOSPHERIC EFFECTS MAY BE',/25X,'I        
     7MPORTANT',//)     
  718 FORMAT(20X,'ATTENUATION AND/OR SCATTERING FROM HYDROMETEORS',/25X,        
     8'(RAIN, ETC) MAY BE IMPORTANT')               
  719 FORMAT(20X,'ATMOSPHERIC ABSORPTION ESTIMATES MAY BE',/25X,'UNRELIA        
     9BLE')             
  720 FORMAT(15X,'AIRCRAFT ANTENNA TYPE: ')         
  721 FORMAT(15X,'FACILITY ANTENNA TYPE: ')         
  722 FORMAT(15X,'EQUIVALENT ISOTROPICALLY RADIATED POWER: ',F6.1)              
  723 FORMAT(32X,'REQUIRED OR FIXED',/32X,'----------------- ',/15X,'AIR        
     1CRAFT (OR HIGHER) ANTENNA ALTITUDE:',F8.0,A5,' ABOVE MSL')  
  724 FORMAT(/15X,A2,'COMPUTED VALUE')              
  725 FORMAT(12X,'D(HE) ',F8.0,A3,'   ',F8.0,' KM',A2)            
  726 FORMAT(12X,'EARTH',F9.0 ,A5,15X,F8.0,' KM')   
  727 FORMAT(12X,' H(A) ',F8.0,A5,' MSL           ',F10.1, ' KM MSL')           
  728 FORMAT(12X,'HRE= ',F8.4,'-',F8.4,'-',F8.4,' = ',F8.4,' KM') 
  729 FORMAT(15X,'TIME AVAILABILITY:  ',A33)        
  731 FORMAT(12X,' H(A) ',F8.0,A3,' MSL             ',F8.4,' KM MSL')           
  732 FORMAT(12X,' H(F) ',F8.1,A3,' MSL             ',F8.4,' KM MSL')           
  733 FORMAT(12X,'FREQUENCY', F5.0,' MHZ            ',F8.0,'  MHZ ')            
  734 FORMAT(12X,' A(O)', F9.5,' DB/KM              ',F8.5,' DB/KM',A2)         
  735 FORMAT(12X,' A(W)',F9.5 ,' DB/KM              ',F8.5,' DB/KM',A2)         
  736 FORMAT(12X,'D(HE) ',F8.0,A3,'   ',F8.4,' KM',A2)            
  737 FORMAT(12X,'EIRP ',F9.1 ,' DBW           P CON',F8.1,'  DBW ')            
  738 FORMAT(12X,'F ANT ',6X,I2)      
  739 FORMAT(12X,' D(C) ',F8.0,A3,17X,F8.4,' KM')   
  740 FORMAT(12X,' H(C) ',F8.1,A3,' ABOVE SURFACE   ',F8.4,' KM') 
  741 FORMAT(12X,'COUNTERPOISE',I2,10X,A15)         
  742 FORMAT(12X,'H(FR) ',F8.1,A3,' ABOVE REFLECTION',F8.4,' KM') 
  743 FORMAT(12X,'POLARIZATION',I2,10X,A11)         
  744 FORMAT(15X,'SURFACE TYPE: ',A15)              
  745 FORMAT(10X,A2,'D(HO) ',F8.2,A5,  ' FROM HORIZON  ',F8.2,' KM')            
  746 FORMAT(10X,A2,'E(HO) ',A3,'/',I2,'/',I2,' DEG/MIN/SEC',6X,F8.5,' R        
     6ADIANS')          
  747 FORMAT(10X,A2,'H(HO) ',F8.0,A5,' MSL             ',F8.4,' KM')            
  748 FORMAT(12X,' N(O)',F9.0 ,' N-UNITS       N(S) ',F8.0,' N-UNITS')          
  749 FORMAT(12X,'H(SUR)',F8.0,A3,' MSL             ',F8.4,' KM') 
  750 FORMAT(12X,'DH(SUR)',F7.0,A3,17X,F8.4,' KM')  
  751 FORMAT(12X,'TERRAIN',5X,I2,10X,A15)           
  756 FORMAT(20X,'BEAMWIDTH, HALF-POWER:',F7.2,' DEGREES')        
****  850402            
* 757 FORMAT(12X,'INPUT PARAMETERS FOR',A7,'AIR TO AIR MODEL',//) 
          
  760 FORMAT(1X,F7.2,13F7.1,F6.1,2F5.1,F6.1,A5)     
  761 FORMAT(5X,'HORIZON POW=',F7.1,' AWD=',F8.2,' SLOPE=',F8.2)  
  767 FORMAT(2F7.3,3F7.2,F5.0,F6.0,F5.0,F7.3,2F8.5)               
  768 FORMAT(3F7.3,2F7.1,2F7.2, 5X,4F7.1,E13.5)     
  771 FORMAT(12X,'HRE= ',F8.0,'-',F8.0,'-',F8.4,' = ',F10.1,' KM')              
  772 FORMAT('   HTE    HRE     D     DLT    DLR  ENS  ERTH FREK LAMDA          
     X   TET     TER')                
  773 FORMAT('  HFS    HRS     DH    AED    SLP   DLST   DLSR     
     X DD NM  LBF     AT     D0         WRH')       
  775 FORMAT(/12X,'POWER DENSITY INTO POWER AVAILABLE ADD     ',F6.1,/)         
  776 FORMAT(15X,'POWER DENSITY (DB-W/SQ M) VALUES MAY BE CONVERTED TO P        
     XOWER',/20X,'AVAILABLE AT THE TERMINALS OF A PROPERLY POLARIZED',/2        
     X0X,'ISOTROPIC ANTENNA (DBW) BY ADDING ',F6.1,' DB-SQ M.')   
  778 FORMAT(15X,'SURFACE REFLECTION LOBING:  CONTRIBUTES TO VARIABILITY        
     X')                
  779 FORMAT(15X,'SURFACE REFLECTION LOBING:  DETERMINES MEDIAN') 
  785 FORMAT(12X,'SURFACE REFLECTION LOBING:  CONTRIBUTES TO VARIABILITY        
     X')                
  786 FORMAT(12X,'SURFACE REFLECTION LOBING:  DETERMINES MEDIAN') 
  794 FORMAT(15X,'RAIN ATTENUATION ZONE:',I2,/20X,'SIZE OF STORM:',F3.0,        
     X' KM')            
  795 FORMAT(20X,'FREQUENCY SCALING FACTOR USED')   
  796 FORMAT(15X,'IONOSPHERIC SCINTILLATION INDEX GROUP:',I2)     
  797 FORMAT(15X,'IONOSPHERIC SCINTILLATION INDEX: VARIABLE (SEE TEXT)')        
  800 FORMAT(//10X,'SOME PARAMETERS ARE OUT OF RANGE')            
  809 FORMAT(20X,'DLT IS LESS THAN .1XDLST OR GREATER THAN 3XDLST')             
  810 FORMAT(20X,'INITIAL TAKE-OFF ANGLE GREATER THAN 12 DEG.')   
  818 FORMAT(15X,'GAIN SUM OF MAIN BEAMS:',F6.1,' DBI')           
  819 FORMAT(15X,'GRAPH IS IN POWER AVAILABLE')     
  820 FORMAT(12X,'HTE= ',F8.4,'-',F8.4,'-',F8.4,' = ',F8.4,' KM') 
  840 FORMAT(20X,'ANTENNA IS TRACKING')             
  850 FORMAT(20X,'TILT IS',F6.1,' DEGREES ABOVE THE HORIZONTAL')  
  851 FORMAT (15X,'EIRP PLUS RECEIVING ANTENNA MAIN BEAM GAIN:',F7.1,           
     C' DBW')           
          
      CHARACTER*2 PXH,PHS,PDS,PTS,PAS(2),BMOD*5,UN*3,UD*5,BLANK*34              
      CHARACTER LL(3)*22,TT*32,PDH*2,QMD*7          
      CHARACTER*5 CMOD,EMOD,SMOD,TSC(7)*15,POL(3)*11,VYD(2)*33    
****  850402 CBUF ADDED               
      CHARACTER CBUF*80               
      CHARACTER SSS(10)*17,TYD(2)*17,UND(3)*5, UNH(3)*3           
          
      DIMENSION ACD(101),AND(101),SCT(101),AAD(101),RW(101),GS(101),DQD         
     X(101)             
      DIMENSION CFK(3),CMK(3),CFM(3),CKM(3),CKN(3)  
****  940914  REMOVE SYSTEMC
****  850607  ADD IRRAY FOR SYSTEMC   
*     DIMENSION IRRAY(6)
      DIMENSION MTM(5),YCON(5)        
      DIMENSION NDM(5),XDON(5)        
      DIMENSION P(35),QC(35),QA(35),PQA(35),PQK(35),QK(35),PQC(35)              
      DIMENSION RQ(2),QR(35),BD(35),ALM(12),SSH(10)               
          
      COMMON/DIFPR/HTD,HRD,DH,AED,SLP,DLST,DLSR,IPL,KSC,HLD,HRP,AWD,SWP         
     X,II               
      COMMON/DROPS/RS(35),IZ,STS,FQX,DX,HA1,HA2,EFA,AN            
      COMMON/EGAP/IP,LN               
      COMMON/PARAM/HTE,HRE,D,DLT,DLR,ENS,EFRTH,FREK,ALAM,TET,TER,KD,GAO,        
     XGAW               
      COMMON/PLTD/NU(8),BX(200,8),BY(200,8)         
      COMMON/RYTC/QNS,QHC,QHA,QHS,QQD               
      COMMON/SCTBL/HT,HR,ALSC,TWEND,THRFK,HLT,HLR,THETA,HTP,AA,REW              
      COMMON /SEA/ ISK,SCK,TP,JM,EPK,SGM            
      COMMON/SELCT/RQD,JA,JB,DQM,DBY(12),SELT(12),JG              
      COMMON/SIGHT/DCW,HCW,DMAX,DML,DOZ,IK,EAC,H2,ICC,HFC,PRH,DSL1,PIRP,        
     XQG1,QG9,PFY(200,4),KK,ZH,RDHK,ILB,EAL,IFA,IAA,T1T,HLPBW,T2T,H2PBW,        
     XJT,JS,H1,IJ,JC,NPL,JO,RY2,RY1   
      COMMON/SOI/THO,FS,IOS,IPK,QS(35)              
      COMMON/VARY/KLM,MX1,KLM2,MX2,FKE,AD(35)       
      COMMON/VV/VF(36,17)             
      COMMON/PRINT/KL,IN,IOT          
      COMMON/CURVE/PI,RAD,DEG,TWPI,PITW             
      CHARACTER*80 CARDS(4)
      COMMON/FREAD/CARDS
          
      DATA ALM/-6.2,-6.15,-6.08,-6.0,-5.95,-5.88,-5.8,-5.65,-5.35,-5.0,-        
     X4.5,-3.7/         
      DATA CFK/.001,.0003048,.0003048/              
      DATA CFM/1.,.3048,.3048/        
      DATA CKM/1000.,3280.839895,3280.839895/       
      DATA CKN/1.,.6213711922,.5399568034/          
      DATA CMK/1.,1.609344,1.852/     
****  850402            
*     DATA CMOD,EMOD,SMOD,QMD/' COMB',' DIFR',' SCAT',' APR 77'/  
      DATA CMOD,EMOD,SMOD,QMD/' COMB',' DIFR',' SCAT','APR 77'/   
      DATA MTM/20,10,30,0,0/          
      DATA NDM/10,20,10,10,0/         
      DATA P/.00001,.00002,.00005,.0001,.0002,.0005,.001,.        
     X002,.005,.01,.02,.05,.10,.15,.20,.30,.40,.50,.60,.70,.80,.85,.90,.        
     X95,.98,.99,.995,.998,.999,.9995,.9998,.9999,.99995,.99998,.99999/         
      DATA PAS/'  ','* '/             
****  850402            
*     DATA POL/' HORIZONTAL',' VERTICAL','CIRCULAR'/              
      DATA POL/' HORIZONTAL',' VERTICAL  ',' CIRCULAR  '/         
      DATA SSH/0.,0.02,0.11,0.25,0.46,0.76,1.2,2.,3.,3.3/         
      DATA SSS/'CALM (GLASSY)','CALM (RIPPLED)','SMOOTH (WAVELETS)',            
     X'SLIGHT','MODERATE','ROUGH','VERY ROUGH','HIGH','VERY HIGH',              
     X'PHENOMENAL'/     
      DATA TSC/' SEA WATER',' GOOD GROUND',' AVERAGE GROUND',     
****  850402            
*    X' POOR GROUND',' FRESH WATER',' CONCRETE',' METTALIC'/      
     X' POOR GROUND',' FRESH WATER',' CONCRETE',' METALLIC'/      
      DATA TYD/'SMOOTH EARTH','IRREGULAR TERRAIN'/  
      DATA UND/' KM',' S MI',' N MI'/               
      DATA UNH/' M',' FT',' FT'/      
      DATA VYD/'FOR HOURLY MEDIAN LEVELS EXCEEDED',               
     X'FOR INSTANTANEOUS LEVELS EXCEEDED'/          
      DATA XDON/.5,1.,2.,5.,0./       
      DATA YCON/5.,10.,25.,0.,0./     
****  850402            
*     DATA LL/' POWER AVAILABLE FOR ','  POWER DENSITY FOR ','TRANSMISSI        
*    XON LOSS FOR '/    
      DATA LL/'  POWER AVAILABLE FOR ','    POWER DENSITY FOR ','TRANSMI        
     XSSION LOSS FOR '/               
      DATA BLANK/' '/                 
C         
      FNA(FX,FA,FB,FC,FD)=((FX-FB)*(FC-FD)/(FA-FB))+FD            
          
**** 850402 ADD IN AND IOT SPECIFICATIONS           
*GH    CBUF(1:10)='INPUT:::::'         
*GH    READ(CBUF(1:10),'(A10)') IN     
*GH    CBUF(1:10)='OUTPUT::::'         
*GH    READ(CBUF(1:10),'(A10)') IOT    
      IN=5
      IOT=6
      PI=4.*ATAN(1.)    
      ERTH=6370.        
      VERTH=1./ERTH     
      THIRD=1./3.       
      RAD=PI/180.       
      DEG=180./PI       
      TPTH=2.617993878E-2             
      TLTH=0.           
      TPK=20.           
      TWPI=2.*PI        
      PITW=PI*0.5       
      TWDG=12.*RAD      
      ZO=.00000001      
      ZH=0.             
      II=0
****  940914  REMOVE SYSTEMC
****  850607  ADD SYSTEMC             
*     IRRAY(4)=0
*ANSI NON-ANSI NAME LENGTH, SYSTEM ERROR MESSAGE CONTROL          
*     SUPRESS EXPONENT TOO SMALL MESSAGES           
*     CALL SYSTEMC(115,IRRAY)         
          
C     -------------PROGRAM START WITH CARD 1--------------------- 
*pdb  
 1001 GO TO 1002
*pdb  Out of range, return      
 100  ATA=-400
      RETURN
 1002 CONTINUE
*pdb  READ(IN,9)IK,IO,IJ,ILB,KK,IA,TT,DMAX,JC,IGPH  
      READ(CARDS(1),9)IK,IO,IJ,ILB,KK,IA,TT,DMAX,JC,IGPH  
*pdb      
      JB=1              
      IF (IGPH .GT. 1) JA=1           
      RQD=DMAX          
*pdb  WRITE(IOT,4)      
      IF(IK.LE.0) GO TO 451           
      ICAR=0            
      NOC=0             
C     -----------------INPUT OF CARD 2--------------------------- 
*pdb  READ(IN,7)HLA,IFA,JT,IPL,T1T,HLPBW,SUR,IZ,STS,KD,KE,DHSI,DHOI,HHOI        
*pdb X,IDG,IMN,ISEC,DCI,HCI,ICC       
      READ(CARDS(2),7)HLA,IFA,JT,IPL,T1T,HLPBW,SUR,IZ,STS,KD,KE,DHSI,
     XDHOI,HHOI,IDG,IMN,ISEC,DCI,HCI,ICC       
*pdb  CALL PAGE(-1)     
*pdb  WRITE(IOT,5)      
          
C     -----------------START OF PARAMETER SHEET------------------ 
*pdb  WRITE(IOT,700)QMD               
C     ---------------INPUT OF CARD 3 IF NECESSARY---------------- 
*pdb  READ(IN,8)HAI,IAA,JS,NPL,T2T,H2PBW,ENO,F,EIRP,HPFI,KSC,TP,SCK,ISS,        
*    XJM,IOS,IPK,JO,KLM,MX1,KLM2,MX2                
      READ(CARDS(3),8)HAI,IAA,JS,NPL,T2T,H2PBW,ENO,F,EIRP,HPFI,KSC,TP,
     XSCK,ISS,JM,IOS,IPK,JO,KLM,MX1,KLM2,MX2                
*pdb      
      FKE=F             
**** 850402             
*     NK=43-((20+IA)/2)               
      NK=38-((20+IA)/2)               
      UN=UNH(IK)        
      UD=UND(IK)        
*pdb  WRITE(IOT,'(A)')BLANK(1:NK)//LL(IO)//TT       
      IF(KSC.LT.1) KSC=3              
      IF(IJ.GT.0) GO TO 80            
      H2=HAI*CFK(IK)    
*pdb  WRITE(IOT,701)HAI,UN            
 81   CONTINUE
*pdb  IF(HAI.GT.150000.) WRITE(IOT,712)             
*pdb  IF(HAI.LT.500.) WRITE(IOT,713)                
*pdb  IF(HAI.LT.1.5) WRITE(IOT,714)   
      IF(HAI.LT.0.) GO TO 825         
      H1=HLA*CFK(IK)    
      FS=F              
      FQX=F             
      FREK=F            
      HFP=HLA-SUR       
      IF(F.LT.100.)GO TO 805          
  806 IF(F.LT.20.) GO TO 100          
*pdb  IF(F.GT.5000.) WRITE(IOT,718)   
      IF(F.GT.17000.) GO TO 807       
  808 IF(F.GT.100000.) GO TO 100      
      CALL ASORP(F,AOI,AWI)           
      PXH=PAS(2)        
      GAO=AOI           
      GAW=AWI           
      IF(SUR.GT.15000.) ICAR=1        
      IF(SUR.LT.0.) GO TO 830         
  831 HRP=HPFI*CFK(IK)                
      ETS=SUR*CFK(IK)                 
      IF(ETS.LT.0.) ETS=0.            
      IF(SUR.GT.15000.) ICAR=1        
      IF(DHSI.LT.0.) DHSI=0.          
      DH=DHSI*CFK(IK)                 
      IF (ENO .LT. 200.) ENO=200.     
      IF (ENO .GT. 400.) GO TO 801    
  802 ENS=ENO*EXP (-0.1057*HRP)       
      IF (ENS .LT. 200.) ENS=200.     
      EFRTH=ERTH/(1.-.04665*EXP(.005577*ENS))       
      EART=EFRTH*CKN(IK)              
  804 CONTINUE          
      IF(HRP.GT.H1) GO TO 825         
          
C     ------------------CALCULATION OF RAY BENDING--------------- 
      PDH=PAS(2)        
      HP2=H2-HRP        
      HP1=H1-HRP        
      DUM=0.0           
      ZER=0.0           
      QLIM=-1.56        
      QNS=329.          
      QHC=HP1           
      QHA=HP2           
      QHS=HRP           
*      CALL RYTRA(DUM)                 
      DUM=RYTRA(DUM)
      RY=TRCRY(QLIM)    
      DS0=QQD           
      QNS=ENS           
      QHC=ZER           
      QHA=HP2           
      QHS=HRP           
*      CALL RYTRA(DUM)                 
      DUM=RYTRA(DUM)
      RY2=TRCRY(ZER)    
      DLSR=QQD          
      TSL2=DLSR/EFRTH                 
      IF(TSL2.LE..1)  GO TO 53        
      R2E=EFRTH/COS (TSL2)            
      HRE=R2E-EFRTH     
   54 IF(HRE.GT.HP2) GO TO 56         
   57 HR=HRE+HRP        
      EAC=H2-HRP-HRE    
      IF(EAC.LE.0.) GO TO 38          
   47 DHEI=EAC*CKM(IK)                
      IF(IJ.GT.0) DHEI=EAC*CKN(IK)    
      QNS=ENS           
      QHC=ZER           
      QHA=HP1           
      QHS=HRP           
      RY1=TRCRY(ZER)    
      DLST=QQD          
      TSL1=DLST/EFRTH                 
      IF(TSL1.LE..1)  GO TO 60        
      R1E=EFRTH/COS (TSL1)            
      HTE=R1E-EFRTH     
   61 IF(HTE.GT.HP1) GO TO 75         
   73 HT=HTE+HRP        
      EAL=H1-HRP-HTE    
      IF(EAL.LE.0.) GO TO 58          
   59 DHEL=EAL*CKM(IK)                
      HFS=HT-ETS        
      HFI=HFS*CKM(IK)                 
      HFRI=HTE*CKM(IK)                
*pdb  WRITE(IOT,702)HFP,UN            
      IF(HFI.LT.0.) GO TO 825         
*pdb  IF(HFI.LT.1.5) WRITE(IOT,716)   
*pdb  WRITE(IOT,703)FREK              
*pdb  WRITE(IOT,5)      
*pdb  WRITE(IOT,704)    
*pdb  WRITE(IOT,720)    
      CALL ANTNA(0.,IAA,H2PBW,T2T,GAVD,GAHD,GAV,IPL)              
      H2PTW=2.*H2PBW    
*pdb  IF(H2PBW.GT..01) WRITE(IOT,756)H2PTW          
*pdb  WRITE(IOT,707) POL(NPL)         
      IF(T2T.GE..01.OR.T2T.LE.(-.01)) WRITE(IOT,850)T2T           
*pdb  IF(JS.GT.0) WRITE(IOT,840)      
*pdb  WRITE(IOT,715)HPFI,UN           
      IF(IO-2) 791,792,793            
  791 PIRP=EIRP         
*pdb  WRITE(IOT,851) EIRP             
      GO TO 817         
  793 PIRP=EIRP         
*pdb  WRITE(IOT,818) EIRP             
      GO TO 817         
 792  CONTINUE
*pdb  WRITE(IOT,722)EIRP              
      PDCON=38.544-20.*LOG10(F)      
      PIRP=EIRP-PDCON                 
 817  CONTINUE
*pdb  WRITE(IOT,721)    
      CALL ANTNA(0.,IFA,HLPBW,T1T,GFVD,GFHD,GFV,IPL)              
      HLPTW=2.*HLPBW    
*pdb  IF(HLPBW.GT..01) WRITE(IOT,756)HLPTW          
*pdb  WRITE(IOT,707) POL(IPL)         
*pdb  IF(T1T.GE..01.OR.T1T.LE.(-.01)) WRITE(IOT,850)T1T           
*pdb  IF(JT.GT.0) WRITE(IOT,840)      
      IF(DCI.LE.ZO) GO TO 789         
      IF(ICC.LE.0) GO TO 789          
****  850402 ADD TEST ON HCI          
      IF(HCI.LE.ZO) GO TO 789         
          
C     -------------COUNTERPOISE PARAMETERS CONVERTED------------- 
      NOC=1             
      DCW=DCI*CFK(IK)                 
      HCW=HCI*CFK(IK)                 
*pdb  WRITE(IOT,706)DCI,UN,HCI,UN,TSC(ICC)          
      IF(HCI.LT.0.) GO TO 828         
  829 IF(HCI.GT.500.) ICAR=1          
      IF (DCW.GT..1524) ICAR=1        
      IF(HCW.GT.HFS) GO TO 825        
      HFC=HT-ETS-HCW    
  788 CONTINUE          
          
C     ------HORIZON AND INITIAL TAKE-OFF ANGLE COMPUTATIONS------ 
      PDS=PAS(1)        
      PTS=PAS(1)        
      PHS=PAS(1)        
      IF(KD.LE.1) GO TO 755           
      HLT=HHOI*CFK(IK)                
      DLT=DHOI*CMK(IK)                
      HLTS=HLT-HT       
      HLR=HLT           
      DG=IDG            
      AMN=IMN           
      SEC=ISEC          
      TET=RAD*(DG+(((SEC/60.)+AMN)/60.))            
      TATET=TAN (TET)                 
      IF(KE.EQ.3) GO TO 782           
      IF(DLT.LE.ZO) GO TO 781         
  759 IF(KE-1)730,758,780             
  758 IF(TET.LT.0.) GO TO 752         
      HLTS=DLT*TATET+(DLT*DLT/(2.*EFRTH))           
****  850402            
*     ENCODE(3,140,ADG)IDG            
      WRITE(CBUF(1:3),140) IDG        
      READ(CBUF(1:3),'(A3)') ADG      
  753 HLT=HLTS+HT       
      HHOI=HLT*CKM(IK)                
      PHS=PAS(2)        
  783 CONTINUE          
      HLR=HLT           
      IF(HTE.GT.3.) GO TO 74          
 72   CONTINUE
*pdb  IF(DLT.LT.(.1*DLST).OR.DLT.GT.(3.*DLST)) WRITE(IOT,809)     
*pdb  IF(TET.GT..20943951) WRITE(IOT,810)           
      IF(HHOI.GT.15000.) ICAR=1       
      TO1=-TET-(DLT/EFRTH)            
      IF(EAL.LE.0.) GO TO 10          
      QHC=HLT-HRP       
      QHA=HP1           
      QHS=HRP           
      RY=TRCRY(TO1)     
      TET=-RY           
      TATET=TAN (TET)                 
      DLT=QQD           
   10 TO2=-TO1          
      IF(EAC.LE.0.) GO TO 16          
      QHC=HLT-HRP       
      QHA=HP2           
      QHS=HRP           
      RY=TRCRY(TO2)     
      TER=-RY           
      TATER=TAN (TER)                 
      DLR=QQD           
   18 DML=DLR+DLT       
 70   CONTINUE
*pdb  WRITE(IOT,708)DHOI,UD,PDS,ADG,IMN,ISEC,PTS,HHOI,UN,PHS      
C     ----------------------------------------------------------- 
      IF(JO.LE.0) GO TO 79            
      IF(IOS.LT.0) GO TO 84           
*pdb  WRITE(IOT,796)IOS               
 85   CONTINUE
*pdb  IF(IPK.GT.0) WRITE(IOT,795)     
   79 CONTINUE          
      IF(IZ.LE.0) GO TO 76            
*pdb  WRITE(IOT,794)IZ,STS            
   76 CONTINUE          
*pdb  WRITE(IOT,709)EART,UD,ENO       
      IF(ILB.GT.0) GO TO 762          
*pdb  WRITE(IOT,778)    
  763 CONTINUE          
      CALL SIG(KSC,F)                 
*pdb  WRITE(IOT,744)TSC(KSC)          
      IF(KSC.EQ.1.OR.KSC.EQ.5) GO TO 173            
      IF(JM.GT.0) GO TO 174           
      ISK=0             
      SCK=0.            
      RMS=0.            
  177 CONTINUE          
*pdb  WRITE(IOT,710)SUR,UN,DHSI,UN    
*pdb  WRITE(IOT,729)VYD(KK)           
      DE=0.             
      CALL VARIB(DE)    
*pdb  IF(IO.EQ.2) WRITE(IOT,776)PDCON               
*pdb  WRITE(IOT,724)PAS(2)            
      IF(DMAX.GT.1000.) DMAX=1000.    
*pdb  IF(ICAR.GT.0) WRITE(IOT,800)    
          
C     ------------------START OF WORK SHEET---------------------- 
*pdb  WRITE(IOT,4)      
*pdb  CALL PAGE(0)      
*pdb  WRITE(IOT,5)      
**** 850402             
*     WRITE(IOT,757)QMD               
*pdb  WRITE(IOT,700)QMD               
*pdb  WRITE(IOT,'(A)')BLANK(1:NK)//LL(IO)//TT       
*pdb  WRITE(IOT,5)      
*pdb  WRITE(IOT,6)      
      IF(IJ.GT.0) GO TO 82            
*pdb  WRITE(IOT,731)HAI,UN,H2         
*pdb  WRITE(IOT,736)DHEI,UN,EAC,PDH   
*pdb  WRITE(IOT,728)H2,EAC,HRP,HRE    
   83 CONTINUE          
*pdb  WRITE(IOT,732)HLA,UN,H1         
*pdb  WRITE(IOT,736)DHEL,UN,EAL,PDH   
*pdb  WRITE(IOT,820)H1,EAL,HRP,HTE    
*pdb  WRITE(IOT,733)F,FREK            
*pdb  WRITE(IOT,734)AOI,GAO,PXH       
*pdb  WRITE(IOT,735)AWI,GAW,PXH       
*pdb  WRITE(IOT,737)EIRP,PIRP         
*pdb  WRITE(IOT,738)IFA               
*pdb  IF(HLPBW.GT..01) WRITE(IOT,756)HLPTW          
*pdb  IF(T1T.GE..01.OR.T1T.LE.(-.01)) WRITE(IOT,850)T1T           
*pdb  IF(JT.GT.0) WRITE(IOT,840)      
*pdb  WRITE(IOT,738)IAA               
*pdb  IF(H2PBW.GT..01) WRITE(IOT,756)H2PTW          
*pdb  IF(T2T.GE..01.OR.T2T.LE.(-.01)) WRITE(IOT,850)T2T           
*pdb  IF(JS.GT.0) WRITE(IOT,840)      
      IF(NOC.LT.1) GO TO 754          
*pdb  WRITE(IOT,739)DCI,UN,DCW        
*pdb  WRITE(IOT,740)HCI,UN,HCW        
*pdb  WRITE(IOT,741)ICC,TSC(ICC)      
  754 CONTINUE          
*pdb  WRITE(IOT,742)HFRI,UN,HTE       
*pdb  WRITE(IOT,743)IPL,POL(IPL)      
*pdb  WRITE(IOT,745)PDS,DHOI,UD,DLT   
*pdb  WRITE(IOT,746)PTS,ADG,IMN,ISEC,TET            
*pdb  WRITE(IOT,747)PHS,HHOI,UN,HLT   
*pdb  WRITE(IOT,748)ENO,ENS           
*pdb  WRITE(IOT,726)EART,UD,EFRTH     
*pdb  WRITE(IOT,749)SUR,UN,ETS        
*pdb  WRITE(IOT,750)DHSI,UN,DH        
*pdb  WRITE(IOT,751)KSC,TSC(KSC)      
*pdb  IF(KSC.EQ.1.OR.KSC.EQ.5) WRITE(IOT,172)ISS,SSS(ISK),RMS,UN,TP             
      IF(ILB.GT.0) GO TO 764          
*pdb  WRITE(IOT,785)    
  765 CONTINUE
*pdb  IF(IO.EQ.2) WRITE(IOT,775)PDCON               
*pdb  IF(IO.EQ.3) WRITE(IOT,818)PIRP                
*pdb  IF(IO.EQ.1) WRITE(IOT,819)      
*pdb  WRITE(IOT,729)VYD(KK)           
      CALL VARLB(DE)    
*pdb  WRITE(IOT,724)PAS(2)            
*pdb  WRITE(IOT,5)      
*pdb  WRITE(IOT,5)      
*pdb  IF(ICAR.GT.0) WRITE(IOT,800)    
C     --------------END OF PRELIMINARY PRINTING------------------ 
          
      DSD=65.*((100./F)**THIRD)       
      DSL1=DS0+DSD      
      ALAM=.2997925/F                 
*pdb  WRITE(IOT,4)      
*pdb  CALL PAGE(0)      
*pdb  WRITE(IOT,5)      
      AFP=32.45+20.*LOG10(FREK)      
          
C     ----HORIZON POINT DISTANCE AND PARAMETER CALCULATION------- 
      DNM=DML*CKN(IK)                 
      D=DML             
      TWEND=20.*LOG10(D)             
      ALFS=AFP+TWEND    
      DLR=D-DLT         
      HTP=HRP           
      DRP=DLSR          
      TES=-RY2          
      TATES=TAN (TES)                 
      TEK=TER           
      IF(HLT-HRP)15,15,14             
   15 DHRP=DLSR+DLT     
      DPRO=0.           
      GO TO 13          
   14 DHRP=DLT+DLSR+SQRT (2.*EFRTH*(HLT-HRP))       
      DPRO=(TES-TER)/(DHRP-DML)       
   13 CONTINUE          
          
C     ----------------------DIFFRACTION----------------------     
      T1=(H1-HLT)**2+4.*(6370.+H1)*(6370.+HLT)*(SIN(0.5*DLT*VERTH))**2          
      T1=SQRT(T1)       
      HTD=HT            
      HRD=HR            
      HLD=HLT           
      CALL DFRAC        
      IF(JT.GT.0)T1T=TET*DEG          
      CALL GANE(TET,IFA,HLPBW,T1T,GFVD,GFHD,GFV,IPL)              
      IF(JS.GT.0)T2T=TER*DEG          
      CALL GANE(TER,IAA,H2PBW,T2T,GAVD,GAHD,GAV,IPL)              
      GVD=GFV*GAV       
      IF (IPL .GE. 3) GVD=0.5*(GFVD*GAVD+GFHD*GAHD)               
      GDD=20.*LOG10(GVD)             
      SMD=((AINT(DNM/1.))*1.)+1.      
      AMD=AWD+(SWP*D)                 
      ATD=AMD           
      ARD=AMD           
      DZR=-(AWD/SWP)    
      DOZ=DZR           
      PRH=-(AMD-GDD)    
      WRH=10.**(PRH*.1)               
C     -----------------------WRITE STATEMENTS-------------------- 
*pdb  WRITE(IOT,772)    
*pdb  WRITE(IOT,767)HTE,HRE,D,DLT,DLR,ENS,EFRTH,FREK,ALAM,TET,TER 
*pdb  WRITE(IOT,773)    
*pdb  WRITE(IOT,768) HT,HR ,DH,AED,SLP,DLST,DLSR,DNM,ALFS,AMD,DZR,WRH           
*pdb  WRITE(IOT,761)PRH,AWD,SWP       
*pdb  WRITE(IOT,5)      
*pdb  CALL PAGE(6)      
C     ----------------------------------------------------------- 
          
      KS=1              
      ACD(KS)=ARD       
      AND(KS)=DML       
      BMOD=EMOD         
      EC1=HTE+EFRTH     
      EC2=HRE+EFRTH     
      EC3=HLT-HRP+EFRTH               
      IF(EC1.GT.EC3) GO TO 63         
      CALL SORB(EC1,EC3,EFRTH,DLT,TET,RQ)           
   64 RO1=RQ(1)         
      RW1=RQ(2)         
      IF(EC2.GT.EC3) GO TO 65         
      CALL SORB(EC2,EC3,EFRTH,DLR,TER,RQ)           
   66 RO2=RQ(1)         
      RW2=RQ(2)         
      REO=RO1+RO2       
      REW=RW1+RW2       
      AA=GAO*REO+GAW*REW              
      RW(1)=REW         
      AAD(1)=AA         
      T2=(H2-HLR)**2+4.*(6370.+H2)*(6370.+HLR)*(SIN(0.5*DLR*VERTH))**2          
      T2=SQRT(T2)       
      DQS=DML-DLT-DLR                 
      IF(DQS.LT.0.) DQS=0.            
      DQ=T1+T2+DQS      
      ALFS=AFP+20.*LOG10(DQ)         
      DQD(1)=ALFS       
      GS(1)=GDD         
C     ------------------LINE-OF-SIGHT---------------------------- 
*pdb
*     CALL AALOS        
      ATA=AALOS(RANG)
      IF(D.GE.RANG) RETURN
*pdb      
      IDD=0             
      IF (JG .GT. 0) GO TO 100        
      HA1=EC1           
      HA2=EC2           
      EFA=EFRTH         
      AN=TET            
      DX=T1+T2          
      IF(IZ.GT.0) CALL RAIN           
      NCT=NU(1)         
      SPD=SMD+2.        
          
C     -------------BEYOND THE HORIZON CALCULATIONS--------------- 
      KFD=0             
      NSP=0             
      IF(JC.GT.0) GO TO 960           
  900 NSP=NSP+1         
      IF(NSP.GT.5) GO TO 907          
      MZS=MTM(NSP)      
      IF(MZS.LE.0) GO TO 907          
      MXS=0             
  901 MXS=MXS+1         
      IF(MXS.GT.MZS) GO TO 908        
      D=SPD*CMK(IK)     
      DNM=SPD           
  953 IF(D.GT.DHRP) GO TO 17          
      DLR=D-DLT         
      HLR=HLT           
      TER=TEK+(DPRO*(DHRP-D))         
      TATER=TAN (TER)                 
   19 CONTINUE          
      IF(JS.GT.0)T2T=TER*DEG          
      CALL GANE(TER,IAA,H2PBW,T2T,GAVD,GAHD,GAV,IPL)              
      GVD=GFV*GAV       
      IF (IPL .GE. 3) GVD=0.5*(GFVD*GAVD+GFHD*GAHD)               
      GDD=20.*LOG10(GVD)             
      IF(KFD-1)40,41,42               
   40 KR=0              
      KC=0              
   30 KC=KC+1           
      IF(KC.GT.100) GO TO 910         
      KS=KS+1           
      D=DNM*CMK(IK)     
      SPD=DNM           
      ACD(KS)=AED+(SLP*D)             
      AND(KS)=D         
      IF(D.GT.DHRP) GO TO 44          
      HLR=HLT           
      DLR=D-DLT         
      TER=TEK+(DPRO*(DHRP-D))         
      TATER=TAN (TER)                 
   45 CONTINUE          
      CALL SCAT         
      T2=(H2-HLR)**2+4.*(6370.+H2)*(6370.+HLR)*(SIN(0.5*DLR*VERTH))**2          
      T2=SQRT(T2)       
      DQS=D-DLT-DLR     
      IF(DQS.LT.0.) DQS=0.            
      DQ=T1+T2+DQS      
      ALFS=AFP+20.*LOG10(DQ)         
      DQD(KS)=ALFS      
      SCT(KS)=ALSC      
      AAD(KS)=AA        
      RW(KS)=REW        
      IF(JS.GT.0)T2T=TER*DEG          
      CALL GANE(TER,IAA,H2PBW,T2T,GAVD,GAHD,GAV,IPL)              
      GVD=GFV*GAV       
      IF (IPL .GE. 3) GVD=0.5*(GFVD*GAVD+GFHD*GAHD)               
      GDD=20.*LOG10(GVD)             
      GS(KS)=GDD        
      IF(SCT(KS).LT.20.) GO TO 31     
      KR=KR+1           
      IF(KR.LE.1) GO TO 31            
      KP=KS-1           
      SSP= (SCT(KS)-SCT(KP))/(AND(KS)-AND(KP))      
      IF(SSP.LE.(-.01)) GO TO 49      
      IF(SSP.LE.SLP) GO TO 48         
   31 DNM=DNM+1.        
      GO TO 30          
  910 CONTINUE          
*pdb  WRITE(IOT,22)     
      KFD=1             
      GO TO 33          
   49 KR=0              
      GO TO 31          
   33 KG=0              
   43 KG=KG+1           
      IF(KG.GT.KP) GO TO 911          
      D=AND(KG)         
      DNM=D*CKN(IK)     
      SPD=DNM           
      ATTS=ACD(KG)      
      AA=AAD(KG)        
      REW=RW(KG)        
      THETA=TET+TER+(D/EFRTH)         
      ALFS=DQD(KG)      
      GDD=GS(KG)        
      ASSIGN 36 TO KT                 
      GO TO 200         
   36 CONTINUE          
      GO TO 43          
  911 CONTINUE          
      SPD=DNM           
      MZS=6             
      KFD=1             
      GO TO 37          
   48 IF(SCT(KP).GE.ACD(KP)) GO TO 33               
      ACD(KP)=SCT(KP)                 
      SLP=(ACD(KP)-ARD)/(AND(KP)-DML)               
      AED=ACD(KP)-(AND(KP)*SLP)       
      ASSIGN 35 TO KT                 
      KG=0              
   34 KG=KG+1           
      IF(KG.GT.KP) GO TO 912          
      D=AND(KG)         
      DNM=D*CKN(IK)     
      SPD=DNM           
      ATD=AED+(SLP*D)                 
      ATTS=ATD          
      BMOD=CMOD         
      ALFS=DQD(KG)      
      AA=AAD(KG)        
      REW=RW(KG)        
      THETA=TET+TER+(D/EFRTH)         
      GDD=GS(KG)        
      GO TO 200         
   35 CONTINUE          
      GO TO 34          
  912 CONTINUE          
      SPD=DNM           
      MZS=6             
      KFD=2             
      GO TO 37          
   41 CONTINUE          
      BMOD=EMOD         
      ASSIGN 37 TO KT                 
      ATD=AED+(SLP*D)                 
      IF(D.GT.DHRP) GO TO 24          
      HLR=HLT           
      DLR=D-DLT         
      TER=TEK+(DPRO*(DHRP-D))         
      TATER=TAN (TER)                 
   25 CONTINUE          
      CALL SCAT         
      T2=(H2-HLR)**2+4.*(6370.+H2)*(6370.+HLR)*(SIN(0.5*DLR*VERTH))**2          
      T2=SQRT(T2)       
      DQS=D-DLT-DLR     
      IF(DQS.LT.0.) DQS=0.            
      DQ=T1+T2+DQS      
      ALFS=AFP+20.*LOG10(DQ)         
      IF(JS.GT.0)T2T=TER*DEG          
      CALL GANE(TER,IAA,H2PBW,T2T,GAVD,GAHD,GAV,IPL)              
      GVD=GFV*GAV       
      IF (IPL .GE. 3) GVD=0.5*(GFVD*GAVD+GFHD*GAHD)               
      GDD=20.*LOG10(GVD)             
      ATS=ALSC          
      IF(ATS.LE.ATD) GO TO 46         
      ATTS=ATD          
      THETA=TET+TER+(D/EFRTH)         
      GO TO 200         
   46 ATTS=ATS          
      KFD=2             
      BMOD=SMOD         
      GO TO 200         
   42 CONTINUE          
      BMOD=SMOD         
      CALL SCAT         
      T2=(H2-HLR)**2+4.*(6370.+H2)*(6370.+HLR)*(SIN(0.5*DLR*VERTH))**2          
      T2=SQRT(T2)       
      DQS=D-DLT-DLR     
      IF(DQS.LT.0.) DQS=0.            
      DQ=T1+T2+DQS      
      ALFS=AFP+20.*LOG10(DQ)         
      ATS=ALSC          
      ATTS=ATS          
      ASSIGN 37 TO KT                 
      IF(JS.GT.0)T2T=TER*DEG          
      CALL GANE(TER,IAA,H2PBW,T2T,GAVD,GAHD,GAV,IPL)              
      GVD=GFV*GAV       
      IF (IPL .GE. 3) GVD=0.5*(GFVD*GAVD+GFHD*GAHD)               
      GDD=20.*LOG10(GVD)             
  200 CONTINUE          
C     ----------------LONG-TERM POWER FADING--------------------- 
      IF(D-DSL1)311,311,312           
  311 DEE=(130.*D)/DSL1               
      GO TO 313         
  312 DEE=130.+D-DSL1                 
      GO TO 313         
  313 CALL VARDT(DEE)                 
      NCT=NCT+1         
      PFS=PIRP-ALFS     
      PL=-ATTS          
      ALIM=3.           
      AL10=PL+AD(13)    
      AY=AL10-ALIM      
      IF(AY.LT.0.) AY=0.              
      DO 11 K=1,35      
      BD(K)=PL+AD(K)-AY               
   11 CONTINUE          
      DO 12 K=1,12      
      ALLM=-ALM(K)      
      IF(BD(K).GT.ALLM) BD(K)=ALLM    
   12 CONTINUE
C     -------------VALUES PUT INTO PLOTTING ARRAY---------------- 
      THD=(D/6370.)*DEG               
      THO=THD           
      IF(JC.GT.0) GO TO 101           
      DO 104 NN=1,8     
  104 BX(NCT,NN)=DNM    
  102 CONTINUE          
      IF(KK.GT.1) GO TO 20            
      IF(JO.GT.0) GO TO 92            
      IF(IZ.GT.0) GO TO 93            
   23 PGS=PFS+GDD       
      BY(NCT,1)=PGS     
      BY(NCT,2)=PGS+BD(18)-AA         
      BY(NCT,3)=PGS+BD(12)-AA         
      BY(NCT,4)=PGS+BD(24)-AA         
      BY(NCT,5)=PGS+BD(23)-AA         
      BY(NCT,6)=PGS+BD(26)-AA         
      BY(NCT,7)=PGS+BD(29)-AA         
      BY(NCT,8)=PGS+BD(32)-AA         
      PFY(NCT,1)=PGS+BD(4)-AA         
      PFY(NCT,2)=PGS+BD(7)-AA         
      PFY(NCT,3)=PGS+BD(10)-AA        
      PFY(NCT,4)=PGS+BD(13)-AA        
C     -----------------------WRITE STATEMENTS-------------------- 
*pdb
      ATA=BY(NCT,2)
*pdb  WRITE(IOT,760)DNM,(BY(NCT,LZ),LZ=1,8),(PFY(NCT,MW),MW=1,4),THD,           
*pdb XPL,AA,AY,BK,BMOD 
*pdb  CALL PAGE(1)      
      IF(D.LT.RANG) GO TO 1010
      RETURN
*pdb      
 1010 CONTINUE
      IF (JA .GT. 0) GO TO 400        
  404 CONTINUE          
C     ----------------------------------------------------------- 
      IF(JC.GT.0) GO TO 955           
      IF(SPD.GT.DMAX) GO TO 907       
  956 CONTINUE          
      GO TO KT,(35,36,37)             
   37 CONTINUE          
      IF(JC.GT.0) GO TO 954           
      SPD=SPD+YCON(NSP)               
      GO TO 901         
  908 CONTINUE          
      SPD=SPD-YCON(NSP)               
      NPP=NSP+1         
      IF(NPP.GT.5) GO TO 907          
      IF(YCON(NPP).EQ.0.) GO TO 907   
      IF(NPP.EQ.0) GO TO 907          
      IXD=INT (SPD/YCON(NPP))         
      SPD=(YCON(NPP)*FLOAT (IXD))+YCON(NPP)         
      GO TO 900         
  907 CONTINUE          
      GO TO 100         
          
  400 IF (DNM .GE. RQD .AND. JB .GT. 1) GO TO 401   
      DQM=DNM           
      DO 402 I=1,8      
  402 DBY(I)=BY(NCT,I)                
      DO 403 I=1,4      
      JE=I+8            
  403 DBY(JE)=PFY(NCT,I)              
      GO TO 404         
  401 RAT=(RQD-DQM)/(DNM-DQM)         
      DO 405 I=1,8      
  405 SELT(I)=RAT*(BY(NCT,I)-DBY(I))+DBY(I)         
      DO 406 I=1,4      
      JE=I+8            
  406 SELT(JE)=RAT*(PFY(NCT,I)-DBY(JE))+DBY(JE)     
*pdb  WRITE(IOT,760) RQD,SELT         
      GO TO 100         
C     ---LOOPING BACK TO START FOR NEW SET OF PARAMETERS-----------------'      
          
   16 TRM=((HTE+EFRTH)*COS (TET))/(HRE+EFRTH)       
      DML=EFRTH*(ACOS (TRM)-TET)      
      IF(DML.LE.0.) GO TO 100         
      DLR=DML-DLT       
      TATER=((HLR-HR)/DLR)-(DLR/(2.*EFRTH))         
      TER=ATAN (TATER)                
      GO TO 18          
   17 TER=TES           
      DLR=DRP           
      HLR=HRP           
      TATER=TATES       
      GO TO 19          
C     ------------------TROPOSPHERIC MULTIPATH------------------- 
   20 DO 21 I=1,35      
      QA(I)=BD(I)-PL    
      PQA(I)=P(I)       
   21 CONTINUE          
      IF(THETA.GE.TPTH) GO TO 26      
      IF(THETA.LE.0.)  GO TO 27       
      BK=FNA(THETA,TPTH,TLTH,TPK,RDHK)              
   28 CONTINUE          
      CALL YIKK(BK,PQK,QK)            
      CALL CNLUT(QA,QK,PQA,35,+1.D0,0.D0,PQC,QC)        
      IF(JO.GT.0) GO TO 90            
      IF(IZ.LE.0) GO TO 91            
      DO 98 I=1,35      
   98 QA(I)=QC(I)       
      GO TO 94          
          
   24 TER=TES           
      DLR=DRP           
      HLR=HRP           
      TATER=TATES       
      GO TO 25          
   26 BK=TPK            
      GO TO 28          
   27 BK=RDHK           
      GO TO 28          
   44 TER=TES           
      DLR=DRP           
      HLR=HRP           
      TATER=TATES       
      GO TO 45          
          
   53 HRE=(DLSR*DLSR)/(2.*EFRTH)      
      GO TO 54          
   38 RY2= (SQRT(2.*HRE/EFRTH))       
      DLSR=SQRT(2.*HRE*EFRTH)         
      GO TO 47          
   56 HRE=HP2           
      DLSR=SQRT (2.*EFRTH*HP2)        
      GO TO 57          
   58 RY1= (SQRT(2.*HTE/EFRTH))       
      DLST=SQRT(2.*HTE*EFRTH)         
      GO TO 59          
   60 HTE=(DLST*DLST)/(2.*EFRTH)      
      GO TO 61          
   63 CALL SORB(EC3,EC1,EFRTH,DLT,TO1,RQ)           
      GO TO 64          
   65 CALL SORB(EC3,EC2,EFRTH,DLR,TO2,RQ)           
      GO TO 66          
   74 IF(TET.GT.0..AND.HT.GT.HLT) GO TO 755         
      IF(TET.LE.0..AND.HLT.GT.HT) GO TO 755         
      GO TO 72          
   75 HTE=HP1           
      DLST=SQRT (2.*EFRTH*HP1)        
      GO TO 73          
   80 H2=HAI*CMK(IK)    
*pdb  WRITE(IOT,723) HAI,UND(IK)      
      GO TO 81          
 82   CONTINUE
*pdb  WRITE(IOT,727)HAI,UND(IK),H2    
*pdb  WRITE(IOT,725)DHEI,UND(IK),EAC,PDH            
*pdb  WRITE(IOT,771)H2,EAC,HRP,HRE    
      GO TO 83          
 84   CONTINUE
*pdb  WRITE(IOT,797)    
      GO TO 85          
   92 DO 95 I=1,35      
      QC(I)=BD(I)-PL    
   95 PQA(I)=P(I)       
   90 CALL SCNTL        
      CALL CNLUT(QC,QS,PQA,35,+1.D0,0.D0,PQC,QA)        
      IF(IZ.GT.0) GO TO 94            
      DO 99 I=1,35      
   99 BD(I)=QA(I)+PL    
      GO TO 23          
   93 DO 96 I=1,35      
      QA(I)=BD(I)-PL    
   96 PQA(I)=P(I)       
   94 IF(IZ.GT.6) GO TO 199           
      DO 87 I=1,35      
   87 QR(I)=-RS(I)      
      CALL CNLUT(QA,QR,PQA,35,+1.D0,0.D0,PQC,QC)        
   91 DO 97 I=1,35      
   97 BD(I)=QC(I)+PL    
      GO TO 23          
  101 DO 103 NN=1,8     
  103 BX(NCT,NN)=THD    
      GO TO 102         
****  850402            
* 142 ENCODE(3,141,ADG)IDG            
  142 WRITE(CBUF(1:3),141) IDG        
      READ(CBUF(1:3),'(A3)') ADG      
      GO TO 753         
  143 IF(TET.GE.0.) GO TO 138         
****  850402            
*     ENCODE(3,141,ADG)IDG            
      WRITE(CBUF(1:3),141) IDG        
      READ(CBUF(1:3),'(A3)') ADG      
      GO TO 144         
  173 ISK=ISS+1         
      RMS=SSH(ISK)/CFM(IK)            
       IF(JM.GT.0) RMS=SCK            
      IF(JM.GT.0) SCK=SCK*CFM(IK)     
**** 850606             
*     WRITE (1,172)ISS,SSS(ISK),RMS,UN,TP           
*pdb  WRITE (IOT,172)ISS,SSS(ISK),RMS,UN,TP         
*pdb  IF(KSC.EQ.1) WRITE(IOT,175)     
      GO TO 177         
  174 RMS=SCK           
*pdb  WRITE(IOT,176) RMS,UN           
      SCK=SCK*CFM(IK)                 
      GO TO 177         
  199 PL=PL-RS(18)      
      DO 198 I=1,35     
  198 BD(I)=QA(I)+PL    
      GO TO 23          
 762  CONTINUE
*pdb  WRITE(IOT,779)    
      GO TO 763         
 764  CONTINUE
*pdb  WRITE(IOT,786)    
      GO TO 765         
          
C     --------------HORIZON PARAMETER CALCULATIONS--------------- 
  781 HE=AMAX1(HTE,.005)              
      DLT=DLST*EXP (-.07*SQRT (DH/HE))              
      PDS=PAS(2)        
      IF(DLT.LT.(.1*DLST)) DLT=.1*DLST              
      IF(DLT.GT.(3.*DLST)) DLT=3.*DLST              
      DHOI=DLT*CKN(IK)                
      GO TO 759         
  730 TRM=1.3*DH*((DLST/DLT)-1.)      
      TET=(.5/DLST)*(TRM-(4.*HTE))    
      IF(TET.GT.TWDG) TET=TWDG        
      CALL RDEMS(TET,ADG,IMN,SEC)     
      ISEC= INT (SEC)                 
      PTS=PAS(2)        
      TATET=TAN (TET)                 
      GO TO 758         
  752 HLTS=DLT*TET  +(DLT*DLT/(2.*EFRTH))           
      IF(IDG.EQ.0) GO TO 142          
****  850402            
*     ENCODE(3,140,ADG)IDG            
      WRITE(CBUF(1:3),140) IDG        
      READ(CBUF(1:3),'(A3)') ADG      
      GO TO 753         
  782 XTRM=SQRT ((EFRTH*EFRTH*TATET*TATET)+(2.*EFRTH*HLTS))       
      IF(IDG.EQ.0) GO TO 143          
****  850402            
* 138 ENCODE(3,140,ADG)IDG            
  138 WRITE(CBUF(1:3),140) IDG        
      READ(CBUF(1:3),'(A3)') ADG      
  144 CONTINUE          
      YTRM=-EFRTH*TATET               
      DLT=YTRM-XTRM     
      IF(DLT.LE.0.) DLT=YTRM+XTRM     
      PDS=PAS(2)        
      DHOI=DLT*CKN(IK)                
      GO TO 783         
  780 TATET=(HLTS/DLT)-(DLT/(2.*EFRTH))             
      TET=ATAN (TATET)                
      PTS=PAS(2)        
      CALL RDEMS(TET,ADG,IMN,SEC)     
      ISEC= INT (SEC)                 
      GO TO 783         
C     -------------------SMOOTH EARTH PARAMETERS----------------- 
  755 PTS=PAS(2)        
      PDS=PAS(2)        
      HLT=HRP           
      HHOI=HLT*CKM(IK)                
      DH=0.             
      DLT=DLST          
      HLR=HLT           
      TET=-RY1          
      TATET=TAN (TET)                 
      TER=-RY2          
      DLR=DLSR          
      TO1=0.0           
      TO2=0.0           
      DHOI=DLT*CKN(IK)                
      CALL RDEMS(TET,ADG,IMN,SEC)     
      ISEC= INT (SEC)                 
      DML=DLST+DLSR     
      GO TO 70          
          
C     --------------------MISCELLANEOUS STATEMENTS--------------- 
  789 HFC=0.            
****  850402 ALSO SET ICC AND DCI FOR NO COUNTERPOISE CASE        
      ICC=0             
      DCI=0.            
      GO TO 788         
  801 IF (ENO .GT. 500.) GO TO 803    
      ICAR=1            
      ENO=400.          
      GO TO 802         
  803 EART=ENO          
      EFRTH=EART*CMK(IK)              
      ENS=LOG((1.-(6370./EFRTH))/.04665)           
      ENO=ENS/EXP(-0.1057*HRP)        
      GO TO 804         
  805 ICAR=1            
*pdb  WRITE(IOT,717)    
      GO TO 806         
  807 ICAR=1            
*pdb  WRITE(IOT,719)    
      GO TO 808         
 825  CONTINUE
*pdb  WRITE(IOT,800)    
      GO TO 100         
  828 ICAR=1            
      HCI=0.            
      GO TO 829         
  830 ICAR=1            
      SUR=0.            
      GO TO 831         
  955 IF(SDD.GT.DMAX) GO TO 907       
      GO TO 956         
  960 SDD=(SPD*CMK(IK)/6370.)*DEG     
  950 NSP=NSP+1         
      IF(NSP.GT.5) GO TO 907          
      MZS=NDM(NSP)      
      IF(MZS.LE.0) GO TO 907          
      MXS=0             
  951 MXS=MXS+1         
      IF(MXS.GT.MZS) GO TO 952        
      D=(SDD*RAD)*6370.               
      DNM=D*CKN(IK)     
      GO TO 953         
  954 IF(IDD.LE.0) SDD=THD            
      IF(IDD.LE.0) IDD=1              
      SDD=SDD+XDON(NSP)               
      GO TO 951         
  952 SDD=SDD-XDON(NSP)               
      NPP=NSP+1         
      IF(NPP.GT.5) GO TO 907          
      IF(XDON(NPP).EQ.0.) GO TO 907   
      IF(NPP.EQ.0) GO TO 907          
      IXD=INT(SDD/XDON(NPP))          
      SDD=(XDON(NPP)*FLOAT(IXD))+XDON(NPP)          
      GO TO 950         
          
C     -------------------TERMINATION OF PROGRAM------------------ 
 451  CONTINUE
*pdb  WRITE(IOT,4)      
*pdb  WRITE(IOT,2)      
*pdb  STOP              
      RETURN
*pdb
      END               
      BLOCK DATA SOME                 
        implicit real*8(A-H,O-Z)
****  850402 CORRECTIONS INCLUDED     
          
      COMMON/DIFPR/HTD,HRD,DH,AED,SLP,DLST,DLSR,IPL,KSC,HLD,HRP,AWD,SWP         
     X,II               
*     COMMON/DROPS/RS(35),IZ,SSTS,FQX,DX,HA1,HA2,EFA,AN           
      COMMON/DROPS/RS(35),IZ,STS,FQX,DX,HA1,HA2,EFA,AN            
      COMMON/EGAP/IP,LN               
      COMMON/PARAM/HTE,HRE,D,DLT,DLR,ENS,EFRTH,FREK,ALAM,TET,TER,KD,GAO,        
     XGAW               
      COMMON/PLTD/NU(8),BX(200,8),BY(200,8)         
      COMMON/RYTC/QNS,QHC,QHA,QHS,QQD               
*     COMMON/SCTPR/HT,HR,ALSC,TWEND,THRFK,HLT,HLR,THETA,HTP,AA,REW              
      COMMON/SCTBL/HT,HR,ALSC,TWEND,THRFK,HLT,HLR,THETA,HTP,AA,REW              
      COMMON/SEA/ISK,SCK,TP,JM,EPK,SGM              
      COMMON/SELCT/RQD,JA,JB,DQM,DBY(12),SELT(12),JG              
      COMMON/SIGHT/DCW,HCW,DMAX,DML,DOZ,IK,EAC,H2,ICC,HFC,PRH,QSL1,PIRP,        
****  850402            
*    XQG1,QG9,PFY(200,4),KK,ZH,RDHK,ILB,EAL,IFA,IAA,T1T,HLPBW,T2T,H2PBW         
     XQG1,QG9,PFY(200,4),KK,ZH,RDHK,ILB,EAL,IFA,IAA,T1T,HLPBW,T2T,H2PBW,        
     XJT,JS,H1,IJ,JC,NPL,JO,RY2,RY1   
      COMMON/SOI/THO,FS,IOS,IPK,QS(35)              
      COMMON/VARY/KLM,MX1,KLM2,MX2,FKE,AD(35)       
      COMMON/VV/VF(36,17)             
      COMMON/SPLIT/L1,L2,N,X(140),Y(140),D6(140),XS(55),XD(55),XR(55),YS        
     X(55),YD(55),YR(55),L3,ZS(25),ZD(25),ZR(25)    
      COMMON/CURVE/PI,RAD,DEG,TWPI,PITW             
      COMMON/PRINT/KL,IN,IOT          
          
      DATA (VF(I, 1),I=1,36)/-40.0000,  -.2581,  -.2487,  -.2357, 
     ,  -.2255,  -.2148,  -.1998,  -.1878,  -.1750,  -.1568,  -.1417,           
     ,  -.1252,  -.1004,  -.0784,  -.0634,  -.0516,  -.0321,  -.0155,           
     ,  0.0000,   .0156,   .0323,   .0518,   .0639,   .0790,   .1016,           
     ,   .1270,   .1440,   .1596,   .1786,   .1919,   .2045,   .2202,           
     ,   .2314,   .2421,   .2557,   .2656/          
      DATA (VF(I, 2),I=1,36)/-25.0000, -1.3620, -1.3143, -1.2484, 
     , -1.1966, -1.1427, -1.0669, -1.0055,  -.9401,  -.8460,  -.7676,           
     ,  -.6811,  -.5496,  -.4312,  -.3487,  -.2855,  -.1764,  -.0852,           
     ,  0.0000,   .0897,   .1857,   .2953,   .3670,   .4538,   .5868,           
     ,   .7391,   .8421,   .9374,  1.0544,  1.1374,  1.2165,  1.3161,           
     ,  1.3882,  1.4561,  1.5427,  1.6053/          
      DATA (VF(I, 3),I=1,36)/-20.0000, -2.2901, -2.2126, -2.1055, 
     , -2.0214, -1.9343, -1.8111, -1.7110, -1.6037, -1.4486, -1.3184,           
     , -1.1738,  -.9524,  -.7508,  -.6072,  -.5003,  -.3076,  -.1484,           
     ,  0.0000,   .1624,   .3363,   .5309,   .6646,   .8218,  1.0696,           
     ,  1.3572,  1.5544,  1.7389,  1.9678,  2.1320,  2.2900,  2.4911,           
     ,  2.6380,  2.7751,  2.9497,  3.0760/          
      DATA (VF(I, 4),I=1,36)/-18.0000, -2.8028, -2.7074, -2.5755, 
     , -2.4720, -2.3678, -2.2205, -2.1003, -1.9713, -1.7840, -1.6264,           
     , -1.4508, -1.1846,  -.9332,  -.7609,  -.6240,  -.3888,  -.1878,           
     ,  0.0000,   .2023,   .4188,   .6722,   .8373,  1.0453,  1.3660,           
     ,  1.7416,  2.0014,  2.2461,  2.5520,  2.7732,  2.9875,  3.2621,           
     ,  3.4644,  3.6434,  3.8716,  4.0366/          
      DATA (VF(I, 5),I=1,36)/-16.0000, -3.3978, -3.2842, -3.1271, 
     , -3.0038, -2.8808, -2.7061, -2.5634, -2.4096, -2.1856, -1.9963,           
     , -1.7847, -1.4573, -1.1558,  -.9441,  -.7760,  -.4835,  -.2335,           
     ,  0.0000,   .2564,   .5308,   .8519,  1.0647,  1.3326,  1.7506,           
     ,  2.2463,  2.5931,  2.9231,  3.3402,  3.6452,  3.9433,  4.3340,           
     ,  4.6182,  4.8661,  5.1818,  5.4103/          
      DATA (VF(I, 6),I=1,36)/-14.0000, -4.0877, -3.9537, -3.7685, 
     , -3.6232, -3.4794, -3.2747, -3.1069, -2.9256, -2.6605, -2.4355,           
     , -2.1829, -1.7896, -1.4247, -1.1664,  -.9613,  -.5989,  -.2893,           
     ,  0.0000,   .3251,   .6730,  1.0802,  1.3558,  1.7028,  2.2526,           
     ,  2.9156,  3.3872,  3.8422,  4.4271,  4.8619,  5.2933,  5.8622,           
     ,  6.2894,  6.6446,  7.0972,  7.4245/          
      DATA (VF(I, 7),I=1,36)/-12.0000, -4.8738, -4.7177, -4.5020, 
     , -4.3326, -4.1666, -3.9298, -3.7349, -3.5237, -3.2136, -2.9491,           
     , -2.6507, -2.1831, -1.7455, -1.4329, -1.1846,  -.7381,  -.3565,           
     ,  0.0000,   .4123,   .8535,  1.3698,  1.7289,  2.1808,  2.9119,           
     ,  3.8143,  4.4715,  5.1188,  5.9723,  6.6239,  7.2862,  8.1865,           
     ,  8.8923,  9.4335, 10.1228, 10.6214/          
      DATA (VF(I, 8),I=1,36)/-10.0000, -5.7509, -5.5715, -5.3235, 
     , -5.1288, -4.9399, -4.6694, -4.4462, -4.2034, -3.8453, -3.5384,           
     , -3.1902, -2.6408, -2.1218, -1.7471, -1.4495,  -.9032,  -.4363,           
     ,  0.0000,   .5221,  1.0809,  1.7348,  2.2053,  2.7975,  3.7820,           
     ,  5.0372,  5.9833,  6.9452,  8.2658,  9.3196, 10.4384, 12.0469,           
     , 13.1278, 14.0025, 15.1165, 15.9224/          
      DATA (VF(I, 9),I=1,36)/ -8.0000, -6.7058, -6.5025, -6.2214, 
     , -6.0007, -5.7888, -5.4844, -5.2322, -4.9571, -4.5493, -4.1980,           
     , -3.7975, -3.1602, -2.5528, -2.1091, -1.7566, -1.0945,  -.5287,           
     ,  0.0000,   .6587,  1.3638,  2.1887,  2.3535,  3.5861,  4.9287,           
     ,  6.7171,  8.1418,  9.6386, 11.8333, 13.6864, 15.7730, 18.8754,           
     , 21.4724, 23.1043, 25.1829, 26.6866/          
      DATA (VF(I,10),I=1,36)/ -6.0000, -8.2248, -7.8505, -7.3331, 
     , -6.9269, -6.6923, -6.3546, -6.0739, -5.7667, -5.3093, -4.9132,           
     , -4.4591, -3.7313, -3.0307, -2.5127, -2.1011, -1.3092,  -.6324,           
     ,  0.0000,   .8239,  1.7057,  2.7374,  3.5494,  4.5714,  6.4059,           
     ,  8.9732, 11.0972, 13.4194, 16.5515, 19.5474, 22.4091, 26.2921,           
     , 29.2688, 31.4933, 34.3267, 36.3765/          
      DATA (VF(I,11),I=1,36)/ -4.0000, -8.7379, -8.4880, -8.1426, 
     , -7.8714, -7.6158, -7.2466, -6.9388, -6.6008, -6.0955, -5.6559,           
     , -5.1494, -4.3315, -3.5366, -2.9421, -2.4699, -1.5390,  -.7434,           
     ,  0.0000,  1.0115,  2.0942,  3.3610,  4.4009,  5.7101,  8.1216,           
     , 11.5185, 14.2546, 17.1017, 20.9722, 24.0284, 26.8961, 30.3797,           
     , 33.9080, 36.3139, 39.3784, 41.5953/          
      DATA (VF(I,12),I=1,36)/ -2.0000, -9.7222, -9.4513, -9.0768, 
     , -8.7828, -8.5080, -8.1100, -7.7773, -7.4109, -6.8613, -6.3811,           
     , -5.8252, -4.9219, -3.9366, -3.3234, -2.8363, -1.5390,  -.7434,           
     ,  0.0000,  1.1969,  2.0942,  3.9770,  4.6052,  6.7874,  9.6278,           
     , 13.4690, 16.4258, 19.4073, 23.3679, 26.3778, 29.3751, 33.3813,           
     , 36.3666, 38.8076, 41.9169, 44.1663/          
      DATA (VF(I,13),I=1,36)/  0.0000,-10.5951,-10.3056, -9.9054, 
     , -9.5912, -9.2995, -8.8764, -8.5215, -8.1301, -7.5411, -7.0246,           
     , -6.4248, -5.4449, -4.4782, -3.7425, -3.1580, -1.9678,  -.9505,           
     ,  0.0000,  1.3384,  2.7709,  4.4471,  5.8105,  7.5267, 10.5553,           
     , 14.5401, 17.5512, 20.5618, 24.5411, 27.5515, 30.5618, 34.5412,           
     , 37.5516, 39.9995, 43.1180, 45.3741/          
      DATA (VF(I,14),I=1,36)/  2.0000,-12.2990,-11.9117,-11.3764, 
     ,-10.9561, -9.9223, -9.4777, -9.1044, -8.6919, -8.0697, -7.5228,           
     , -6.8861, -5.8423, -4.8088, -4.0196, -3.3926, -2.1139, -1.0211,           
     ,  0.0000,  1.4189,  2.9376,  4.7145,  6.1724,  8.0074, 11.0005,           
     , 15.0271, 18.0527, 21.0706, 25.0544, 28.0664, 31.0774, 35.0594,           
     , 38.0697, 40.5201, 43.6413, 45.8992/          
      DATA (VF(I,15),I=1,36)/  4.0000,-11.7687,-11.4512,-11.0122, 
     ,-10.6676,-10.3504, -9.8887, -9.5007, -9.0714, -8.4231, -7.8525,           
     , -7.1873, -6.0956, -5.0137, -4.1879, -3.5318, -2.2007, -1.0630,           
     ,  0.0000,  1.4563,  3.0149,  4.8385,  6.2706,  8.0732, 11.1876,           
     , 15.2273, 18.2573, 21.2774, 25.2627, 30.2749, 31.2868, 35.2664,           
     , 38.2767, 40.7273, 43.8489, 46.1071/          
      DATA (VF(I,16),I=1,36)/  6.0000,-12.0323,-11.7170,-11.2811, 
     ,-10.9389,-10.6130,-10.1386, -9.7395, -9.2980, -8.6309, -8.1435,           
     , -7.3588, -6.2354, -5.1233, -4.2762, -3.6032, -2.2451, -1.0845,           
     ,  0.0000,  1.8080,  3.7430,  6.0071,  6.9508,  8.1386, 11.2606,           
     , 15.3046, 18.3361, 21.3565, 25.3426, 28.3549, 31.3664, 35.3464,           
     , 38.3567, 40.8076, 43.9293, 46.1877/          
      DATA (VF(I,17),I=1,36)/ 20.0000,-12.4109,-12.0713,-11.6020, 
     ,-11.2336,-10.8939,-10.3997, -9.9845, -9.5253, -8.8326, -8.2238,           
     , -7.5154, -6.3565, -5.2137, -4.3470, -3.6584, -2.2795, -1.1011,           
     ,  0.0000,  1.4815,  3.0672,  4.9224,  6.3652,  8.1814, 11.3076,           
     , 15.3541, 18.3864, 21.4076, 25.3935, 28.4060, 31.4174, 35.3974,           
     , 38.4077, 40.8586, 43.9805, 46.2389/          
          
****  850402            
*     DATA IN/L"INPUT"/               
*     DATA IOT/L"OUTPUT"/             
          
      END               
