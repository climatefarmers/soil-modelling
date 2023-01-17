# modified functions
# This script includes modified functions from the soilR package

fW.RothC.Modified <- 
  function (P, 
            E, 
            S.Thick = 23, 
            pClay = 23.4, 
            pE = 0.75, 
            bare_profile = c(T,T,T,F,F,F,F,F,F,T,T,T)) 
  {
    # B = ifelse(bare == FALSE, 1, 1.8)
    # Covered: B = 1
    # Bare: B = 1.8
    
    Max.TSMD_bare = -(20 + 1.3 * pClay - 0.01 * (pClay^2)) * (S.Thick/23) * 
      (1/1.8)
    Max.TSMD_covered = -(20 + 1.3 * pClay - 0.01 * (pClay^2)) * (S.Thick/23) * 
      (1/1)
    
    Max.TSMD <- as.data.frame(bare_profile)
    
    Max.TSMD = Max.TSMD %>% 
      mutate(Max.TSMD = ifelse(bare_profile == TRUE, Max.TSMD_bare, Max.TSMD_covered))
    
    M = P - E * pE
    
    TSMD = Max.TSMD %>% select(Max.TSMD)
    TSMD$Acc.TSMD = 0
    
    for (i in 2:length(M)) {
      TSMD[1,"Acc.TSMD"] = ifelse(M[1] > 0, 0, M[1])
      if (TSMD[i - 1,"Acc.TSMD"] + M[i] < 0) {
        TSMD[i,"Acc.TSMD"] = TSMD[i - 1,"Acc.TSMD"] + M[i]
      }
      else (TSMD[i,"Acc.TSMD"] = 0)
      if (TSMD[i,"Acc.TSMD"] <= TSMD[i,"Max.TSMD"]) {
        TSMD[i,"Acc.TSMD"] = TSMD[i,"Max.TSMD"]
      }
    }
    
    TSMD <- TSMD %>% 
      rowwise() %>% 
      mutate(
        b = ifelse(Acc.TSMD > 0.444 * Max.TSMD, 1, (0.2 + 0.8 * ((Max.TSMD - Acc.TSMD)/(Max.TSMD - 0.444 * Max.TSMD))))
      )
    
    return(TSMD)
  }

fW.RothC.Modified_semiArid <- 
  function (P, 
            E, 
            S.Thick = 23, 
            pClay, 
            pSilt, 
            bulkDensity,
            SOC,
            pE = 0.75, 
            bare_profile = c(T,T,T,F,F,F,F,F,F,T,T,T)) 
  { percSOC = SOC/(bulkDensity*S.Thick)
    t = 1
    alpha = exp(-14.96 + 0.03135*pClay + 0.0351*pSilt + 0.646*(percSOC*1.72) 
                + 15.29*bulkDensity - 0.192*t - 4.671*bulkDensity**2 - 0.000781*pClay**2
                - 0.00687*(percSOC*1.72)**2 + 0.0449*(percSOC*1.72)**(-1) + 0.0663*log(pSilt) 
                + 0.1482*log(percSOC*1.72) -0.04546*bulkDensity*pSilt -0.4852*bulkDensity 
                *(percSOC*1.72) + 0.00673*pClay*t)
    
    deltaS = (0.7919 +0.001691*pClay -0.29619*bulkDensity -0.000001491*pSilt**2 +0.0000821 
              *(percSOC*1.72 )**2 +0.02427*pClay**(-1) +0.01113*pSilt**(-1) +0.01472 
              *log(pSilt) -0.0000733*(percSOC*1.72)*pClay -0.000619*bulkDensity*pClay 
              -0.001183*bulkDensity*(percSOC*1.72) -0.0001664*pSilt*t)
    
    deltaR = 0.01
    
    n = exp (-25.23 -0.02195*pClay +0.0074*pSilt -0.194*(percSOC*1.72) +45.5*bulkDensity -7.24*bulkDensity**2 +0.0003658*pClay**2 +0.002885
             *(percSOC*1.72)**2 -12.81*bulkDensity**(-1) -0.1524*pSilt**(-1) -0.01958*(percSOC*1.72)**(-1) -0.2876*log(pSilt) -0.0709
             *log(percSOC*1.72) -44.6*log(bulkDensity) -0.02264*bulkDensity*pClay +0.0896*bulkDensity*(percSOC*1.72) +0.00718*pClay*t) +1
    
    m = 1  -1/n
    mbar = 50
    WC_fc = deltaR +(deltaS -deltaR)/(1 +(alpha*mbar)**n)**m
    mbar = 1e3
    WC_1 = deltaR +(deltaS -deltaR)/(1 +(alpha*mbar)**n)**m
    mbar = 15e3
    WC_15 = deltaR +(deltaS -deltaR)/(1 +(alpha*mbar)**n)**m
    mbar = 1e6
    WC_1000 = deltaR +(deltaS -deltaR)/(1 +(alpha*mbar)**n)**m
    Mb_pedotranfer = (WC_1-WC_fc)*10*S.Thick
    M_pedotranfer = (WC_15-WC_fc)*10*S.Thick
    Mc_pedotranfer = (WC_1000-WC_fc)*10*S.Thick
    
    Max.TSMD = data.frame(Max.TSMD = rep(Mc_pedotranfer,12),  # now not really needed
             Mb = rep(Mb_pedotranfer,12)) 
    
    M = P - E * pE
    
    TSMD = Max.TSMD
    TSMD$Acc.TSMD = 0
    
    for (i in 2:length(M)) {
      TSMD[1,"Acc.TSMD"] = ifelse(M[1] > 0, 0, M[1])
      if (TSMD[i - 1,"Acc.TSMD"] + M[i] < 0) {
        TSMD[i,"Acc.TSMD"] = TSMD[i - 1,"Acc.TSMD"] + M[i]
      }
      else (TSMD[i,"Acc.TSMD"] = 0)
      if (TSMD[i,"Acc.TSMD"] <= TSMD[i,"Max.TSMD"]) {
        TSMD[i,"Acc.TSMD"] = TSMD[i,"Max.TSMD"]
      }
    }
    
    TSMD <- TSMD %>% 
      rowwise() %>% 
      mutate( #CAUTION HERE v original script was Acc.TSMD >!!!! 0.444 * Max.TSMD which seems very wrong
        b = ifelse(Acc.TSMD > Mb, 1, ifelse(Acc.TSMD < M_pedotranfer, 0.1, (0.1 + 0.8 * ((M_pedotranfer - Acc.TSMD)/(M_pedotranfer - Mb))))) # Mb replace 0.444 * Max.TSMD
      )
    
    return(TSMD)
  }
