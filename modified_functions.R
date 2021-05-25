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
