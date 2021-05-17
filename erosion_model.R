# Erosion Factor


erosion_factor <- function(R,
                           K,
                           L,
                           S,
                           C,
                           P){
  # I don't think these are necessary
  # L = L - 22.1
  # S = S - 0.09
  
  
  # source: http://www.fao.org/3/T1765E/t1765e0p.htm
  
  if(R > 2000 | R < 100){warning("R, the rainfall factor is generally between 100 and 2000")}
  if(K > 0.3 | K < 0.01){warning("K, the erodability factor is generally between 0.01 and 0.3")}
  if(C > 1 | C < 0){stop("C must be between 0 and 1")}
  if(P > 1 | P < 0){stop("P must be between 0 and 1")}
  
  E = R * K * L * S * C * P
  
  return(E)
  
}