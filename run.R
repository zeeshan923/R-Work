run = function(H)
{
  
  # % H = ceil(H);
  # disp('Run');
  H21 = round(H[2,1],digits=3)
  if (H21 == 0.707 || H21 == -0.707)
  {
    H1 = matrix(
      c(1, 0, 0, 14142.13562,
        0, 1, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1),
      nrow=4, 
      ncol=4,
      byrow = TRUE)
    H2 = H%*%H1
    return(H2)
    
  }else #if(H[2,1] == 0 || H[2,1] == 1)
  {
    H1 = matrix(
      c(1, 0, 0, 10000,
        0, 1, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1),
      nrow=4, 
      ncol=4,
      byrow = TRUE)
    H2 = H%*%H1
    return(H2)
    
    
  }
  # else
  # {
  #   var = "run else statement"
  #   return(var)
  # }
  
}
