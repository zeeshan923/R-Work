run = function(H)
{
  
  # % H = ceil(H);
  # disp('Run');
  
  if (H[2,1] == 0.7071068 || H[2,1] == -0.7071068)
  {
    H1 = matrix(
      c(1, 0, 0, 14142.13562,
        0, 1, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1),
      nrow=4, 
      ncol=4,
      byrow = TRUE)
    H2 = H%*%H1;
    return(H2)
    
  }else 
  {
    H1 = matrix(
      c(1, 0, 0, 10000,
        0, 1, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1),
      nrow=4, 
      ncol=4,
      byrow = TRUE)
    H2 = H%*%H1;
    return(H2)
    
  }
  
}
