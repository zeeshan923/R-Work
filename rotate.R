rotate = function (H)
{
  H_exrot = matrix(c(0, 1, 0, 0,
                     -1, 0, 0, 0,
                     0, 0, 1, 0,
                     0, 0, 0, 1),
                   nrow = 4,
                   ncol = 4,
                   byROW = TRUE)
  H=H*H_exrot;
  for (rot_run in 1:2)
  {
    H=run(H)
  }
  flag=3 
  return(H)
}