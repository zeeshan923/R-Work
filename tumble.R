tumble = function(H)
{
  # disp('Tumble');
  # % prev_angle = atand(H(2,1)/H(1,1));
  
  # % if prev_angle == 45
  beta = c(pi/2, -pi/2)
  
  i = sample(1:2, 1)
  theta =  beta[i]
  # % theta = ceil(theta)
  H_temp = matrix(c(cos(theta), -sin(theta), 0, 0,
                    sin(theta), cos(theta), 0, 0,
                    0, 0, 1, 0,
                    0, 0, 0, 1),
                  nrow=4, 
                  ncol=4,
                  byrow = TRUE)
  H_temp = round(H_temp)
  A = H%*%H_temp
  A21 = round(A[2,1], digits=3)
  if(A21 == 0.707 || A21 == -0.707)
  {
    H1 = matrix(c(1, 0, 0, 14142.13562,
                  0, 1, 0, 0,
                  0, 0, 1, 0,
                  0, 0, 0, 1),
                nrow=4, 
                ncol=4,
                byrow = TRUE)
    return(Homog = A%*%H1)
  } else
  {
    H1 = matrix(c(1, 0, 0, 10000,
                  0, 1, 0, 0,
                  0, 0, 1, 0,
                  0, 0, 0, 1),
                nrow=4, 
                ncol=4,
                byrow = TRUE)
    return(Homog = A%*%H1)  
  }
  
  
  
}
