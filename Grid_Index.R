Grid_Index =  function(dat, x_value, y_value)
{
  for ( k in 1:length(dat[,1]))
  {
    if(dat[k,1] == x_value && dat[k,2] == y_value)
    {
      
      return(k)
      break
    }
    
  }
}
