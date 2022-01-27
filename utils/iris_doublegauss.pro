function iris_doublegauss, xarr, p
 
  model = p[0] + p[1]*exp(-((xarr - p[2])/p[3])^2) + p[4]*exp(-((xarr - p[5])/p[6])^2)

  return, model
end