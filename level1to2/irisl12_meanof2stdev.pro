function irisl12_meanof2stdev, stdev1, stdev2, mean1, mean2, n1, n2
  ; $Id: irisl12_meanof2stdev.pro,v 1.1 2013/10/03 00:39:14 mawiesma Exp $  ;

  stsq1 = double(stdev1)^2
  stsq2 = double(stdev2)^2
  nsq1 = ulong64(n1)^2
  nsq2 = ulong64(n2)^2
  ntot = ulong64(n1)+ulong64(n2)
  nmul = ulong64(n1)*ulong64(n2)
  
  sq = (nsq1*stsq1 + nsq2*stsq2 - n2*stsq1 - n2*stsq2 - n1*stsq1 - n1*stsq2 $
    + nmul*stsq1 + nmul*stsq2 + nmul*double(mean1-mean2)^2) $
    / ((ntot-1) * (ntot))
    
  return,sqrt(sq)
end