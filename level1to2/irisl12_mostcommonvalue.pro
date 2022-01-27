FUNCTION IRISl12_mostcommonvalue, array, nonzero=nonzero
  ; $Id: irisl12_mostcommonvalue.pro,v 1.2 2015/06/22 14:04:55 mawiesma Exp $  ;
  
  ;this function returns the most common value in an array

  temp = array
  if keyword_set(nonzero) then begin
    ind = where(array NE 0, count)
    if count gt 0 then temp = array[ind]
  endif
  temp = temp[sort(temp)]
  a=uniq(temp)
  b=a
  for i=0,N_ELEMENTS(a)-1 do if i eq 0 then b[i]=a[i]+1 else b[i]=a[i]-a[i-1]
  b=max(b, maxb)
  return, temp[a[maxb]]
END
