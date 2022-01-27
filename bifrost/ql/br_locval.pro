function br_locval,var,pos,fix=fix

xx=findgen(n_elements(var))
out=interpol(xx,var,pos)
if n_elements(fix) then out=round(out)

return,out

end 
