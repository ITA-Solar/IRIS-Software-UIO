function iris_gen_rb_profile, v, profile, steps, dv

red = dblarr(n_elements(steps))
blue = dblarr(n_elements(steps))
for kk=0,n_elements(steps)-1 do begin

   lo = steps[kk]
   hi = lo + dv

   BEST_BLUE = WHERE((v GE -hi) AND (v LE -lo), BBC)
   BEST_RED = WHERE((v GE lo) AND (v LE hi), BRC)

   if (bbc gt 0) AND (BRC GT 0) then BEGIN
       blue[kk] = total(profile(best_blue)) / float(bbc)
       red[kk] = total(profile(best_red)) / float(brc)	
   endif

endfor ;; kk

str = {red:red, blue:blue, steps:steps}

return, str
end