pro iris_synthetic_fuv,idlparam,it,wvlr=wvlr
if n_elements(wvlr) eq 0 then wvlr=[1330,1410]
a=br_aux()
;
lcii='c9_m3d'
lsi=a->getlines('si',wvlr)
lo=a->getlines('o',wvlr)
lfe=a->getlines('fe',wvlr)
;lc=a->getlines('c',wvlr)
;
for itt=it[0],it[n_elements(it)-1] do begin
  populate_fuv,[lcii,lsi,lo,lfe],idlparam,itt,/rest
endfor
end
