pro red_ct,blue=blue,green=green

loadct,0
tvlct,r,g,b,/get
g(255)=0&b(255)=0
if keyword_set(blue) and not(keyword_set(green)) then begin
 g(254)=0
 r(254)=0
endif
if keyword_set(green) and not(keyword_set(blue)) then begin
 b(254)=0
 r(254)=0
endif
if keyword_set(blue) and (keyword_set(green)) then begin
 g(254)=0
 r(254)=0
 b(253)=0
 r(253)=0
endif


tvlct,r,g,b


return
end
