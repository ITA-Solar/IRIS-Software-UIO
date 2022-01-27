function iris_erg2dn,flux,band,lambda,old_effective_area=old_effective_area,cal=cal
;
if n_elements(old_effective_area) eq 0 then old_effective_area=0
;
if n_elements(cal) eq 0 then cal=obj_new('iris_cal')
id=cal->getid(old_effective_area=old_effective_area)
;
ib=where(strupcase(strtrim(band,2)) eq id)
if ib eq -1 then begin
  message,'no such band '+strupcase(strtrim(band,2)),/info
  return,flux*0.-1
end

if n_elements(lambda) eq 0 then lambda=(cal->getlambda0(ib))[0]

flux_photons=flux/cal->enph(lambda) ; photons/cm^2/s/sr/Angstrom

sr=(cal->getarcsec2sr()*(cal->getresx(ib))*(cal->getresy()))[0] ; transformation from arcsec^2 to sr

effective_area=cal->geteffective_area(ib,lambda,old_effective_area=old_effective_area)

flux_electrons=flux_photons*sr*effective_area*(cal->getdellambda(ib)*cal->geteperph(ib))[0]
; still need to convert flux_electrons to flux_dn... BDP suggests
; using 8 electrons/DN for now until we get better data.
flux_dn=flux_electrons/8.

return,flux_dn

end
