pro br_iion,cion,d,isnap,tau=tau,ix=ix,iy=iy,iz=iz,ion=ion,mode=mode,absorb=absorb,tgi=tgi,nofits=nofits,tgm=tgm, $
             extraionlines=extraionlines,highres=highres,img=img,rhoi=rhoi,eni=eni,losi=losi,rhom=rhom,enm=enm,table=table, $
            cont=cont,line=line,xm=xm,ym=ym,zm=zm,phcut=phcut
; $Id: br_iion.pro,v 1.10 2016/02/10 08:47:49 juanms Exp $
;
if n_params() ne 3 then begin
  message,'br_iion,cion,d,isnap,ix=ix,iy=iy,iz=iz',/info
  return
endif

a=d->getaux()
if (where(a->getallowed_lines() eq cion))[0] eq -1 then begin
  message,'no such line '+cion+' found',/info
  message,'creating a new chianti line data within 1 \AA range',/info
  br_chianti_build_goftne_nsens,cion+'.sav',path=a->getdata_dir()+'lines/'
  a->setallowed_lines
  if (where(a->getallowed_lines() eq cion))[0] ne -1 then message,'success to create a new chianti line data file '+cion,/info
endif

if n_elements(absorb) eq 0 then absorb=0
if n_elements(line) eq 0 then line=-1
if n_elements(highres) eq 0 then highres=0
if n_elements(mode) eq 0 then mode='xy'
if n_elements(nofits) eq 0 then nofits=d->getnofits()
notr=n_elements(rhoi)
if notr eq 0 then rho=d->getvar('r',isnap,ix=ix,iy=iy,iz=iz,swap=(d->getaux())->getswap(),nofits=nofits) else rho=rhoi
nottg=n_elements(tgi)
if nottg eq 0 then tg=d->getvar('tg',isnap,ix=ix,iy=iy,iz=iz,swap=(d->getaux())->getswap(),nofits=nofits) else tg=tgi
d->readpars,isnap[0],nofits=nofits
ion=a->getion(cion)
notne=n_elements(eni)
units=d->getunits()
ur_si2cgs = 1.e-3
ue_si2cgs = 1.e+4
une_si2cgs= 1.e-6
nottg=n_elements(tg)
if notne eq 0 then begin
   if n_elements(table) then begin
      ee=d->getvar('ee',isnap,ix=ix,iy=iy,iz=iz,swap=(d->getaux())->getswap(),nofits=nofits)
      br_load_rhoeetab
      if nofits eq 1 then begin
        br_rhoeetab,rho*units.ur,ee*units.uee,nel=en
      endif else begin
        br_rhoeetab,rho*ur_si2cgs,ee*ue_si2cgs,nel=en
      endelse
   endif else begin
      if nofits eq 1 then begin
        ne_compute,tg,rho,en
      endif else begin
        en=(d->getvar('ne',isnap,ix=ix,iy=iy,iz=iz,swap=(d->getaux())->getswap()))*une_si2cgs
      endelse
   endelse
endif else begin
en=eni
endelse

x=d->getx()
y=d->gety()
z=d->getz()

tg=reform(tg) 
goft=tg*0
sz=size(tg)
img=fltarr(sz[1],sz[2])
abund = (a->abund(cion))
if abund eq -1 then abund=1.
if n_elements(extraionlines) ne -1 and n_elements(ion.lines[*]) gt 1 then goftn=total(ion.lines[*].goft,2) else goftn=ion.lines[0].goft
if where(tag_names(ion) eq 'PRESSURE') ne -1 then onlytg = 0
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;; THIS IS USED FOR BLENDED LINES, RIGHT NOW ONLY USES G(T) INSTEAD
;;;; OF G(T,NE)
if line ge 0 and line lt n_elements(ion.lines[*]) then begin
   goftne=ion.lines[line].goftne   ; this one is for the line=line then it is blended.
;   onlytg = 1
endif else begin
  if onlytg eq 0 then goftne=ion.goftne   ;; This one adds goft together for all lines saved in the *.dat file
endelse
; ------------------------------------------------------------

mintg=min(ion.ioneq_logt(where(goftn ge max(goftn)/1.e12)))
maxtg=max(ion.ioneq_logt(where(goftn ge max(goftn)/1.e12)))

if n_elements(phcut) eq 0 then if nofits eq 1 then phcut=- 0.1 else phcut=0.1
;phcut=- 0.5
;if nofits eq 1 then ztemp=z else ztemp=-reverse(z)
ztemp=z
zm=z
ym=y
xm=x
if highres le 1 then begin
   tau=0.*rho
   if nofits eq 1 then begin
       case sz[0] of
          1: loc=where(alog10(tg(where(ztemp le phcut))) ge mintg and alog10(tg(where(ztemp le phcut))) le maxtg)
          2: loc=where(alog10(tg(*,where(ztemp le phcut))) ge mintg and alog10(tg(*,where(ztemp le phcut))) le maxtg)
          3: begin
             if n_elements(isnap) ge 2 then begin
                loc=where(alog10(tg(*,where(ztemp le phcut),*)) ge mintg and alog10(tg(*,where(ztemp le phcut),*)) le maxtg)
             endif else begin
                loc=where(alog10(tg(*,*,where(ztemp le phcut))) ge mintg and alog10(tg(*,*,where(ztemp le phcut))) le maxtg)
             endelse
          end
       endcase
   endif else begin
      loc=-1
   endelse
;   loc=-1

   if loc[0] ne -1 or nofits eq 1 then begin
      if onlytg eq 0 then begin
         ip=interpol(indgen(n_elements(ion.pressure)),ion.pressure,tg(loc)*en(loc))
         it=interpol(indgen(n_elements(ion.logtgr)),ion.logtgr,alog10(tg(loc)))
         goftreg=interpolate(goftne,it,ip)* abund
      endif else begin
         goftreg=interpol(goftn,ion.ioneq_logt,alog10(tg(loc)))*abund
      endelse
      goft(loc)=goftreg
   endif else begin
      goft=interpol(goftn,ion.ioneq_logt,alog10(tg))*abund
   endelse
   tgm=tg
   rhom=rho
   enm=en
   zm=z
   ym=y
	 xm=x
   if nofits eq 1 then begin
      if absorb eq 1 and ion.lines[0].wvl le 912. then begin
         h_he_absorb,ion.lines[0].wvl,x,y,zm,rhom,tgm,tau,mode=mode
      endif else begin
         if n_elements(isnap) ge 2 then begin
            tau(*,where(ztemp ge phcut),*)=rhom(*,where(ztemp ge phcut),*)*1e7
         endif else begin

            if sz[0] eq 2 then begin
               tau(*,where(ztemp ge phcut))=rhom(*,where(ztemp ge phcut))*1e7
            endif else begin
               tau(*,*,where(ztemp ge phcut))=rhom(*,*,where(ztemp ge phcut))*1e7
            endelse
         endelse
      endelse
   endif else begin
      if absorb eq 1 and ion.lines[0].wvl le 912. then begin
         h_he_absorb,ion.lines[0].wvl,x,y,zm,rhom,tgm,tau,mode=mode
      endif else begin
         if n_elements(isnap) ge 2 then begin
            tau(*,where(ztemp le phcut),*)=rhom(*,where(ztemp le phcut),*)*1e7
         endif else begin
           if sz[0] eq 2 then begin
            tau(*,where(ztemp le phcut))=rhom(*,where(ztemp le phcut))*1e7
          endif else begin
            tau(*,*,where(ztemp le phcut))=rhom(*,*,where(ztemp le phcut))*1e7
          endelse
         endelse
      endelse
   endelse
endif else begin
   if highres ge 2 then begin
      case sz[0] of
         1: begin
            if nofits eq 1 then izp=where(ztemp le phcut) else izp=where(ztemp ge phcut)
            zm=interpol(z(izp),indgen(n_elements(z(izp))),indgen(n_elements(z(izp))*highres)/(highres*1.0))
            if n_elements(losi) ne n_elements(zm) then begin
               tgm=interpol(tg(izp),z(izp),zm)
               rhom=interpol(rho(izp),z(izp),zm)
               enm=interpol(en(izp),z(izp),zm)
            endif else begin
               tgm=tg
               rhom=rho
               enm=en
            endelse
            ip=interpol(indgen(n_elements(ion.pressure)),ion.pressure,tgm*enm)
            it=interpol(indgen(n_elements(ion.logtgr)),ion.logtgr,alog10(tgm))
            goft=interpolate(goftne,it,ip)* abund
            tau=0.*goft
;            if nofits eq 1 then zmtemp=zm else zmtemp=-reverse(zm)
            zmtemp=zm
            if nofits eq 1 then begin
               if absorb eq 1 and ion.lines[0].wvl le 912. then h_he_absorb,ion.lines[0].wvl,x,y,zm,rhom,tgm,tau,mode=mode else $
                  tau(where(zmtemp ge phcut))=rhom(where(zmtemp ge phcut))*1.e7
            endif else begin
               if absorb eq 1 and ion.lines[0].wvl le 912. then h_he_absorb,ion.lines[0].wvl,x,y,zm,rhom,tgm,tau,mode=mode else $
                  tau(where(zmtemp le phcut))=rhom(where(zmtemp le phcut))*1.e7
            endelse
         end
         2: begin
            if nofits eq 1 then izp=where(ztemp le phcut) else izp=where(ztemp ge phcut)
            zm=interpol(z(izp),indgen(n_elements(z(izp))),indgen(n_elements(z(izp))*highres)/(highres*1.0))
            ;if nofits eq 1 then zmtemp=zm else zmtemp=-reverse(zm)
            zmtemp=zm
            if n_elements(losi) ne n_elements(zm) then begin
               tgm=fltarr(sz[1],n_elements(zm))
               rhom=fltarr(sz[1],n_elements(zm))
               enm=fltarr(sz[1],n_elements(zm))
            endif else begin
               tgm=tg
               rhom=rho
               enm=en
            endelse
            for ix=0,sz[1]-1 do begin
               tgm(ix,*)=interpol(tg(ix,izp),z(izp),zm)
               rhom(ix,*)=interpol(rho(ix,izp),z(izp),zm)
               enm(ix,*)=interpol(en(ix,izp),z(izp),zm)
            endfor
            ip=interpol(indgen(n_elements(ion.pressure)),ion.pressure,tgm*enm)
            it=interpol(indgen(n_elements(ion.logtgr)),ion.logtgr,alog10(tgm))
            goft=interpolate(goftne,it,ip)* abund
            tau=0.*goft
            if nofits eq 1 then begin
               if absorb eq 1 and ion.lines[0].wvl le 912. then h_he_absorb,ion.lines[0].wvl,x,y,zm,rhom,tgm,tau,mode=mode else $
                  tau(*,where(zmtemp ge phcut))=rhom(*,where(zmtemp ge phcut))*1.e7
            endif else begin
               if absorb eq 1 and ion.lines[0].wvl le 912. then h_he_absorb,ion.lines[0].wvl,x,y,zm,rhom,tgm,tau,mode=mode else $
                  tau(*,where(zmtemp le phcut))=rhom(*,where(zmtemp le phcut))*1.e7
            endelse
         end
         3: begin

            if nofits eq 1 then izp=where(z le phcut) else izp=where(z ge phcut)
            case mode of
               'xy': begin
                  zm=interpol(z(izp),indgen(n_elements(z(izp))),indgen(n_elements(z(izp))*highres)/(highres*1.0))
                                ;            if nofits eq 1 then zmtemp=zm else zmtemp=-reverse(zm)
                  zmtemp=zm
                  if n_elements(losi) ne n_elements(zm) then begin
                     tgm=fltarr(sz[1],sz[2],n_elements(zm))
                     rhom=fltarr(sz[1],sz[2],n_elements(zm))
                     enm=fltarr(sz[1],sz[2],n_elements(zm))
                     for ix=0,sz[1]-1 do begin
                        for iy=0,sz[2]-1 do begin
                           tgm(ix,iy,*)=interpol(tg(ix,iy,izp),z(izp),zm)
                           rhom(ix,iy,*)=interpol(rho(ix,iy,izp),z(izp),zm)
                           enm(ix,iy,*)=interpol(en(ix,iy,izp),z(izp),zm)
                        endfor
                     endfor
                  endif else begin
                     tgm=tg
                     rhom=rho
                     enm=en
                  endelse
               end
               'xz': begin
                  zm=z(izp)
                  ym=interpol(y,indgen(n_elements(y)),indgen(n_elements(y)*highres)/(highres*1.0))
                                ;            if nofits eq 1 then zmtemp=zm else zmtemp=-reverse(zm)
                  zmtemp=zm
                  if n_elements(losi) ne n_elements(ym) then begin
                     tgm=fltarr(sz[1],n_elements(ym),n_elements(zm))
                     rhom=fltarr(sz[1],n_elements(ym),n_elements(zm))
                     enm=fltarr(sz[1],n_elements(ym),n_elements(zm))
                     for ix=0,sz[1]-1 do begin
                        for iz=0,n_elements(zm)-1 do begin
                           tgm(ix,*,iz)=interpol(tg(ix,*,izp(iz)),y,ym)
                           rhom(ix,*,iz)=interpol(rho(ix,*,izp(iz)),y,ym)
                           enm(ix,*,iz)=interpol(en(ix,*,izp(iz)),y,ym)
                        endfor
                     endfor
                 endif else begin
                     tgm=tg
                     rhom=rho
                     enm=en
                  endelse
               end
               'yz': begin
                  zm=z(izp)
                  xm=interpol(x,indgen(n_elements(x)),indgen(n_elements(x)*highres)/(highres*1.0))
                                ;            if nofits eq 1 then zmtemp=zm else zmtemp=-reverse(zm)
                  zmtemp=zm
                  if n_elements(losi) ne n_elements(ym) then begin
                     tgm=fltarr(n_elements(xm),sz[2],n_elements(zm))
                     rhom=fltarr(n_elements(xm),sz[2],n_elements(zm))
                     enm=fltarr(n_elements(xm),sz[2],n_elements(zm))
                     for iy=0,sz[2]-1 do begin
                        for iz=0,n_elements(zm)-1 do begin
                           tgm(*,iy,iz)=interpol(tg(*,iy,izp(iz)),x,xm)
                           rhom(*,iy,iz)=interpol(rho(*,iy,izp(iz)),x,xm)
                           enm(*,iy,iz)=interpol(en(*,iy,izp(iz)),x,xm)
                        endfor
                     endfor
                  endif else begin
                     tgm=tg
                     rhom=rho
                     enm=en
                  endelse
               end
            endcase
            ip=interpol(indgen(n_elements(ion.pressure)),ion.pressure,tgm*enm)
            it=interpol(indgen(n_elements(ion.logtgr)),ion.logtgr,alog10(tgm))
            goft=interpolate(goftne,it,ip)* abund
            tau=0.*goft
            if nofits eq 1 then begin
               if absorb eq 1 and ion.lines[0].wvl le 912. then h_he_absorb,ion.lines[0].wvl,xm,ym,zm,rhom,tgm,tau,mode=mode else $
                  tau(*,*,where(zmtemp ge phcut))=rhom(*,*,where(zmtemp ge phcut))*1.e7
            endif else begin
               if absorb eq 1 and ion.lines[0].wvl le 912. then h_he_absorb,ion.lines[0].wvl,xm,ym,zm,rhom,tgm,tau,mode=mode else $
                  tau(*,*,where(zmtemp le phcut))=rhom(*,*,where(zmtemp le phcut))*1.e7
            endelse
         end
      endcase
   endif
endelse

case sz[0] of
1: begin
   sz[1]=n_elements(zm)
   goft=reform(goft,sz[1])
end
2: begin
   sz[1]=n_elements(x)
   sz[2]=n_elements(zm)
   goft=reform(goft,sz[1],sz[2])
end
3: begin
   if n_elements(x) gt 1 and n_elements(y) gt 1 and n_elements(z) gt 1 then begin
      sz[1]=n_elements(xm)
      sz[2]=n_elements(ym)
      sz[3]=n_elements(zm)
   endif else begin
      if n_elements(x) eq 1 then begin
         sz[1]=n_elements(ym)
         sz[2]=n_elements(zm)
         sz[3]=n_elements(isnap)
      endif else begin
         if n_elements(y) eq 1 then begin
            sz[1]=n_elements(xm)
            sz[2]=n_elements(zm)
            sz[3]=n_elements(isnap)
         endif else begin
            sz[1]=n_elements(xm)
            sz[2]=n_elements(ym)
            sz[3]=n_elements(isnap)
         endelse
      endelse
   endelse
   goft=reform(goft,sz[1],sz[2],sz[3])
end
else:
endcase

if n_elements(highres) ne 0 and ion.lines[0].tmax lt 5.5 then begin
if highres eq 1  then begin
   case sz[0] of
      3: begin
         if n_elements(snap) eq 1 then begin
         for iix=0,sz[1]-1 do begin
            for iiy=0,sz[2]-1 do begin
               pos=interpol(z,alog10(tg(iix,iiy,*)),ion.lines[0].tmax)
               aa=where(alog10(tg(iix,iiy,*)) le ion.lines[0].tmax)
               pos2=where(aa -shift(aa,1) gt 1)
               if pos2[0] gt 1 then begin
                  if aa[0] eq 0 then inipost=1 else inipost=0
                  for ipost=inipost,n_elements(pos2)-1 do begin
; add min(aa)...
                     if nofits eq 1 then begin
                        pos=interpol(z[aa(inipost)-1:aa(inipost)+1], $
                                     alog10(tg(iix,iiy,aa(inipost)-1:aa(inipost)+1)),ion.lines[0].tmax)
                     endif else begin
                        pos=interpol(z[max(aa)-1:max(aa)+1], $
                                     alog10(tg(iix,iiy,max(aa)-1:max(aa)+1)),ion.lines[0].tmax)
                     endelse
                     pos=[pos,interpol(z[aa(pos2(ipost))-1:aa(pos2(ipost))+1], $
                                       alog10(tg(iix,iiy,aa(pos2(ipost))-1:aa(pos2(ipost))+1)),ion.lines[0].tmax)]
                     pos=[pos,interpol(z[aa(pos2(ipost)-1)-1:aa(pos2(ipost)-1)+1], $
                                       alog10(tg(iix,iiy,aa(pos2(ipost)-1)-1:aa(pos2(ipost)-1)+1)),ion.lines[0].tmax)]
                  endfor

               endif
               ipos=fix(round(interpol(indgen(n_elements(z)),z,pos)))
               if where(tag_names(ion) eq 'PRESSURE') ne -1 then begin
                  enr=interpol(en(iix,iiy,*),z,pos)
                  rhor=interpol(rho(iix,iiy,*),z,pos)
                  ipr=interpol(indgen(n_elements(ion.pressure)),ion.pressure,ion.lines[0].tmax*enr)
                  itr=interpol(indgen(n_elements(ion.logtgr)),ion.logtgr,indgen(n_elements(pos))*0.+ion.lines[0].tmax)
                  goftr=interpolate(ion.goftne,itr,ipr)*abund
               endif else begin
                  goftr=interpol(goftn,ion.ioneq_logt,indgen(n_elements(pos))*0.+ion.lines[0].tmax)*abund
                  enr=interpol(en(iix,iiy,*),z,pos)
                  rhor=interpol(rho(iix,iiy,*),z,pos)
               endelse
               for i=0,n_elements(pos)-1 do rho(iix,iiy,ipos(i))=rhor(i)
               for i=0,n_elements(pos)-1 do en(iix,iiy,ipos(i))=enr(i)
               for i=0,n_elements(pos)-1 do goft(iix,iiy,ipos(i))=goftr(i)
            endfor
         endfor
      endif else begin
         for iix=0,sz[1]-1 do begin
            for iit=0,sz[3]-1 do begin
               pos=interpol(z,alog10(tg(iix,*,iit)),ion.lines[0].tmax)
               aa=where(alog10(tg(iix,*,iit)) le ion.lines[0].tmax)
               pos2=where(aa -shift(aa,1) gt 1)
               if pos2[0] gt 1 then begin
                  if aa[0] eq 0 then inipost=1 else inipost=0
                  for ipost=inipost,n_elements(pos2)-1 do begin
; add min(aa)...
                     if nofits eq 1 then begin
                        pos=interpol(z[aa(inipost)-1:aa(inipost)+1], $
                                     alog10(tg(iix,aa(inipost)-1:aa(inipost)+1,iit)),ion.lines[0].tmax)
                     endif else begin
                        pos=interpol(z[max(aa)-1:max(aa)+1], $
                                     alog10(tg(iix,max(aa)-1:max(aa)+1,iit)),ion.lines[0].tmax)
                     endelse
                     pos=[pos,interpol(z[aa(pos2(ipost))-1:aa(pos2(ipost))+1], $
                                       alog10(tg(iix,aa(pos2(ipost))-1:aa(pos2(ipost))+1,iit)),ion.lines[0].tmax)]
                     pos=[pos,interpol(z[aa(pos2(ipost)-1)-1:aa(pos2(ipost)-1)+1], $
                                       alog10(tg(iix,aa(pos2(ipost)-1)-1:aa(pos2(ipost)-1)+1,iit)),ion.lines[0].tmax)]
                  endfor

               endif
               ipos=fix(round(interpol(indgen(n_elements(z)),z,pos)))
               if where(tag_names(ion) eq 'PRESSURE') ne -1 then begin
                  enr=interpol(en(iix,*,iit),z,pos)
                  rhor=interpol(rho(iix,*,iit),z,pos)
                  ipr=interpol(indgen(n_elements(ion.pressure)),ion.pressure,ion.lines[0].tmax*enr)
                  itr=interpol(indgen(n_elements(ion.logtgr)),ion.logtgr,indgen(n_elements(pos))*0.+ion.lines[0].tmax)
                  goftr=interpolate(ion.goftne,itr,ipr)*abund
               endif else begin
                  goftr=interpol(goftn,ion.ioneq_logt,indgen(n_elements(pos))*0.+ion.lines[0].tmax)*abund
                  enr=interpol(en(iix,*,iit),z,pos)
                  rhor=interpol(rho(iix,*,iit),z,pos)
               endelse
               for i=0,n_elements(pos)-1 do rho(iix,ipos(i),iit)=rhor(i)
               for i=0,n_elements(pos)-1 do en(iix,ipos(i),iit)=enr(i)
               for i=0,n_elements(pos)-1 do goft(iix,ipos(i),iit)=goftr(i)
            endfor
         endfor
       endelse
      end
      2: begin
         for iix=0,sz[1]-1 do begin
            pos=interpol(z,alog10(tg(iix,*)),ion.lines[0].tmax)
            aa=where(alog10(tg(iix,*)) le ion.lines[0].tmax)
            pos2=where(aa -shift(aa,1) gt 1)
            if pos2[0] gt 1 then begin
               for ipost=0,n_elements(pos2)-1 do begin
; add min(aa)...
                  if nofits eq 1 then begin
                     pos=interpol(z[max([aa(0)-1,0]):min([aa(0)+1,n_elements(z)-1])], $
                                  alog10(tg(iix,max([aa(0)-1,0]):min([aa(0)+1,n_elements(z)-1]))),ion.lines[0].tmax)
                  endif else begin
                     pos=interpol(z[max([max(aa)-1,0]):min([max(aa)+1,n_elements(z)-1])], $
                                  alog10(tg(iix,max([max(aa)-1,0]):min([max(aa)+1,n_elements(z)-1]))),ion.lines[0].tmax)
                  endelse
                  pos=[pos,interpol(z[aa(pos2(ipost))-1:aa(pos2(ipost))+1], $
                                    alog10(tg(iix,aa(pos2(ipost))-1:aa(pos2(ipost))+1)),ion.lines[0].tmax)]
                  pos=[pos,interpol(z[aa(pos2(ipost)-1)-1:aa(pos2(ipost)-1)+1], $
                                    alog10(tg(iix,aa(pos2(ipost)-1)-1:aa(pos2(ipost)-1)+1)),ion.lines[0].tmax)]
               endfor

            endif
            ipos=fix(round(interpol(indgen(n_elements(z)),z,pos)))
            if where(tag_names(ion) eq 'PRESSURE') ne -1 then begin
               enr=interpol(en(iix,*),z,pos)
               rhor=interpol(rho(iix,*),z,pos)
               ipr=interpol(indgen(n_elements(ion.pressure)),ion.pressure,ion.lines[0].tmax*enr)
               itr=interpol(indgen(n_elements(ion.logtgr)),ion.logtgr,indgen(n_elements(pos))*0.+ion.lines[0].tmax)
               goftr=interpolate(ion.goftne,itr,ipr)*abund
            endif else begin
               goftr=interpol(goftn,ion.ioneq_logt,indgen(n_elements(pos))*0.+ion.lines[0].tmax)*abund
               enr=interpol(en(iix,*),z,pos)
               rhor=interpol(rho(iix,*),z,pos)
            endelse
            for i=0,n_elements(pos)-1 do rho(iix,ipos(i))=rhor(i)
            for i=0,n_elements(pos)-1 do en(iix,ipos(i))=enr(i)
            for i=0,n_elements(pos)-1 do goft(iix,ipos(i))=goftr(i)
         endfor
      end
   endcase
endif
endif
if nofits eq 1 then begin
   goft=goft*enm*(rhom*units.ur/d->getgrph())*exp(-tau)>0. ;;goft=goft*en*en*exp(-tau)>0.
endif else begin
   goft=goft*enm*(rhom*ur_si2cgs/d->getgrph())*exp(-tau)>0. ;;goft=goft*en*en*exp(-tau)>0.
endelse

if n_elements(cont) ne 0 then begin
   cs=3.d10                     ; light  speed
   aatocm=1.d8                  ; convert from \AA to cm
   h=6.62d-27                   ; H constant
   if n_elements(phabund) eq 0 then restore,'~/mpi3d/code/IDL/data/lines/contco.sav' else restore,'~/mpi3d/code/IDL/data/lines/contph.sav'
   emissint=fltarr(505)
   for itt=0,504 do emissint(itt)=interpol(infoc.emiss(*,itt),infoc.wvl,ion.lines[0].wvl)/ion.lines[0].wvl*h*cs*aatocm
   goftcont=interpol(emissint,ion.logtgr,alog10(tg(loc)))
   cont=goft*0.
   cont(loc)=goftcont
   if nofits eq 1 then begin
      cont=cont*en*(rho*units.ur/d->getgrph())*exp(-tau)>0.
   endif else begin
      cont=cont*en*(rho*ur_si2cgs/d->getgrph())*exp(-tau)>0.
   endelse
   goft=goft+cont
endif

d->setvar,goft
d->setvarname,cion

goft=0
if nottg eq 0 then delvar,tg
if notr eq 0 then delvar,rho
if notne eq 0 then delvar,en
delvar,tgi,rhoi,losi,eni,veli
return
end
