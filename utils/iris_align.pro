pro iris_align,file,dxy ,region=region,limb_xy=limb_xy, debug=debug, $
               level=level,fita=fita,derot=derot
;+
;
;   iris_align,file,dxy ,/limb
;
;            finds dxy array with shift between 
;            consecutive images
;            if /limb_xy is given, determines if data corresponds to limb
;            image and finds shift from determining x and y
;            for solar center (date is then needed)
;            pix and date taken from hdr if not given as keywords
;            region is [x0,y0,x1,y1] for alignment. Whole image if not
;            given
;            /derot will calculate rotation rate and store in dxy_rot
;              dxy returned will have rotation added in
;
;   based on matsc's mfg_align used for Hinode filtergrams
;   $Id: iris_align.pro,v 1.2 2014/01/28 13:11:05 viggoh Exp $ 
;-
if(n_params() lt 2) then begin
  print,'iris_align,file,dxy ,region=region,/limb,level=level,fita=fita,/derot'
  return
endif
if(n_elements(limb_xy) eq 0) then limb_xy=0
; for now assume slit jaw data, logic for including arrays of rasters
; can be included later
s=iris_sji(file)
file_align='iris_align_'+anytim2cal(s->getdate_obs(),/date,form=8)+'_'+ $
                         anytim2cal(s->getdate_obs(),/time,form=8)+'.sav'
;
pix=s->getresx()
if abs(s->getresy()-pix) gt 1.e-3 then begin
  message,'Warning x pixels size ne y pixel size',/info
endif

nt=s->getnexp()
nx=s->getnaxis1()
ny=s->getnaxis2()
dxy=fltarr(2,nt)
im=s->getvar()

; read first image
i=0
im1=im[*,*,i]
if(n_elements(region) ne 0) then im1=im1[region[0]:region[2],region[1]:region[3]]

; this limb finding piece of code is not in use...
if(limb_xy ne 0) then begin   
; check all corners to see if this is a limb image
  ll=mean(im1[0:49,0:49])
  lr=mean(im1[nx-50:nx-1,0:49])
  ul=mean(im1[0:49,ny-50:ny-1])
  ur=mean(im1[nx-50:nx-1,ny-50:ny-1])
  imean=mean(im1)
  lim=0.1*imean
  limb=(ll lt lim) or (ur lt lim)
endif

if(limb_xy ne 0) then begin
  if(n_elements(date) eq 0) then begin
    date=s->getdate_obs()
  endif
  iris_limb_fit,im1,date,x1,y1,level=level,pix=pix,fita=fita
endif

mtimer,/start
for i=1,nt-1 do begin
  im2=im[*,*,i]
  if(n_elements(region) ne 0) then im2=im2[region[0]:region[2],region[1]:region[3]]
  iw=where(im2 eq 0,count)
  if(count gt 10.*ny) then begin   ; missing sections of image
    dxy[*,i]=0.
  endif else begin
    if(limb_xy ne 0) then begin
      iris_limb_fit,im2,date,x2,y2,level=level,pix=pix,fita=fita,verbose=verbose
      dxy[0,i]=x1-x2
      dxy[1,i]=y1-y2
      if(abs(dxy[0,i]) gt 200) then dxy[0,i]=0
      if(abs(dxy[1,i]) gt 200) then dxy[1,i]=0
      x1=x2
      y1=y2
    endif else begin
      dxy[*,i]=malign(im1,im2)
    endelse   
    im1=im2
  endelse
  mtimer,'iris_align',i,nt,/remaining
endfor
if(keyword_set(derot)) then begin
  if(n_elements(region) eq 0) then begin
    x0=0.5*(nx-1)*pix
    y0=0.5*(ny-1)*pix
  endif else begin
    x0=0.5*(region[2]+region[0])*pix
    y0=0.5*(region[3]+region[1])*pix
  endelse
  x0=mean(s->xscale())+x0-0.5*(nx-1)*pix ; mean(s->xscale())=xcen
  y0=mean(s->yscale())+y0-0.5*(ny-1)*pix ; mean(s->yscale())=ycen
  dt=(max(s->ti2tai())-min(s->ti2tai()))/(nt-1.)
  xy=rot_xy(x0,y0,dt,date=s->getdate_obs())
  dxy_rot=(xy-[x0,y0])/pix
  if(keyword_set(debug)) then stop
endif else begin
  dxy_rot=[0,0]
endelse
mtimer,/stop
if(n_elements(region) eq 0) then region=[0,0,nx-1,ny-1]
dxy[0,*]=dxy[0,*]+dxy_rot[0]
dxy[1,*]=dxy[1,*]+dxy_rot[1]

save,dxy,dxy_rot,limb_xy,region,file=file_align
obj_destroy,s

end
