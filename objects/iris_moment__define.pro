; 
;+
; NAME:
;       IRIS_MOMENT__DEFINE
;
; PURPOSE:
;       IRIS_MOMENT__DEFINE defines the class IRIS_MOMENT. Objects of this
;       class contains intensity, velocity and line width data 
;       (0th, 1st and 2nd order intensity moments)
;       from the IRIS instrument on SOLAR-B. 
;
; CATEGORY:
;       Hansteen/Wikstol Data analysis SW
;
; CALLING SEQUENCE:
;
; INPUTS:
;       data: Object of class IRIS_DATA, containing the IRIS data
;       from which the intensity data will be calculated and stored
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;       Objects of type IRIS_MOMENT 
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;       The procedure opens an object of class IRIS_MOMENT. In the
;       INIT method, the method called CONSTRUCT is called. This
;       method uses the information from the IRIS_DATA
;       object to perform moment calculations in
;       order to calculate moments for all line window profiles. 
;       The moments are stored in the instance data called 'w' 
;       of IRIS_MOMENT.
; 
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       04-Feb-2004: Viggo Hansteen/Oivind Wikstol - First version EIS_MOMENT__DEFINE
;       30-Jan-2013: Viggo Hansteen - Converted for IRIS data       
;       $Id: iris_moment__define.pro,v 1.14 2014/02/03 15:28:21 mawiesma Exp $
;-   
;
function iris_moment::init, data, iwin = iwin, fit = fit, $
                     group_leader = group_leader, $
                     lamtype = lamtype, line_def = line_def, debug=debug
;
  if n_params() eq 0 then begin
     message,' ,data, iwin=iwin,fit=fit,lamtype=lamtype,line_def=line_def',/cont
     return,-1
  endif
  if n_elements(group_leader) gt 0 then self.group_leader = group_leader $
    else self.group_leader = -1
  if n_elements(lamtype) eq 0 then self.lam_type = 'AA' $
    else self.lam_type=lamtype
  if n_elements(line_def) eq 0 then self.line_def = 0 else self.line_def = line_def
  if n_elements(fit) eq 0 then self.fit='moment' else self.fit = fit
  if n_elements(debug) eq 0 then self->setdebug,1
  self.default_win=-1
  self-> setcomment, 'IRIS_moment'
  self.aux=ptr_new(obj_new('iris_aux'))
  self.cal=ptr_new(obj_new('iris_cal'))
  self.mapping=data->getmapping()
  self.aux_info[0]=data->getaux_info()
  self.nfit = 20
  if n_params() ge 1 then begin
    if n_elements(iwin) eq 0 then begin
      message, 'No data window specified. Processing all...',/cont
      self-> construct, data, /all
    endif else begin
      format='('+strtrim(string(n_elements(iwin)),2)+'i3)'
      message, 'Processing window(s) ' +strtrim(string(iwin,format=format), 2) +'...',/cont
      self-> construct, data, iwin = iwin
    endelse
  endif
  return, 1
end

pro iris_moment::help, description=description
;prints out this help, setting the 'description' keyword will also print the header info
  if arg_present(description) || keyword_set(description) then $
    obj_help,self, description=description $
  else $
    obj_help,self
end

pro iris_moment::construct, data, iwin = iwin, all = all
  if n_params() lt 1 then begin
    message, ' ,data, iwin = iwin, all = all',/cont
    return
  endif
 ;
  nwin = data->getnwin()
  if n_elements(all) ne 0 then iwin=indgen(nwin)
  if n_elements(iwin) eq 0 then begin
    message, 'No line specified. Using default (0)',/cont
    iwin = [0]
  endif
  iwin=data->getwindx(iwin)
  self->setnwin,nwin
;
  self.naxis2 = data->getnslit()
  self.naxis3 = data->getnraster() ; & print,'nraster',self.nraster
  if ptr_valid(self.linelist) then ptr_free,self.linelist
  self.linelist = ptr_new(intarr(nwin))
  self.hdr[0]=ptr_new(data->gethdr(0))
  for iw=0,n_elements(iwin)-1 do begin
    if self.default_win eq -1 then self.default_win=iwin[iw]
    self.hdr[iwin[iw]+1]=ptr_new(data->gethdr(iwin[iw]+1))
;
    self.win_info[iwin[iw]].xs=0
    self.win_info[iwin[iw]].xw=self.nfit
    self.win_info[iwin[iw]].ys=data->getys(iwin[iw])
    self.win_info[iwin[iw]].yw=data->getyw(iwin[iw])
;
    self.line_id[iwin[iw]]=data->getline_id(iwin[iw])
    self.region[iwin[iw]]=data->getregion(iwin[iw],/full)
  endfor
;
  nwin = n_elements(iwin)
  if ptr_valid(self.linelist) then ptr_free,self.linelist
  self.linelist = ptr_new(intarr(nwin))
  self.lam=ptr_new(fltarr(data->getnwin(),2))
  for iw = 0,nwin-1 do begin
    (*self.linelist)[iw] = iwin[iw]
    (*self.lam)[iwin[iw],*]= $
       [min(data->getlam(iwin[iw])),max(data->getlam(iwin[iw]))]
    if ptr_valid(self.w[iwin[iw]]) then ptr_free,self.w[iwin[iw]]
    self.w[iwin[iw]] = $
      ptr_new(fltarr(self->getxw(iwin[iw]),self->getyw(iwin[iw]),self->getnexp(iwin[iw]))) 
    case self.fit of 
    'moment': begin
      self->moment,data,iwin[iw]
              end
    'gauss': begin
      self->gaussian,data,iwin[iw]
             end
    'dgf': begin 
      self->dgf,data,iwin[iw]
             end 
    else: begin
          ok=dialog_message('Line fit option '+self.fit+' not implemented')
          end
    endcase
  endfor
end

pro iris_moment::cleanup
  ptr_free,self.linelist
  ptr_free,self.lam
  ptr_free,self.mspec
  ptr_free,self.variablename
  if ptr_valid(self.cal) then begin
    obj_destroy,*self.cal
    ptr_free,self.cal
  endif
  index = where(ptr_valid(self.w),ct)
  if ct gt 0 then ptr_free,self.w[index]
end

pro iris_moment::read, file, iwin = iwin, all = all, init=init
  if n_elements(init) eq 1 then return
  data = obj_new('iris_data', file=file)
  self->construct,data,iwin=iwin,all=all
end

pro iris_moment::save,iwin=iwin, init=init
  if n_elements(init) eq 1 then return
  filename = self->getfilename()
  savefile = ''+'.fits'
  fxist =  file_test(savefile)
  if(fxist) then begin
    message =  'File already exists. Do you want to overwrite?'
    q =  dialog_message(message,/question,/center)
    if q eq 'No' then return
  endif
  if n_elements(iwin) eq 0 then iwin=indgen(self->getnwin())
  self->mkfits,filename,savefile,iwin
end

pro iris_moment::setlam_type,str
  self.lam_type = strtrim(str, 2)
  return
end

function iris_moment::getlam_type
  return, self.lam_type
end

function iris_moment::getlinelist
  return, *self.linelist
end

pro iris_moment::setlam_lim,iwin,lam
  (*self.lam)[iwin]=lam
end

function iris_moment::getlam_lim,iwin
  return,(*self.lam)[iwin,*]
end

function iris_moment::getwavelength,iwin
  crpix1=fxpar(self->gethdr(iwin+1),'crpix1')
  cdelt1=fxpar(self->gethdr(iwin+1),'cdelt1')
  crval1=fxpar(self->gethdr(iwin+1),'crval1')
  naxis1=fxpar(self->gethdr(iwin+1),'naxis1')
  return,crval1+(findgen(naxis1)+1.0-crpix1)*cdelt1
end

function iris_moment::getmspec,iwin
  return,*self.mspec[iwin]
end

function iris_moment::mk_iris_map,win,mom,int=int,vel=vel,wid=wid
  if keyword_set(wid) then mom=2
  if keyword_set(vel) then mom=1
  if keyword_set(int) then mom=0
  if n_elements(mom) eq 0 then mom=0
  iwin=(self->getwindx(win))[0]
  im=rotate(reform((self->getvar(iwin,/load,/noscale))[mom,*,*]),1)
  xp=self->getxpos() 
  nx=n_elements(xp)
  yp=self->getypos()
  ny=n_elements(yp)
  xp=xp#(1.+fltarr(ny))
  yp=(1.+fltarr(nx))#yp
  time=self->getinfo('DATE_OBS')
; should also put in test to see if dx is constant, in which case one
; can call make_map(im,xc=xc,yc=yc,dx=dx,dy=dy,time=time) which is
; more efficient
  iris_map=make_map(im,xc=xp,yc=yp,time=time)
  add_prop,iris_map,id='IRIS '+self->getline_id(iwin),/replace
  dur=(max(self->gettime())-min(self->gettime()))/1000. ; duration in seconds
  add_prop,iris_map,dur=dur,/replace
  ang=pb0r(time,/arcsec)
  add_prop,iris_map,b0=ang[1]
  add_prop,iris_map,rsun=ang[2]
  return,iris_map
end

pro iris_moment::setvariablename,variablename
  if ptr_valid(self.variablename) then ptr_free,self.variablename
  self.variablename=ptr_new(variablename)
end

function iris_moment::getvariablename
  return,*self.variablename
end

function iris_moment::getdebug
  return,self.debug
end

pro iris_moment::setdebug,debug
  self.debug=debug
end

function iris_moment::appreadme
; This function initializes some readme-files for the user, which are
; saved in the hidden .idl folder in the home directory of the user
; It returns the path to that directory, which is used to save the
; line window definitions
;
    AuthorDirname = 'viggoh'
    AuthorDesc = 'Viggo Hansteen, Institute of Theoretical Astrophysics, University of Oslo'
    AppDirname = 'iris_moment'
    AppDesc = 'IRIS Moment'
    AppReadmeText = 'iris_moment__define, written by Viggo Hansteen, ITA, University of Oslo ' + $
                    '(viggo.hansteen@astro.uio.no)' + $
                    '/n these files can be deleted without restrictions.'
    AppReadmeVersion=1
    return, APP_USER_DIR(AuthorDirname, AuthorDesc, AppDirname, AppDesc, AppReadmeText, AppReadmeVersion)
end

pro iris_moment__define
  mwin=25
  struct={iris_moment, $
          p0:0.0, $
          fit:'gaussian',$
          nfit:5,$
          debug:0, $
          lam_type:'pixels', $
          line_def:0, $
          linelist:ptr_new(), $
          lam:ptr_new(), $    
          mspec:ptrarr(mwin), $
          variablename:ptr_new(), $
          group_leader:-1, $
          inherits iris_data}
end

