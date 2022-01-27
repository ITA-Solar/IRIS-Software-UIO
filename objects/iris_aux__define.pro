;+
; NAME:
;       IRIS_AUX__DEFINE
;
; PURPOSE:
;       Defines the class IRIS_AUX, the class for auxiliary
;       objects for IRIS data. The major use of this class is to 
;       contain variables with axes names, menu items and the like,
;       that do not naturally fit elsewhere.
;
; CATEGORY:
;       Hansteen/Wikstol Data analysis SW
;
; CALLING SEQUENCE:
;         aux = obj_new('iris_aux'). The init method is
;         automatically run when the object is declared, and all the
;         objects parameters are set there.
;
; INPUTS:
;       None
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;       Defines the IRIS_AUX object and fille the parameters (instance
;       data) of the object in the init-method. Based on EIS_AUX.
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       XX-Aug-2001: Viggo Hansteen
;       08-Jan-2013: Viggo Hansteen first IRIS version.
;
; $Id: iris_aux__define.pro,v 1.18 2015/03/26 15:50:12 viggoh Exp $
;-
function iris_aux::init
  self.aa=string(197B)
  self.variablename='Intensity'
;  self.variableunit='erg/cm!e2!N/s/sr/'+self.aa

  self.variableunit='Data Numbers'
  self.momentnames=['Intensity', 'Velocity','Line width',$
                   'Int. error', 'Vel. error']
;  self.momentunits=['erg/cm!e2!N/s/sr', 'km/s', ' (FWHM)  m'+self.aa,$
;                    'erg/cm!e2!N/s/sr', 'km/s']  
self.momentunits=['Data Numbers', 'km/s', ' (FWHM)  m'+self.aa,$
                  'Data Numbers', 'km/s']

  self.wscale='pixels'
  self.sscale='pixels'
  self->setxytitle,wscale=self.wscale
  self->setxytitle,sscale=self.sscale
  if getenv('IRIS_ANCILLARY') eq '' then $
    set_logenv,'IRIS_ANCILLARY',concat_dir(getenv('SSW'),'iris/idl/uio/ancillary/')
  self.xsz_standard=500
  self.ysz_standard=500
  self.screenfac=0.75
  return,1
end

pro iris_aux::cleanup
  return
end

pro iris_aux::help, description=description
;prints out this help, setting the 'description' keyword will also print the header info
  if arg_present(description) || keyword_set(description) then $
    obj_help,self, description=description $
  else $
    obj_help,self
end

function iris_aux::getdrawsize,sizemode,aspect=aspect
  if n_elements(sizemode) eq 0 then sizemode='standard'
  case sizemode of 
    'standard': begin
      ysz=self.ysz_standard
      if n_elements(aspect) eq 0 then xsz=self.xsz_standard else xsz=self.ysz_standard*aspect
              end
    'big':begin
      ysz=(get_screen_size())[1]*self.screenfac
      if n_elements(aspect) eq 0 then xsz=(get_screen_size())[1]*self.screenfac else xsz=ysz*aspect
        end
    else: begin
      message,"unknown sizemode '"+sizemode+"' given, ",/info
      message,"possible choices are: 'standard' and 'big'",/info
      xsz=-1
      ysz=-1
          end    
  endcase
  return,fix([xsz,ysz])
end

pro iris_aux::setscreenfac,screenfac
  self.screenfac=screenfac
end

function iris_aux::getscreenfac
  return,self.screenfac
end

pro iris_aux::setwscale, scale
  self.wscale=strtrim(scale,2)
  return
end

function iris_aux::getwscale
  return,self.wscale
end

pro iris_aux::setsscale, scale
  self.sscale=strtrim(scale,2)
  return
end

function iris_aux::getsscale
  return,self.sscale
end

pro iris_aux::setxytitle,wscale=wscale,sscale=sscale,tscale=tscale
  if n_elements(wscale) eq 0 and n_elements(sscale) eq 0 then begin
    wscale='pixels'
    slitscale='pixels'
    rasterscale='step nr'
    timescale='step nr'
  endif
  if n_elements(wscale) eq 0 then begin
    if n_elements(sscale) eq 0 then scale='step nr'
    slitscale=sscale
    rasterscale=sscale
    if n_elements(tscale) eq 0 then tscale=sscale
    self.xytitle[1] = 'Slit pos ['+strtrim(slitscale,2)+']'
    self.xytitle[2] = 'Raster pos ['+strtrim(rasterscale,2)+']'
    self.xytitle[3] = 'Time ['+strtrim(tscale,2)+']'
    return
  endif
  if n_elements(sscale) eq 0 then begin
    self.xytitle[0] = '!4k!3 ['+strtrim(wscale,2)+']'
    return
  endif
  slitscale=sscale
  rasterscale=sscale
  self.xytitle[0] = '!4k!3 ['+strtrim(wscale,2)+']'
  self.xytitle[1] = 'Slit pos ['+strtrim(slitscale,2)+']'
  self.xytitle[2] = 'Raster pos ['+strtrim(rasterscale,2)+']'
  self.xytitle[3] = 'Time ['+strtrim(rasterscale,2)+']'
  return
end

function iris_aux::getxytitle,ivar
  if n_elements(ivar) eq 0 then return,self.xytitle $
  else return,self.xytitle[ivar]
end

pro iris_aux::setvariablename,varname
  self.variablename=varname
end

pro iris_aux::setvariableunit,unit
  self.variableunit=unit
end

function iris_aux::getvariablename
  return,self.variablename
end

function iris_aux::getvariableunit
  return,self.variableunit
end

function iris_aux::getmomentunits,moment
  if n_params() eq 0 then return, self.momentunits $ 
  else return,(self.momentunits)[moment]
end

function iris_aux::getmomentnames,moment
  if n_params() eq 0 then return, self.momentnames $ 
  else return,(self.momentnames)[moment]
end

pro iris_aux::loadct,ct,rgb=rgb,contrast=contrast
cct=ct
if datatype(cct) eq 'STR' then begin
  case cct of
  'int': cct=0
  'vel': cct=98
  'wid': cct=13
  'int+red': cct=97
  else: begin
    message,'no such ct '+cct+' defined, set to 0',/info
    cct=0
 end
endcase
endif
case cct of 
97: begin
  loadct,0,/silent
  tvlct,r,g,b,/get
  r[255]=255
  g[255]=0
  b[255]=0
  tvlct,r,g,b
   end
98: begin
; "Ada's" red and blue colorscale
   if n_elements(contrast) eq 0 then contrast=1.0
   iris_colors_yk,r,g,b, contrast=1.0
   end
99: begin
  restore,concat_dir(getenv('IRIS_ANCILLARY'),'bluegreyred.sav')
  tvlct,r,g,b
   end
13: begin 
  loadct,13,/silent
  tvlct,r,g,b,/get
  flc=255
  r[flc]=255 & g[flc]=255 & b[flc]=255
  tvlct,r,g,b
  end
else: begin
  loadct,cct,/silent
  tvlct,r,g,b,/get
      end
endcase
rgb={r:r,g:g,b:b}
end

pro iris_aux__define
  nitem=10
  struct={iris_aux, $
          aa:'',$
          xsz_standard:600,$
          ysz_standard:600,$
          screenfac:0.8, $
          title:'',$
          wscale:'',$
          sscale:'',$
          xytitle:strarr(nitem), $
          variablename:'', $
          variableunit:'', $
          momentnames:strarr(nitem), $
          momentunits:strarr(nitem) $
          }
end
