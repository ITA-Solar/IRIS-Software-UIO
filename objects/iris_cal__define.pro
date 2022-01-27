; 
;+
; NAME:
;       IRIS_CAL__DEFINE
;
; PURPOSE:
;       iris_cal__define defines the class structure 'iris_cal'.
;       this object is made to collect most if not all IRIS specific
;       parameters in one easily maintained location.
;
; CATEGORY:
;       IRIS Data analysis SW
;
; CALLING SEQUENCE:
;       The IRIS_CAL__DEFINE procedure is not called directly. An
;       object of class IRIS_SJI is created with the following
;       statement:
;                   iris_cal = obj_new('iris_cal')
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;       Objects of type IRIS_CAL which describes and contains 
;       IRIS specific parameters describing the instrument
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;       The procedure opens an object of class IRIS_CAL. 
;       This procedure includes various functions (methods of
;       class  'iris_cal' whose purpose is to get, display, and/or manipulate
;       the different fields of the object.
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       Spring/Summer 2012: Viggo Hansteen - first version
;
; $Id: iris_cal__define.pro,v 1.13 2014/02/03 15:28:21 mawiesma Exp $
;
;-
function iris_cal::init,earead=earead
  if n_elements(earead) eq 0 then earead=0
  self.title='IRIS Calibration Object'
  self.id0=['FUV','NUV','SJI_FUV','SJI_NUV']
  self.id=['FUV','NUV','SJI_1330','SJI_1400','SJI_2796','SJI_2832']
  self.lambda0=[1335,2800,1340,2800]
; for FUV and NUV this is the spectral pixel size
; for SJI this is the fwhm of the wavelength band (40 and 4Angstrom
; respectively for FUV and NUV SJI)
  self.dellambda0=[12.5d-3,25d-3,40.,4.] ; wavelength range in Angstrom over which to integrate
  self.dellambda=[12.5d-3,25d-3,12.5d-3,12.5d-3,25d-3,25d-3] ; wavelength range in Angstrom over which to integrate
; pixel size along the slit in arcseconds
  self.resy=1./6. 
; slit width or pixel size in other direction in arcseconds for FUV, NUV, SJI_FUV, SJI_NUV
  self.resx0=[1./3.,1./3.,1./6.,1./6.] 
  self.resx=[1./3.,1./3.,1./6.,1./6.,1./6.,1./6.] 
; effective area in cm^2 for FUV, NUV, SJI_FUV, SJI_NUV preliminary numbers
  self.effective_area0=[1.8,0.25, 0.45, 0.003] 
; ...and still preliminary but better: mostly based on component
;    measurements.  The offband values in particular include some
;    approximations.
  self.effective_area_dir=getenv('HOME')+'/iris/cal/counts'
  self.effective_area_file='A101RP122546_IRIS_spectral_response_effarea_20120618.sav'
  if earead then begin
    self->effective_area_read
  end
; conversion from arcsec^2 to sr
  self.arcsec2sr=(2.*!pi/360./60./60.)*(2.*!pi/360./60./60.)
; electrons per photon (in CCD detector): FUV has more energetic photons
  self.eperph0=[2,1,2,1] 
  self.eperph=[2,1,2,2,1,1] 
; dispersion and ccd parameters
  self.dispersion.nwfuv1=2072
  self.dispersion.nwfuv2=2072
  self.dispersion.nwnuv=2072
  self.dispersion.dispfuv1=0.01298
  self.dispersion.dispfuv2=0.01272
  self.dispersion.dispnuv=0.02546
  self.dispersion.wfuv10=1331.56
  self.dispersion.wfuv20=1406.79-self.dispersion.nwfuv2*self.dispersion.dispfuv2
  self.dispersion.wnuv0=2782.56
  self.nuv_sz=[2073,self.dispersion.nwnuv,0,1096]
  self.fuv1_sz=[0,self.dispersion.nwfuv1,0,1096]
  self.fuv2_sz=[2073,self.dispersion.nwfuv2,0,1096]
; slit position on slit jow images
; these might shift or become variable at or after launch
  self.sji_slitpos.nuv_xs0=518
  self.sji_slitpos.nuv_ys0=512
  self.sji_slitpos.fuv_xs0=1554
  self.sji_slitpos.fuv_ys0=512
;
  self.missing=-32768
; 
  self.cc=2.99792e5 ; speed of light
  return,1
end

pro iris_cal::help, description=description
;prints out this help, setting the 'description' keyword will also print the header info
  if arg_present(description) || keyword_set(description) then $
    obj_help,self, description=description $
  else $
    obj_help,self
end

function iris_cal::gettitle
  return,self.title
end

function iris_cal::getid,ib,old_effective_area=old_effective_area
  if n_elements(old_effective_area) eq 0 then old_effective_area=0
  if old_effective_area then id=self.id0 else id=self.id
  if n_elements(ib) ne 0 then return,id[ib]
  return,id
end

function iris_cal::getlambda0,ib
  if n_elements(ib) ne 0 then return,(self.lambda0)[ib]
  return,self.lambda0
end

function iris_cal::getdellambda,ib,old_effective_area=old_effective_area
  if n_elements(old_effective_area) eq 0 then old_effective_area=0
  if old_effective_area then dellambda=self.dellambda0 else dellambda=self.dellambda
  if n_elements(ib) ne 0 then return,dellambda[ib]
  return,dellambda
end

function iris_cal::getdispersion
  return,self.dispersion
end

function iris_cal::pixels_fuv1
  disp=self->getdispersion()
  return,findgen(disp.nwfuv1)+1
end

function iris_cal::pixels_fuv2
  disp=self->getdispersion()
  return,findgen(disp.nwfuv2)+disp.nwfuv1+1
end

function iris_cal::pixels_nuv
  disp=self->getdispersion()
  return,findgen(disp.nwnuv)+1
end

function iris_cal::pixels,region
  case region of 
  'FUV1': return,self->pixels_fuv1()
  'FUV2': return,self->pixels_fuv2()
  'NUV': return,self->pixels_nuv()
  else: begin
    message,'no such region '+region,/info
    return,-1
        end
  endcase
end

function iris_cal::lambda_fuv1
  disp=self->getdispersion()
  return,disp.wfuv10+findgen(disp.nwfuv1)*disp.dispfuv1
end

function iris_cal::lambda_fuv2
  disp=self->getdispersion()
  return,disp.wfuv20+findgen(disp.nwfuv2)*disp.dispfuv2
end

function iris_cal::lambda_nuv
  disp=self->getdispersion()
  return,disp.wnuv0+findgen(disp.nwnuv)*disp.dispnuv
end

function iris_cal::lambda,region
  case region of 
  'FUV1': return,self->lambda_fuv1()
  'FUV2': return,self->lambda_fuv2()
  'NUV': return,self->lambda_nuv()
  else: begin
    message,'no such region '+region,/info
    return,-1
        end
  endcase
end

function iris_cal::getresx,ib,old_effective_area=old_effective_area
  if n_elements(old_effective_area) eq 0 then old_effective_area=0
  if old_effective_area then resx=self.resx0 else resx=self.resx
  if n_elements(ib) ne 0 then return,resx[ib]
  return,resx
end

function iris_cal::geteperph,ib,old_effective_area=old_effective_area
  if n_elements(old_effective_area) eq 0 then old_effective_area=0
  if old_effective_area then eperph=self.eperph0 else eperph=self.eperph
  if n_elements(ib) ne 0 then return,eperph[ib]
  return,eperph
end

function iris_cal::getresy
  return,self.resy
end

function iris_cal::enph,lambda
  if n_elements(lambda) eq 0 then lambda=self->getlambda0()
  return,6.6d-27*3d8/(lambda*1d-10) ; energy per photon in erg
end

function iris_cal::getsji_slitpos
  return,self.sji_slitpos
end

function iris_cal::geteffective_area_read
  return,self.effective_area_read
end

pro iris_cal::effective_area_read,file=file,dir=dir,nonstandard=nonstandard
  forward_function iris_get_response
  if n_elements(dir) eq 0 then dir=self.effective_area_dir
  if n_elements(file) eq 0 then file=self.effective_area_file
  self.effective_area_read=1
  efile=dir+'/'+file
  if n_elements(nonstandard) eq 0 then begin
    irisea=iris_get_response()
    self.effective_area.ll=irisea.lambda
    self.effective_area.sgfuv=irisea.area_sg[*,0]
    self.effective_area.sgnuv=irisea.area_sg[*,1]
    self.effective_area.sjf133=irisea.area_sji[*,0]
    self.effective_area.sjf140=irisea.area_sji[*,1]
    self.effective_area.sjn279=irisea.area_sji[*,2]
    self.effective_area.sjn283=irisea.area_sji[*,3]
    self.effective_area.telccd=fltarr(n_elements(irisea.lambda))+1.0
  endif else begin
    if (file_info(efile)).exists then begin
      restore,efile,/verb
      self.effective_area.ll=ll
      self.effective_area.sgfuv=sgfuv
      self.effective_area.sgnuv=sgnuv
      self.effective_area.sjf133=sjf133
      self.effective_area.sjf140=sjf140
      self.effective_area.sjn279=sjn279
      self.effective_area.sjn283=sjn283
      self.effective_area.telccd=telccd
    endif else begin
      message,efile+' not found, returning',/info
      return
    endelse
  endelse
end

function iris_cal::geteffective_area,ib,lambda,old_effective_area=old_effective_area
  if n_elements(old_effective_area) eq 0 then old_effective_area=0
  if n_elements(ib) eq 0 then return,self.effective_area0
  if n_elements(lambda) eq 0 then begin
    message,'no lambda given, guessing lambda based on band',/info
    lambda=self->getlambda0(ib)
  endif
;
  if old_effective_area then return,self.effective_area0[ib] else begin
    if not self->geteffective_area_read() then self->effective_area_read
    ll=self.effective_area.ll
    case ib of
      0: ea=self.effective_area.sgfuv
      1: ea=self.effective_area.sgnuv
      2: ea=self.effective_area.sjf133
      3: ea=self.effective_area.sjf140
      4: ea=self.effective_area.sjn279
      5: ea=self.effective_area.sjn283
      else: begin
        message,'no such band!!',/info
        ea=fltarr(n_elements(ll))*(-1.)
            end
    endcase
    ea=ea*self.effective_area.telccd
    return,interpol(ea,ll,lambda/10.)
    message,'effective_areas(lambda) not coded up yet',/info
  endelse
end

function iris_cal::getccd_height,region
  if n_elements(region) eq 0 then begin
     message,'no wavelength region given returning nuv size',/info
     region='NUV'
  endif
  case strupcase(region) of
  'NUV': return,(self->getnuv_sz())[3]
  'FUV': return,(self->getfuv_sz())[3]
  'FUV1': return,(self->getfuv1_sz())[3]
  'FUV2': return,(self->getfuv2_sz())[3]
  else: begin
    message,region+' region does not exist',/info
    return,-1
        end
  endcase
end

function iris_cal::getccd_sz,region
  if n_elements(region) eq 0 then begin
     message,'no wavelength region given returning nuv size',/info
     region='NUV'
  endif
  ccd_height=self->getccd_height(region)
  case strupcase(region) of
  'NUV': return,[n_elements(self->pixels_nuv()),ccd_height]
  'FUV': return,[n_elements(self->pixels_fuv1())+n_elements(self->pixels_fuv2()),ccd_height]
  'FUV1': return,[n_elements(self->pixels_fuv1()),ccd_height]
  'FUV2': return,[n_elements(self->pixels_fuv2()),ccd_height]
  else: begin
    message,region+' region does not exist',/info
    return,-1
        end
  endcase
end

function iris_cal::missing
  return,self.missing
end

function iris_cal::getnuv_sz
  return,self.nuv_sz
end

function iris_cal::getfuv_sz
  return,self.fuv1_sz+[0,self.fuv2_sz[1],0,0]
end

function iris_cal::getfuv1_sz
  return,self.fuv1_sz
end

function iris_cal::getfuv2_sz
  return,self.fuv2_sz
end

function iris_cal::getarcsec2sr
  return,self.arcsec2sr
end

pro iris_cal__define
  nband0=4
  nband=6
  neff=3601
  struct={iris_cal, $
          title:'', $
          id0:strarr(nband0), $
          id:strarr(nband), $
          missing:-32768, $
          lambda0:fltarr(nband0), $
          dellambda0:fltarr(nband0), $
          dellambda:fltarr(nband), $
          resy:1.0,$
          resx0:fltarr(nband0), $
          resx:fltarr(nband), $
          effective_area0:fltarr(nband0), $
          arcsec2sr:1.0, $
          cc:1.0,$
          eperph0:fltarr(nband0), $
          eperph:fltarr(nband), $
          sji_slitpos:{slitpos,fuv_xs0:0,fuv_ys0:0,nuv_xs0:0,nuv_ys0:0},$
          nuv_sz:[0.,0.,0.,0.], $
          fuv1_sz:[0.,0.,0.,0.], $
          fuv2_sz:[0.,0.,0.,0.], $
          dispersion:create_struct(name='dispersion', $
                                   'wfuv10',1.0,$
                                   'wfuv20',1.0,$
                                   'wnuv0',1.0,$
                                   'dispfuv1',1.0,$
                                   'dispfuv2',1.0,$
                                   'dispnuv',1.0,$
                                   'nwfuv1',1,$
                                   'nwfuv2',1,$
                                   'nwnuv',1), $
          effective_area_read:0,$
          effective_area:create_struct(name='effective_area', $
                                   'll',fltarr(neff),$
                                   'sgfuv',fltarr(neff),$
                                   'sgnuv',fltarr(neff),$
                                   'sjf133',fltarr(neff),$
                                   'sjf140',fltarr(neff),$
                                   'sjn279',fltarr(neff),$
                                   'sjn283',fltarr(neff),$
                                   'telccd',dblarr(neff)),$
         effective_area_dir:'',$
         effective_area_file:'' $
          }
end
