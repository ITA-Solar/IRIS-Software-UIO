; 
;+
; NAME:
;       IRIS_DATA__DEFINE
;
; PURPOSE:
;       iris_data__define defines the class structure 'iris_data'.
;
; CATEGORY:
;       IRIS Data analysis SW
;
; CALLING SEQUENCE:
;       The IRIS_DATA__DEFINE procedure is not called directly. An
;       object of class IRIS_DATA is created with the following
;       statement:
;                   iris_data = obj_new('iris_data')
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;       Objects of type IRIS_DATA which describes and contains 
;       a level 2 IRIS raster
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;       The procedure opens an object of class IRIS_DATA. 
;       This procedure includes various functions (methods of
;       class  'iris_data' whose purpose is to get and/or manipulate
;      the different fields of the object.
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       31-Dec-2012: Viggo Hansteen (based on EIS_HDR/DATA__DEFINE)
;       06-Dec-2022: viggoh/Fixed possibility of W->E scan.
;
; $Id: 2022-12-06 12:43 CET $
;
;-
function iris_data::init,file,verbose=verbose
; initializes object, calls read if "file" parameter given
  self.title='IRIS'
  self.xcen = 0.
  self.ycen = 0.
  self.default_sjiwin = -1
  self.default_win = -1
  self.aux=ptr_new(obj_new('iris_aux'))
  self.cal=ptr_new(obj_new('iris_cal'))
  if n_elements(file) ne 0 then begin
    self->read,file,verbose=verbose
  endif
  return,1
end

pro iris_data::close
; frees pointer to main data array "w" and closes all associated files
  for i=0,self.nwin-1 do begin
    if ptr_valid(self.w[i]) then ptr_free,self.w[i]
  endfor
  if self.lu ge 100 and self.lu le 128 then free_lun,self.lu
  for lwin=0,n_elements(self.lusji)-1 do begin
    if self.lusji[lwin] ge 100 and self.lusji[lwin] le 128 then free_lun,self.lusji[lwin]
  endfor
end

pro iris_data::cleanup
; called by obj_destroy, frees all pointers and closes all associated files
  if ptr_valid(self.aux) then begin
    obj_destroy,*self.aux
    ptr_free,self.aux
  endif
  if ptr_valid(self.cal) then begin
    obj_destroy,*self.cal
    ptr_free,self.cal
  endif
  for i=0,self.nwin do begin
    ptr_free,self.hdr[i]
  endfor
  self->close
  for i=0,self.nfiles-1 do begin
    ptr_free,self.aux_info[i].time
    ptr_free,self.aux_info[i].pztx
    ptr_free,self.aux_info[i].pzty
    ptr_free,self.aux_info[i].exptimef
    ptr_free,self.aux_info[i].exptimen
    ptr_free,self.aux_info[i].sumsptrf
    ptr_free,self.aux_info[i].sumsptrn
    ptr_free,self.aux_info[i].sumspatf
    ptr_free,self.aux_info[i].sumspatn
    ptr_free,self.aux_info[i].dsrcf
    ptr_free,self.aux_info[i].dsrcn
    ptr_free,self.aux_info[i].lutidf
    ptr_free,self.aux_info[i].lutidn
    ptr_free,self.aux_info[i].xcen
    ptr_free,self.aux_info[i].ycen
    ptr_free,self.aux_info[i].obs_vr
    ptr_free,self.aux_info[i].ophase
;
    ptr_free,self.obs_info[i].frmid
    ptr_free,self.obs_info[i].fdbidf
    ptr_free,self.obs_info[i].fdbidn
    ptr_free,self.obs_info[i].crsidf
    ptr_free,self.obs_info[i].crsidn
    ptr_free,self.obs_info[i].filef    
    ptr_free,self.obs_info[i].filen    
  endfor
  if self.lu ge 100 and self.lu le 128 then free_lun,self.lu
  return
end

pro iris_data::help, description=description
;prints out this help, setting the 'description' keyword will also print the header info
  if arg_present(description) || keyword_set(description) then $
    obj_help,self, description=description $
  else $
    obj_help,self
end

function iris_data::getnfiles
  return,self.nfiles
end

function iris_data::gettitle
  return,self.title
end

function iris_data::getfilename
;returns the raster filename
  return, self.filename
end

pro iris_data::setfilename,filename
  self.filename=filename
end

function iris_data::getfilename_sji,lwin
;returns the sji filename of lwin (lwin=0 through 5) 
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  return, self.filename_sji[lwin]
end

function iris_data::getmapping,iwin
  if n_params() eq 1 then return,(self.mapping)[iwin]
  return,self.mapping
end

function iris_data::getcomment
; returns comment for object
  return, self.comment
end

pro iris_data::setcomment,comment
; sets comment for object
  self.comment=comment
end

function iris_data::getaux
;returns the aux data as an iris_aux object
  return,*self.aux
end

function iris_data::getcal
;returns IRIS specific parameters as an iris_cal object
  return,*self.cal
end

function iris_data::missing
;returns the "missing" value
  bscale=self->getinfo('BSCALE')
  if datatype(bscale) eq 'INT'and bscale eq 0 then bscale=1.0
  bzero=self->getinfo('BZERO')
  return,bscale*((*self.cal)->missing())+bzero
end

pro iris_data::setnwin,nwin
  self.nwin=nwin
end

function iris_data::getnwin
;returns the number of line windows
  return,self.nwin
end

function iris_data::getnslit,iwin
; returns the number of pixels in slit for line window iwin
  nwin=self->getnwin()
  if n_params() eq 0 then begin
    return,round((self.naxis2)[0:nwin-1])
  endif else begin
    return,round(self.naxis2[iwin])
  endelse
end

function iris_data::getnraster,iwin
; returns the number of raster positions for line window iwin
  nwin=self->getnwin()
  if n_params() eq 0 or n_elements(iwin) eq 0 then begin
    return,round((self.naxis3)[0:nwin-1])
  endif else begin
    return,round(self.naxis3[iwin])
  endelse
end

function iris_data::getccd,region
; returns size of CCD depending on region value FUV, NUV, FUV1, FUV2
  if n_elements(region) eq 0 then begin
     message,'no wavelength region given returning nuv size',/info
     region='NUV'
  endif
  case strupcase(region) of
  'NUV': return,(*self.cal->getnuv_sz())
  'FUV': return,(*self.cal->getfuv_sz())
  'FUV1': return,(*self.cal->getfuv1_sz())
  'FUV2': return,(*self.cal->getfuv2_sz())
  else: begin
    message,region+' region does not exist',/info
    return,-1
        end
  endcase
end

function iris_data::getccd_sz,region
  return,*self.cal->getccd_sz(region)
end

function iris_data::getregion,iwin,full=full
;returns FUV or NUV depending on the region
  nwin=self->getnwin()
  if n_params() eq 0 then iwin=indgen(nwin)
  if n_elements(full) eq 0 then full=0
  region=self.region[iwin] ; was [self.mapping[iwin]]
  if not full then return,strmid(region,0,3) else return,region
end

function iris_data::getsit_and_stare,iwin
;returns 1 if raster is a sit-and-stare, 0 otherwise
  if n_elements(iwin) eq 0 then iwin=self.default_win
  ;if self->getfovx(iwin) lt (self->getresy(iwin))*2.1 then return,1 else return,0
  if abs(self->getinfo('CDELT3',iwin+1)) lt 0.1 then return,1 else return,0
end

function iris_data::binning_region,region
;returns the spatial summing, region has to be either 'FUV' or 'NUV'
  sumfuv=self->getinfo('SUMSPTRF') eq 0 ? 1:self->getinfo('SUMSPTRF')
  sumnuv=self->getinfo('SUMSPTRN') eq 0 ? 1:self->getinfo('SUMSPTRN')
  if self->getinfo('SUMSPTRL') ne 0 then begin
    sumfuv=self->getinfo('SUMSPTRL')
    sumnuv=self->getinfo('SUMSPTRL')
  endif
  case strmid(region,0,3) of
    'FUV': return,sumfuv
    'NUV': return,sumnuv
    else: begin
      message,'No such region '+region,/info
      return,-1
    endcase
  endcase
end

function iris_data::binning_spectral,iwin
;returns the spectral summing, region has to be either 'FUV' or 'NUV'
  if n_elements(iwin) eq 0 then iwin=indgen(self->getnwin())
  region=self->getregion(iwin) 
  sumfuv=self->getinfo('SUMSPTRF') eq 0 ? 1:self->getinfo('SUMSPTRF')
  sumnuv=self->getinfo('SUMSPTRN') eq 0 ? 1:self->getinfo('SUMSPTRN')
  if self->getinfo('SUMSPTRL') ne 0 then begin
    sumfuv=self->getinfo('SUMSPTRL')
    sumnuv=self->getinfo('SUMSPTRL')
  endif
  sumsptrl=intarr(n_elements(iwin))
  ind=where(region eq 'FUV',count)
  if count gt 0 then sumsptrl[ind]=sumfuv
  ind=where(region eq 'NUV',count)
  if count gt 0 then sumsptrl[ind]=sumnuv
  return,sumsptrl
end

function iris_data::scaling
  iwin=0
  if fxpar(self->gethdr(iwin),'O_BSCALE') ne 0 then begin
    scaling=[fxpar(self->gethdr(iwin),'O_BSCALE'),fxpar(self->gethdr(iwin),'O_BZERO')]
  endif else begin
    scaling=[fxpar(self->gethdr(iwin),'BSCALE'),fxpar(self->gethdr(iwin),'BZERO')]
    if scaling[0] eq 0 then scaling=[1.,0.]
  endelse
  return,scaling
end

function iris_data::getxytitle,axis
  return,*self.aux->getxytitle(axis)
end

function iris_data::getvariablename
;returns BTYPE (type of data)
  case datatype(self->getinfo('BTYPE')) of
  'INT': return,*self.aux->getvariablename()
  'STR': return,self->getinfo('BTYPE')
  endcase
end

function iris_data::getvariableunit
;returns BUNIT (unit of data)
  case datatype(self->getinfo('BUNIT')) of
  'INT': return,*self.aux->getvariableunit()
  'STR': return,self->getinfo('BUNIT')
  endcase
end

function iris_data::getdatatype,exten=exten
;returns datatype
  if n_elements(exten) eq 0 then exten=1
  case self->getinfo('BITPIX',exten) of
      8: return,'byte'
     16: return,'int'
    -32: return,'float'
    -64: return,'double'
    else: return,'float'
  endcase
end

function iris_data::getmomentunits,moment
  if n_params() eq 0 then return,*self.aux->getmomentunits() $ 
  else return,*self.aux->getmomentunits(moment)
end

function iris_data::getmomentnames,moment
  if n_params() eq 0 then return,*self.aux->getmomentnames() $ 
  else return,*self.aux->getmomentnames(moment)
end

function iris_data::getaux_info,ifile
; return pointer to aux_info stucture 
  if n_elements(ifile) eq 0 then ifile=0
  return,(self.aux_info[ifile])
end

function iris_data::getsji_info,lwin
; return pointer to sji_info structure
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  return,(self.sji_info[lwin])
end

function iris_data::aux_info,ifile
;returns auxiliary information about raster in a structure
  if n_elements(ifile) eq 0 then ifile=0
  aux_inf={time:*(self.aux_info[ifile].time), $
           pztx:*(self.aux_info[ifile].pztx), $
           pzty:*(self.aux_info[ifile].pzty), $
           exptimef:*(self.aux_info[ifile].exptimef), $
           exptimen:*(self.aux_info[ifile].exptimen), $
           sumsptrf:*(self.aux_info[ifile].sumsptrf), $
           sumsptrn:*(self.aux_info[ifile].sumsptrn), $
           sumspatf:*(self.aux_info[ifile].sumspatf), $
           sumspatn:*(self.aux_info[ifile].sumspatn), $
           dsrcf:*(self.aux_info[ifile].dsrcf), $
           dsrcn:*(self.aux_info[ifile].dsrcn), $
           lutidf:*(self.aux_info[ifile].lutidf), $
           lutidn:*(self.aux_info[ifile].lutidn), $
           xcen:*(self.aux_info[ifile].xcen), $
           ycen:*(self.aux_info[ifile].ycen), $
           obs_vr:*(self.aux_info[ifile].obs_vr), $
           ophase:*(self.aux_info[ifile].ophase) }
  return,aux_inf
end

function iris_data::sji_info,lwin
;returns auxiliary information about SJI in a structure
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  sji_inf={time:*(self.sji_info[lwin].time), $
           pztx:*(self.sji_info[lwin].pztx), $
           pzty:*(self.sji_info[lwin].pzty), $
           exptimes:*(self.sji_info[lwin].exptime), $
           slit:*(self.sji_info[lwin].slit), $
           sumsptrs:*(self.sji_info[lwin].sumsptrs), $
           sumspats:*(self.sji_info[lwin].sumspats), $
           dsrcs:*(self.sji_info[lwin].dsrcs), $
           lutids:*(self.sji_info[lwin].lutids), $
           xcen:*(self.sji_info[lwin].xcen), $
           ycen:*(self.sji_info[lwin].ycen), $
           sltpx1:*(self.sji_info[lwin].sltpx1), $
           sltpx2:*(self.sji_info[lwin].sltpx2), $
           obs_vr:*(self.sji_info[lwin].obs_vr), $
           ophase:*(self.sji_info[lwin].ophase) }
  return,sji_inf
end

function iris_data::obs_info,ifile
;returns info from auxiliary extension 2 for raster
  if n_elements(ifile) eq 0 then ifile=0
  obs_inf={frmid:*(self.obs_info[ifile].frmid), $
           fdbidf:*(self.obs_info[ifile].fdbidf), $
           crsidf:*(self.obs_info[ifile].crsidf), $
           fdbidn:*(self.obs_info[ifile].fdbidn), $
           crsidn:*(self.obs_info[ifile].crsidn), $
           filef:*(self.obs_info[ifile].filef), $
           filen:*(self.obs_info[ifile].filen)}
  return,obs_inf
end

function iris_data::obs_info_sji,lwin
  ;returns info from auxiliary extension 2 for SJI
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  obs_inf={frmid:*(self.sjiobs_info[lwin].frmid), $
           fdbids:*(self.sjiobs_info[lwin].fdbids), $
           crsids:*(self.sjiobs_info[lwin].crsids), $
           files:*(self.sjiobs_info[lwin].files)}
  return,obs_inf
end

; window info, window data

pro iris_data::show_lines
;prints wavelengths and names of windows
  print,'Spectral regions(windows)'
  for iwin=0,self->getnwin()-1 do begin
    print,iwin,self->getline_wvl(iwin,wscale='AA'),self->getline_id(iwin),format='(i2,3x,f7.2,3x,a)'
  endfor
  if max(self->getread_sji()) eq 1 then print,'Loaded Slit Jaw images'
  for lwin=0,5 do begin
    if self->getread_sji(lwin) then begin
      print,lwin,self->getsji_id(lwin),format='(i2,3x,a)'
   endif
  endfor
end

function iris_data::getpos,iwin
  if n_elements(iwin) eq 0 then iwin=self.default_win
  return,{xs:self->getxs(iwin),xw:self->getxw(iwin),ys:self->getys(iwin),yw:self->getyw(iwin)}
end

function iris_data::getxw,iwin
;returns width of windows (or window iwin)
  nwin=self->getnwin()
  if n_params() eq 0 then begin
    return,round((self.win_info.xw)[0:nwin-1])
  endif else begin
    return,round(self.win_info[iwin].xw)
  endelse
end

function iris_data::getyw,iwin
;returns height of windows (or window iwin)
  nwin=self->getnwin()
  if n_params() eq 0 then begin
    return,(self.win_info.yw)[0:nwin-1]
  endif else begin
    return,self.win_info[iwin].yw
  endelse
end

function iris_data::getxs,iwin
; returns start pixel x direction for window iwin
  nwin=self->getnwin()
  if n_params() eq 0 then begin
    return,(self.win_info.xs)[0:nwin-1]
  endif else begin
    return,self.win_info[iwin].xs
  endelse
end

function iris_data::getys,iwin
; returns start pixel y direction for window iwin
  nwin=self->getnwin()
  if n_params() eq 0 then begin
    return,(self.win_info.ys)[0:nwin-1]
  endif else begin
    return,self.win_info[iwin].ys
  endelse
end

pro iris_data::getwin,iwin,wd,pos,load=load,noscale=noscale
; get window iwin, into wd, position pos on ccd
  wd=self->getvar(iwin,load=load,noscale=noscale)
  pos=[self->getxs(iwin), self->getxw(iwin), self->getys(iwin), self->getyw(iwin)]
end

function iris_data::getdefault_win
;returns default window
  return,self.default_win
end

pro iris_data::setdefault,default_win
;sets default window
  self.default_win=default_win
end

function iris_data::getvar,iwin,revnegdx=revnegdx,load=load,noscale=noscale
;returns the data of iwin if 'load' is set, otherwise pointer to the file
  if n_elements(iwin) eq 0 then iwin=self.default_win
  if n_elements(load) eq 0 then load=0
  if n_elements(noscale) eq 0 then noscale=0
  if n_elements(revnegdx) eq 0 then revnegdx=1
  iwin=(self->getwindx(iwin))[0]
  neg=0
  if revnegdx and self->getdx(0,iwin=iwin) lt 0 then begin
     neg=1
     load=1
;     noscale=1
  endif
  if ptr_valid(self.w[iwin]) then begin
    if load then begin
      w=fltarr(self->getxw(iwin),self->getyw(iwin),self->getnraster(iwin))
      if neg then begin
        for i=0,self->getnraster(iwin)-1 do begin
          if noscale then w[*,*,i]=(*self.w[iwin])[*,*,self->getnraster(iwin)-1-i] $
          else w[*,*,i]=self->descale_array((*self.w[iwin])[*,*,self->getnraster(iwin)-1-i])
;          w[*,*,i]=self->descale_array((*self.w[iwin])[*,*,self->getnraster(iwin)-1-i])
        endfor
        return,w
      endif else begin
        for i=0,self->getnraster(iwin)-1 do begin
          if noscale then w[*,*,i]=(*self.w[iwin])[*,*,i] $
          else w[*,*,i]=self->descale_array((*self.w[iwin])[*,*,i])
        endfor
        return,w
      endelse
    endif else return,*self.w[iwin] 
  endif else return,-1
end

function iris_data::descale_array,var
; descale data in var: data=var*bscale+bzero
  bscale=self->getinfo('BSCALE')
  if datatype(bscale) eq 'INT'and bscale eq 0 then bscale=1.0
  bzero=self->getinfo('BZERO')
  return,var*bscale+bzero
end

function iris_data::getposition,iwin
  if n_elements(iwin) eq 0 then return,self.position
  return,self.position[iwin+1]
end

function iris_data::datamin,iwin
;returns DATAP01 for all windows or the equivalent for iwin
  if n_elements(iwin) eq 0 then tag='DATAP01' else tag='TDP01_'+strtrim(string(iwin+1),2)
  dmin=self->getinfo(tag)
  case datatype(dmin) of
    'STR': begin
       message,tag+' keyword set to NAN, setting datamin to 0',/info
      return,0
    endcase
    else: return,dmin
  endcase
end

function iris_data::datamin_sji,lwin
;returns DATAP01 for sji channel lwin
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  tag='DATAP10'
  dmin=self->getinfo(tag,lwin,/sji)
  case datatype(dmin) of
    'STR': begin
       message,tag+' keyword set to NAN, setting datamin to min(sji[lwin])',/info
      return,min(self->getsji(lwin),/nan)
    endcase
    else: return,dmin
  endcase
end

function iris_data::getsji,lwin,noload=noload
; extract sji data or assoc pointer to file with option /noload
  if n_elements(noload) eq 0 then noload=0
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  if ptr_valid(self.wsji[lwin]) then begin
    if not noload then begin
       sub=self->locsji(lwin)
       wsji=fltarr(self->getnaxis1_sji(lwin),self->getnaxis2_sji(lwin),sub[1]-sub[0]+1)
       for i=sub[0],sub[1] do $
         wsji[*,*,i-sub[0]]=self->descale_array((*(self.wsji[lwin]))[*,*,i])
       return,wsji 
    endif else return,*self.wsji[lwin]
  endif else return,-1
end

function iris_data::getdata
; extract entire object into structure
  d=self->gethdr(/struct)
  for iwin=0,self->getnwin()-1 do begin
    d=boost_tag(temporary(d),self->getvar(iwin),'w'+strtrim(string(iwin,format='(i2)'),2))
    d=boost_tag(temporary(d),self->getlam(iwin),'lam'+strtrim(string(iwin,format='(i2)'),2))
    d=boost_tag(temporary(d),reform(self->getpztx(iwin)),'pztx'+strtrim(string(iwin,format='(i2)'),2))
    d=boost_tag(temporary(d),reform(self->getpzty(iwin)),'pzty'+strtrim(string(iwin,format='(i2)'),2))
    d=boost_tag(temporary(d),reform(self->getexp(iwin=iwin)),'exp'+strtrim(string(iwin,format='(i2)'),2))
    d=boost_tag(temporary(d),reform(self->gettime(iwin)),'time'+strtrim(string(iwin,format='(i2)'),2))
  endfor
  return,d
end

function iris_data::getwindx,input
;returns window index of a given wavelength or window name
  if n_params() eq 0 then begin
    message,'getwindx,input',/info
    return,-1
  endif
  iwin=intarr(n_elements(input))
  for iw=0,n_elements(input)-1 do begin
    if datatype(input[iw]) eq 'STR' then begin
      iwin[iw]=(where((strupcase(self->getline_id())) eq $
                       strupcase(input[iw]),c))[0]
      if c eq 0 then begin
        message,'Line_id not found : '+input[iw],/info
        iwin[iw]=-1
      endif
    endif else begin
      if input[iw] ge 0 and input[iw] le (self->getnwin())-1 then begin
        iwin[iw]=input[iw]
      endif else begin
;   else e.g. input=1334.
        nwin=self->getnwin()
        winmax=fltarr(nwin)
        winmin=fltarr(nwin)
        for i=0,nwin-1 do begin 
          winmax[i]=max(self->getlam(i))
          winmin[i]=min(self->getlam(i))
       endfor
        prod=(winmax-input[iw])*(input[iw]-winmin)
        iwin[iw]=(where(prod gt 0,c))[0]
        if c eq 0 then begin
          message,'wavelength not found '+trim(input[iw],'(f10.2)'),/info
          iwin[iw]=-1
        endif 
      endelse
    endelse
  endfor
  return,iwin
end

function iris_data::getline_id,iwin
;returns window names
  nwin=self->getnwin()
  if n_params() eq 0 then begin
    return,(self.line_id)[0:nwin-1]
  endif else begin
    return,self.line_id[iwin]
  endelse
end

; sji windows

function iris_data::getsji_id,lwin
;returns sji channel names
  nwin=6
  if n_params() eq 0 then begin
    return,(self.sji_id)[0:nwin-1]
  endif else begin
    return,self.sji_id[lwin]
  endelse
end

function iris_data::getxw_sji,iwin
  nwin=6
  if n_params() eq 0 then begin
    return,(self.sji_info.xw)[0:nwin-1]
  endif else begin
    return,self.sji_info[iwin].xw
  endelse
end

function iris_data::getyw_sji,iwin
  nwin=6
  if n_params() eq 0 then begin
    return,(self.sji_info.yw)[0:nwin-1]
  endif else begin
    return,self.sji_info[iwin].yw
  endelse
end

function iris_data::getxs_sji,iwin
  nwin=6
  if n_params() eq 0 then begin
    return,(self.sji_info.xs)[0:nwin-1]
  endif else begin
    return,self.sji_info[iwin].xs
  endelse
end

function iris_data::getys_sji,iwin
  nwin=6
  if n_params() eq 0 then begin
    return,(self.sji_info.ys)[0:nwin-1]
  endif else begin
    return,self.sji_info[iwin].ys
  endelse
end

function iris_data::getfilename_sji,lwin
; return filename for sji file in channel lwin
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  return, self.filename_sji[lwin]
end

function iris_data::getslit_sji,lwin,indx,all=all
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  if n_elements(indx) eq 0 then begin
    if keyword_set(all) then return,reform(*(self.sji_info.slit)[lwin]) else $
                             return,(*(self.sji_info.slit)[lwin])[(self->locsji())[0]:(self->locsji())[1]]
  endif else return,(*(self.sji_info.slit)[lwin])[indx]
end

function iris_data::getread_sji,lwin
; return 1 if sji channel lwin read, zero otherwise
  if n_elements(lwin) eq 0 then return,self.sji_read else return,self.sji_read[lwin]
end

function iris_data::findiwin_sji,sji_id
; given name of sji window (eg SJI_1330) return channel number
  case strupcase(strtrim(sji_id,2)) of
    'SJI_1330': lwin=0
    'SJI_1400': lwin=1
    'SJI_2796': lwin=2
    'SJI_2832': lwin=3
    'SJI_1600W': lwin=4
    'SJI_5000W': lwin=5
    else: begin 
      lwin=-1
      message,'unknown SJI ID, returning -1'
    endelse
  endcase
  return,lwin
end

function iris_data::locsji,lwin
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  return,reform(self.locsji[lwin,*])
end

function iris_data::getdefault_sjiwin
; return default sji window number
  return,self.default_sjiwin
end

; find_slitpos0 is probably not useful anymore...

function iris_data::find_slitpos0,iwin
  if n_elements(iwin) eq 0 then begin
    message,'no slit jaw index given, assuming '+string(self.default_sjiwin,format='(I1)'),/info
    iwin=self.default_sjiwin
  endif
  shift=-999.
  for i=0,3 do begin
    if self->getread_sji(i) then shift=max([shift,reform(self->getpztx_sji(i))])
  endfor
  case strmid(strupcase(strtrim(self->getsji_id(iwin),2)),0,5) of
    'SJI_1': begin
       slitxs0=((self->getcal())->getsji_slitpos()).fuv_xs0
       slitys0=((self->getcal())->getsji_slitpos()).fuv_ys0
       return,{xs0:slitxs0-(self->getxs_sji(iwin))-shift,ys0:(self->getys_sji(iwin))}
           end
    'SJI_2': begin
       slitxs0=((self->getcal())->getsji_slitpos()).nuv_xs0
       slitys0=((self->getcal())->getsji_slitpos()).nuv_ys0
       return,{xs0:slitxs0-(self->getxs_sji(iwin))-shift,ys0:(self->getys_sji(iwin))}
           end
    else: begin
       message,'SJI ID: '+strupcase(strtrim(self->getsji_id(iwin),2))+' is unknown, assuming FUV',/info
       slitxs0=((self->getcal())->getsji_slitpos()).fuv_xs0
       slitys0=((self->getcal())->getsji_slitpos()).fuv_ys0
       return,{xs0:slitxs0-(self->getxs_sji(iwin))-shift,ys0:(self->getys_sji(iwin))}
           end
  endcase
end

function iris_data::find_slitpos,iwin,sjiwin=sjiwin,iexp=iexp,arcsec=arcsec
  if n_elements(arcsec) eq 0 then arcsec=0
  if n_elements(iexp) eq 0 then iexp=0
  nxpos=n_elements(self->xscale_sji(sjiwin))
  nypos=n_elements(self->yscale_sji(sjiwin))
  return,{xs:interpol(indgen(nxpos),self->xscale_sji(sjiwin),(self->aux_info()).xcen()), $
          ys:interpol(indgen(nypos),self->yscale_sji(sjiwin),(self->aux_info()).ycen())}
end

; wavelength and wavelength coordinates

function iris_data::getlam,iwin,wscale=wscale
;returns vector of wavelengths for columns in iwin
  if n_params() eq 0 then begin
    message,'no window nr input',/info
    iwin=-1
  endif
  if n_elements(wscale) eq 0 then wscale='AA'
  iwin=(self->getwindx(iwin))[0]
  if iwin eq -1 then return,-1
  if wscale eq 'pixels' then begin
    bin_sp=self->binning_region(self->getregion(iwin))
    return,1+(self->getxs(iwin)-1)*bin_sp+indgen(self->getxw(iwin)*bin_sp)
  endif
  crpix1=fxpar(self->gethdr(iwin+1),'crpix1')
  cdelt1=fxpar(self->gethdr(iwin+1),'cdelt1')
  crval1=fxpar(self->gethdr(iwin+1),'crval1')
  naxis1=fxpar(self->gethdr(iwin+1),'naxis1')
  if crval1 ne 0 then begin
    return,crval1+(findgen(naxis1)+1.0-crpix1)*cdelt1
  endif else begin ; use calibration object values
    message,'CRVAL1 keyword not found, using calibration object values to compute lambda',/info
    xs=(self->getxs())[iwin]-(self->getccd(self->getregion(iwin)))[0]
    xw=(self->getxw())[iwin]
    return,(self->getlambda(self->getregion(iwin),wscale='AA'))[xs:xs+xw-1]
  endelse
end

pro iris_data::setline_wvl,iwin,lambda0
  if n_params() ne 2 then begin
    message,'d->setline_wvl,iwin,lambda0',/info
    return
  endif
  self.line_wvl[iwin]=lambda0
end

function iris_data::getline_wvl,iwin,wscale=wscale
;returns line-wavelength for iwin either in 'pixels' (default) or 'AA'
  if n_elements(wscale) eq 0 then wscale='pixels'
  if n_elements(iwin) eq 0 then begin
    message,'result=d->getline_wvl(iwin,wscale={pixles (default),AA})',/info
    return,-1
  endif
  lam0=self.line_wvl[iwin]
  case wscale of 
  'pixels': begin
    pix=self->getlambda(self->getregion(iwin),wscale='pixels')
    lam=self->getlambda(self->getregion(iwin),wscale='AA')
    return,interpol(pix,lam,lam0)
            end
  'AA': return,lam0 
  else: begin
          message,'no such wscale',/info
          return,-1
        end
  endcase
end

function iris_data::crwvl,iwin
;returns CRPIX1, CDELT1, CRVAL1 in structure for iwin
  if n_params() ne 1 then begin
    message,'crvals=d->crwvl(iwin)',/info
    return,-1
  endif
  
  crpix1=fxpar(self->gethdr(iwin+1),'crpix1')
  cdelt1=fxpar(self->gethdr(iwin+1),'cdelt1')
  crval1=fxpar(self->gethdr(iwin+1),'crval1')
  return,{crpix1:crpix1,cdelt1:cdelt1,crval1:crval1}
end

function iris_data::getlambda,region,wscale=wscale
  if n_params() lt 1 then begin
    message,'region not given, assuming "FUV"',/info
    region='FUV'
  endif
  if n_elements(wscale) eq 0 then wscale=(self->getaux())->getwscale()
  bin_sp=self->binning_region(region)
  case region of
  'FUV': begin
     pixels1=(self->getcal())->pixels('FUV1')
     pixels2=(self->getcal())->pixels('FUV2')
     if wscale eq 'pixels' then return,[pixels1,pixels2]
     lambda1=-1
     iwin1=min(where(self->getregion(/full) eq 'FUV1'))
     if iwin1 ne -1 then begin
        s=self->crwvl(iwin1)
        if s.crval1 ne 0 then begin
          lambda1=s.crval1+(pixels1-(self->getxs(iwin1)+s.crpix1-1)*bin_sp)*s.cdelt1/bin_sp
        endif
     endif
     if lambda1[0] eq -1 then begin
       message,'CRVAL1 keyword or window '+iwin2+'not found,',/info
       message,'using calibration object values to compute lambda',/info
       lambda1=(self->getcal())->lambda('FUV1')
     endif
     lambda2=-1
     iwin2=min(where(self->getregion(/full) eq 'FUV2'))
     if iwin2 ne -1 then begin
        s=self->crwvl(iwin2)
        if s.crval1 ne 0 then begin
          lambda2=s.crval1+(pixels2-(self->getxs(iwin2)+s.crpix1-1)*bin_sp)*s.cdelt1/bin_sp
        endif
     endif
     if lambda2[0] eq -1 then begin
       message,'CRVAL1 keyword or window '+string(iwin2)+' not found,',/info
       message,'using calibration object values to compute lambda',/info
       lambda2=(self->getcal())->lambda('FUV2')
     endif
     return,[lambda1,lambda2]
  end
  else: begin
     pixels=(self->getcal())->pixels(region)
     if wscale eq 'pixels' then return,pixels
     lambda=-1
     iwin=min(where(self->getregion(/full) eq region))
     if iwin ne -1 then begin
        s=self->crwvl(iwin)
        if s.crval1 ne 0 then begin
          lambda=s.crval1+(pixels-(self->getxs(iwin)+s.crpix1-1)*bin_sp)*s.cdelt1/bin_sp
        endif
     endif
     if lambda[0] eq -1 then begin
       message,'CRVAL1 keyword or window '+iwin+'not found,',/info
       message,'using calibration object values to compute lambda',/info
       lambda=(self->getcal())->lambda(region)
     endif
     return,lambda
  end
  endcase
end

function iris_data::getdispersion,iwin,region=region
; return dispersion for window iwin
  if n_params() lt 1 then begin
    if n_elements(region) eq 0 then begin
      message,'window or region not given, assuming "FUV1"',/info
      region='FUV1'
    endif
  endif
  if n_elements(iwin) ne 0 then region=self->getregion(iwin,/full)
  if self->getinfo('cdelt1',iwin+1) ne 0 then begin
    return,self->getinfo('cdelt1',iwin+1)
  endif
  disp=*self.cal->getdispersion()
  binning=self->binning_region(region)
  case region of
  'FUV1': dispersion=disp.dispfuv1*binning
  'FUV2': dispersion=disp.dispfuv2*binning
  'NUV' : dispersion=disp.dispnuv*binning
  endcase
  return,dispersion
end

function iris_data::getwd_def
  return,self.wd_def
end

pro iris_data::setline_px, iwin, var
; define line position in window
  self.wd_def[iwin].line_px = var
  return
end

pro iris_data::setcont_px, iwin, var
; define continuum position in window
  self.wd_def[iwin].cont_px = var
  return
end

function iris_data::getwscale
  return,*self.aux->getwscale()
end

; fits headers 

function iris_data::getinfo,tag,iext,sji=sji
;returns value of 'tag' in the header of extension iext
  if keyword_set(sji) then begin
    if n_elements(iext) eq 0 then iext=self.default_sjiwin
    result = fxpar(self->gethdr_sji(iext),tag)
    if (tag eq 'OBS_DESC') && (size(result, /type) ne 7) then result = fxpar(self->gethdr_sji(iext),'OBS_DEC')
  endif else begin
    if n_elements(iext) eq 0 then iext=0
    result = fxpar(self->gethdr(iext),tag)
    if (tag eq 'OBS_DESC') && (size(result, /type) ne 7) then result = fxpar(self->gethdr(iext),'OBS_DEC')
  endelse
  return,result
end

function iris_data::gethdr,iext,struct=struct
;returns header of extension iext, if /struct is set then structure is returned
  if n_elements(iext) eq 0 then iext=0
  if n_elements(struct) eq 0 then struct=0
  if ~ptr_valid(self.hdr[iext]) then begin
    print,'invalid index (iext), valid indices are:'
    self->show_lines
    return, -1
  endif
  if struct then return,fitshead2struct(*self.hdr[iext]) $
  else return,*self.hdr[iext]
end

function iris_data::getnaxis1,iwin
  if n_elements(iwin) eq 0 then iwin=self.default_win
  return,fxpar(self->gethdr(iwin+1),'NAXIS1')
end

function iris_data::getnaxis2,iwin
  if n_elements(iwin) eq 0 then iwin=self.default_win
  return,fxpar(self->gethdr(iwin+1),'NAXIS2')
end

function iris_data::gethdr_sji,lwin,struct=struct
;returns header of sji channel lwin, if /struct is set then structure is returned
  if n_elements(lwin) eq 0 then begin
    lwin=self.default_sjiwin
  endif else begin
    if ~ptr_valid(self.hdr[lwin]) then begin
      print,'invalid index (lwin), valid indices are:'
      self->show_lines
      return, -1
    endif
  endelse
  if n_elements(struct) eq 0 then struct=0
  if struct then return,fitshead2struct(*self.hdrsji[lwin[0]]) $
  else return,*self.hdrsji[lwin[0]]
end

function iris_data::getnaxis1_sji,iwin
  if n_elements(iwin) eq 0 then iwin=self.default_sjiwin
  return,self.naxis1sji[iwin]
end

function iris_data::getnaxis2_sji,iwin
  if n_elements(iwin) eq 0 then iwin=self.default_sjiwin
  return,self.naxis2sji[iwin]
end

function iris_data::getcrsid,iwin
  return,''
end

function iris_data::getobsid,iwin
;returns OBS ID
  return,gt_tagval(self->gethdr(0),'OBSID',missing='OBSID tag missing!')
end

function iris_data::getfdbid,iwin
  return,''
end

; positions and spatial coordinates

function iris_data::fixpos,x
  badf=where((self->aux_info()).dsrcf lt 0)
  goodf=where((self->aux_info()).dsrcf gt 0)
  xorg=x
  if badf[0] ne -1 then begin
    for i=0,n_elements(badf)-1 do begin
      j=1
      repeat begin 
        lb=badf[i]-j > (-1)
        j=j+1
      endrep until where(lb eq badf) eq -1 or lb eq -1
      j=1
      repeat begin 
        ub=badf[i]+j < n_elements(x)
        j=j+1
      endrep until where(ub eq badf) eq -1 or ub eq n_elements(x)
      if lb gt -1 and ub lt n_elements(x) then $
                x[badf[i]]=interpolate([x[lb],x[ub]],(badf[i]-lb)/float(ub-lb)) $
      else begin
        if n_elements(goodf) lt 2 then begin
          message,'Only 1 good position found, giving up warp',/info
          return,xorg
        endif
        if lb eq -1 then begin
          p=goodf[0:1]
          x[badf[i]]=x[p[0]]+(p[0]-badf[i])*(x[p[1]]-x[p[0]])/(p[1]-p[0])
       endif
        if ub eq n_elements(x) then begin
          p=goodf[n_elements(goodf)-2:n_elements(goodf)-1]
          x[badf[i]]=x[p[1]]+(badf[i]-p[1])*(x[p[1]]-x[p[0]])/(p[1]-p[0])
        endif
      endelse
    endfor  
 endif
 return,x
end

function iris_data::getxpos,indx,iwin=iwin,sx=sx,sjiwin=sjiwin,sjiexpnr=sjiexpnr
; returns x coordinates of index indx for window parameter iwin=iwin
  if n_elements(iwin) eq 0 then iwin=self.default_win
; do we need to fix this xcen's bad data?
  xcen=self->fixpos((self->aux_info()).xcen)
  angle=round(self->getinfo('SAT_ROT'))
  if angle lt 0 then angle=360+angle
  r90=abs(angle mod 90) lt 1 or abs(abs(angle mod 90)-90) lt 1
  if r90 then begin 
    xtitle='Solar X [arcsec]'
    case angle of 
        0: begin
          xscale=xcen
          rot=0
      endcase
       90: begin
          if n_elements(indx) eq 0 then indx=0
          naxis=self->getnaxis2(iwin)
          crval1=self->getinfo('CRVAL3',iwin+1)
          crpix1=self->getinfo('CRPIX2',iwin+1)
          cdelt1=self->getinfo('CDELT2',iwin+1)
          xscale=crval1-(findgen(naxis)+1.0-crpix1)*cdelt1
          rot=3
      endcase
      180: begin
          xscale=xcen
          rot=2
      endcase
      270: begin
          if n_elements(indx) eq 0 then indx=0
          crval1=xcen[indx]
          crpix1=self->getinfo('CRPIX2',iwin+1)
          cdelt1=self->getinfo('CDELT2',iwin+1)
          naxis=self->getnaxis2(iwin)
          xscale=crval1+(findgen(naxis)+1.0-crpix1)*cdelt1
          rot=1
      endcase
      -90: begin
          if n_elements(indx) eq 0 then indx=0
          crval1=xcen[indx]
          crpix1=self->getinfo('CRPIX2',iwin+1)
          cdelt1=self->getinfo('CDELT2',iwin+1)
          naxis=self->getnaxis2(iwin)
          xscale=crval1+(findgen(naxis)+1.0-crpix1)*cdelt1
          rot=1
      endcase
    endcase
  endif else begin
    if total(self->getread_sji()) ne 0 then begin
      if N_ELEMENTS(sjiexpnr) eq 0 then sjiexpnr=0
      xscale=(self->sji_info(sjiwin)).sltpx1[sjiexpnr]*self->getinfo('CDELT1',/sji) $ ;0 has to be changed to the actual exposure number and actual sji-index
             +((self->getpztx())[0]-(self->getpztx_sji(sjiwin))[0]) $ ;dito for the second 0 here
             +self->getpztx()-(self->getpztx())[0]
    endif else begin
      xscale=self->getpztx()-(self->getpztx())[0]
    endelse
    xtitle='Instrument X [arcsec]'
    rot=-1
  endelse
  sx={xtitle:xtitle,rot:rot}
  return,xscale
end

function iris_data::getypos,indx,iwin=iwin,sy=sy
; returns y coordinates of index indx for window parameter iwin=iwin
  if n_elements(iwin) eq 0 then iwin=self.default_win
  ycen=self->fixpos((self->aux_info()).ycen)
  angle=round(self->getinfo('SAT_ROT'))
  if angle lt 0 then angle=360+angle
  r90=abs(angle mod 90) lt 1 or abs(abs(angle mod 90)-90) lt 1
  if r90 then begin 
    ytitle='Solar Y [arcsec]'
    case angle of 
        0: begin
          crval2=self->getinfo('CRVAL2',iwin+1)
          crpix2=self->getinfo('CRPIX2',iwin+1)
          cdelt2=self->getinfo('CDELT2',iwin+1)
          naxis=self->getnaxis2(iwin)
          yscale=crval2+(findgen(naxis)+1.0-crpix2)*cdelt2
          rot=0
      endcase
       90: begin
          yscale=reverse(ycen)
          rot=3
      endcase
      180: begin
          crval2=self->getinfo('CRVAL2',iwin+1)
          crpix2=self->getinfo('CRPIX2',iwin+1)
          cdelt2=self->getinfo('CDELT2',iwin+1)
          naxis=self->getnaxis2(iwin)
          yscale=crval2+(findgen(naxis)+1.0-crpix2)*cdelt2
          rot=2
      endcase
      270: begin
          yscale=ycen
          rot=1
      endcase
      -90: begin
          yscale=ycen
          rot=1
      endcase
    endcase
  endif else begin
    naxis=self->getnaxis2(iwin)
    cdelt2=self->getinfo('CDELT2',iwin+1)
    yscale=findgen(naxis)*cdelt2
    ytitle='Instrument Y [arcsec]'
    rot=-1
  endelse
  sy={ytitle:ytitle,rot:rot}
  return,yscale
end

function iris_data::getxcen,iwin
; return xcen
  self.xcen=fxpar(self->gethdr(0),'XCEN')
  return,self.xcen
end

function iris_data::getycen,iwin
; return ycen
  self.ycen=fxpar(self->gethdr(0),'YCEN')
  return,self.ycen
end

function iris_data::getfovx_sji,lwin
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  return,max(self->xscale_sji(lwin))-min(self->xscale_sji(lwin))
end

function iris_data::getfovy_sji,lwin
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  return,max(self->yscale_sji(lwin))-min(self->yscale_sji(lwin))
end

function iris_data::xscale_sji,lwin
; return x-coordinates for slit jaw window lwin
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  rot=self->getinfo('SAT_ROT',lwin,/sji)
  if finite(rot) then rot=round(rot)
  if rot lt 0 then rot=360+rot
  r90=abs(rot mod 90) lt 1 or abs(abs(rot mod 90)-90) lt 1
  if r90 then begin 
    crval1=self->getinfo('CRVAL1',lwin,/sji)
    case rot of 
        0: begin
          crpix1=self->getinfo('CRPIX1',lwin,/sji)
          cdelt1=self->getinfo('CDELT1',lwin,/sji)
          naxis=self->getnaxis1_sji(lwin)
          sign=1
      endcase
       90: begin
          crpix1=self->getinfo('CRPIX2',lwin,/sji)
          cdelt1=self->getinfo('CDELT2',lwin,/sji)
          naxis=self->getnaxis2_sji(lwin)
          sign=-1
      endcase
      180: begin
          crpix1=self->getinfo('CRPIX1',lwin,/sji)
          cdelt1=self->getinfo('CDELT1',lwin,/sji)
          naxis=self->getnaxis1_sji(lwin)
          sign=-1
      endcase
      270: begin
          crpix1=self->getinfo('CRPIX2',lwin,/sji)
          cdelt1=self->getinfo('CDELT2',lwin,/sji)
          naxis=self->getnaxis2_sji(lwin)
          sign=1
      endcase
      -90: begin
          crpix1=self->getinfo('CRPIX2',lwin,/sji)
          cdelt1=self->getinfo('CDELT2',lwin,/sji)
          naxis=self->getnaxis2_sji(lwin)
          sign=1
      endcase
      else: begin ;rot can be NAN, then we treat it as 0
        crpix1=self->getinfo('CRPIX1',lwin,/sji)
        cdelt1=self->getinfo('CDELT1',lwin,/sji)
        naxis=self->getnaxis1_sji(lwin)
        sign=1
      endcase
    endcase
  endif else begin
    crval1=0.0
    crpix1=0.0
    cdelt1=self->getinfo('CDELT1',lwin,/sji)
    naxis=self->getnaxis1_sji(lwin)
    sign=1
  endelse
  xscale=crval1+sign*(findgen(naxis)+1.0-crpix1)*cdelt1
  if rot eq 180 then return,reverse(xscale) else return,xscale
end

function iris_data::yscale_sji,lwin
; return y-coordinates for slit jaw window lwin
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  rot=self->getinfo('SAT_ROT',lwin,/sji)
  if finite(rot) then begin
    rot=round(rot)
    if rot lt 0 then rot=360+rot
    r90=abs(rot mod 90) lt 1 or abs(abs(rot mod 90)-90) lt 1
    if r90 then begin 
      crval2=self->getinfo('CRVAL2',lwin,/sji)
      case rot of 
          0: begin
            crpix2=self->getinfo('CRPIX2',lwin,/sji)
            cdelt2=self->getinfo('CDELT2',lwin,/sji)
            naxis=self->getnaxis2_sji(lwin)
        endcase
         90: begin
            crpix2=self->getinfo('CRPIX1',lwin,/sji)
            cdelt2=self->getinfo('CDELT1',lwin,/sji)
            naxis=self->getnaxis1_sji(lwin)
        endcase
        180: begin
            crpix2=self->getinfo('CRPIX2',lwin,/sji)
            cdelt2=self->getinfo('CDELT2',lwin,/sji)
            naxis=self->getnaxis2_sji(lwin)
        endcase
        270: begin
            crpix2=self->getinfo('CRPIX1',lwin,/sji)
            cdelt2=self->getinfo('CDELT1',lwin,/sji)
            naxis=self->getnaxis1_sji(lwin)
        endcase
        -90: begin
            crpix2=self->getinfo('CRPIX1',lwin,/sji)
            cdelt2=self->getinfo('CDELT1',lwin,/sji)
            naxis=self->getnaxis1_sji(lwin)
        endcase
      endcase
    endif else begin
      crval2=0.0
      crpix2=0.0
      cdelt2=self->getinfo('CDELT2',lwin,/sji)
      naxis=self->getnaxis2_sji(lwin)
    endelse
  endif else begin
    crval2=0.0
    crpix2=0.0
    cdelt2=self->getinfo('CDELT2',lwin,/sji)
    naxis=self->getnaxis2_sji(lwin)
  endelse
  yscale=crval2+(findgen(naxis)+1.0-crpix2)*cdelt2
  if rot eq 180 then return,reverse(yscale) else return,yscale
end

function iris_data::getxcen_sji,lwin
; return xcen for slit jaw channel lwin
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  return,fxpar(self->gethdr_sji(lwin),'XCEN')
end

function iris_data::getycen_sji,lwin
; return ycen for slit jaw channel lwin
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  return,fxpar(self->gethdr_sji(lwin),'YCEN')
end

function iris_data::getfovx,iwin
  if n_params() eq 0 then iwin=self.default_win
  return,max(self->getpztx(self.mapping[iwin]))-min(self->getpztx(self.mapping[iwin]))
end

function iris_data::getfovy,iwin
  if n_params() eq 0 then begin
    return,self->getyw()*self->getresy()
  endif else begin
    return,self.getyw(iwin)*self->getresy()
  endelse
end

function iris_data::getresy,iwin
; return resolution in y (slit) direction
  if n_elements(iwin) eq 0 then iwin=self.default_win
  lwin=iwin+1
  cdelt2=fxpar(self->gethdr(lwin),'CDELT2')
  if cdelt2 eq 0 then begin
    message,'No cdelt2 found in header, using default resolution',/info
    return,*(self.cal)->getresy()
  endif
  return,cdelt2
end

function iris_data::getdx,istep,iwin=iwin
; return resolution in x (raster) direction
  if n_elements(iwin) eq 0 then iwin=self.default_win
  m=self.mapping[iwin]
  nraster=self->getnraster(iwin)
  dx=shift((*self.aux_info[m].pztx),-1)-(*self.aux_info[m].pztx)
  dx[nraster-1]=dx[nraster-2]
  if n_params() gt 0 then return,dx[istep] else return,dx
end

function iris_data::getresx_sji,iwin
; return slit jaw resolution in x direction
  if n_elements(iwin) eq 0 then iwin=self.default_sjiwin
  cdelt1=fxpar(self->gethdr_sji(iwin),'CDELT1')
  if cdelt1 eq 0 then begin
    message,'No cdelt1 found in header, using default resolution',/info
    ib=where(self->getsji_id(iwin) eq (self->getcal())->getid())
    return,*(self.cal)->getresx(ib)
  endif
  return,cdelt1
end

function iris_data::getresy_sji,iwin
; return slit jaw resolution in y direction
  if n_elements(iwin) eq 0 then iwin=self.default_sjiwin
  cdelt2=fxpar(self->gethdr_sji(iwin),'CDELT2')
  if cdelt2 eq 0 then begin
    message,'No cdelt2 found in header, using default resolution',/info
    return,*(self.cal)->getresy()
  endif
  return,cdelt2
end

function iris_data::getpztx,iwin,indx
  if n_elements(iwin) eq 0 then iwin=self.default_win
  if n_elements(indx) eq 0 then return,*(self.aux_info.pztx)[self.mapping[iwin]] $
  else return,(*(self.aux_info.pztx)[self.mapping[iwin]])[indx]
end

function iris_data::getpztx_sji,lwin,indx,all=all
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  if n_elements(indx) eq 0 then begin
    if keyword_set(all) then return,reform(*(self.sji_info.pztx)[lwin]) else $
                             return,(*(self.sji_info.pztx)[lwin])[(self->locsji(lwin))[0]:(self->locsji(lwin))[1]]
  endif else return,(*(self.sji_info.pztx)[lwin])[indx]
end

function iris_data::getpzty,iwin,indx
  if n_elements(iwin) eq 0 then iwin=self.default_win
  if n_elements(indx) eq 0 then return,reform(*(self.aux_info.pzty)[self.mapping[iwin]]) $
  else return,(*(self.aux_info.pzty)[self.mapping[iwin]])[indx]
end

function iris_data::getpzty_sji,lwin,indx,all=all
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  if n_elements(indx) eq 0 then begin
    if keyword_set(all) then return,reform(*(self.sji_info.pzty)[lwin]) else $
                             return,(*(self.sji_info.pzty)[lwin])[(self->locsji())[0]:(self->locsji())[1]]
  endif else return,(*(self.sji_info.pzty)[lwin])[indx]
end

; time and temporal coordinates

function iris_data::getnexp,iwin
; return number of exposures in raster
  return,self->getnraster(iwin)
end

function iris_data::getnexp_sji,lwin
; return number of slit jaw exposures channel lwin
  if n_elements(lwin) eq 0 then return,self.nexpsji[self.default_sjlwin] else return,self.nexpsji[lwin]
end

function iris_data::getnexp_prp,iwin
  return,1 ; number of exposures per raster position, so far set to 1
end

function iris_data::getntime,iwin
  return,self->getnraster(iwin)
end

function iris_data::getexp,iexp,iwin=iwin
;returns exposure duration for exp-nr. iexp (optional) and iwin
  if n_elements(iwin) eq 0 then iwin=self.default_win
  m=self.mapping[iwin]
  case self->getregion(iwin) of
  'FUV': exp=*self.aux_info[m].exptimef
  'NUV': exp=*self.aux_info[m].exptimen
  else: begin
    message,'unknown region '+self->getregion(iwin),/info
    return,-1
        end
  endcase
  if n_elements(iexp) ne 0 then return,exp[iexp] else return,exp
end

function iris_data::getexp_sji,lwin,indx,all=all
;returns slit jaw image exposure duration channel lwin, exposure indx
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  if n_elements(indx) eq 0 then begin
    if keyword_set(all) then return,reform(*(self.sji_info.exptime)[lwin]) else $
                             return,(*(self.sji_info.exptime)[lwin])[(self->locsji())[0]:(self->locsji())[1]]
  endif else return,(*(self.sji_info.exptime)[lwin])[indx]
end

function iris_data::getdate_obs
;returns start date/time of raster
  return,self->getinfo('DATE_OBS')
end

function iris_data::getstartobs
;returns start date/time of OBS
  return,self->getinfo('STARTOBS')
end

function iris_data::ti2tai,ti
; returns time in TAI (seconds) of each exposure
  if n_elements(ti) eq 0 then ti=self->gettime()
  return,anytim2tai(self->getstartobs())+self->sec_from_obs_start(ti)
end

function iris_data::ti2utc,ti,time_only=time_only,date_only=date_only
; returns time in UTC of each exposure, /time_only and /date_only options
  if n_elements(ti) eq 0 then ti=self->gettime()
  return,anytim2utc(self->ti2tai(ti),time_only=time_only,date_only=date_only,/ccsds)
end

function iris_data::gettime,iwin,indx
;returns time in s after start of OBS for iwin and exp-nr. indx (optional)
  if n_elements(iwin) eq 0 then iwin=self.default_win
  if n_elements(indx) eq 0 then return,*(self.aux_info.time)[self.mapping[iwin]] $
  else return,(*(self.aux_info.time)[self.mapping[iwin]])[indx]
end

function iris_data::getti_1,iwin,indx
  return,self->gettime(iwin,indx)
end

function iris_data::getti_2,iwin,indx
  return,self->gettime(iwin,indx)+self->getexp(indx,iwin=iwin)
end

function iris_data::sec_from_obs_start,ti
  return,ti
end

function iris_data::gettime_sji,lwin,indx,all=all
;returns time in s after start of OBS for sji channel lwin and exp-nr. indx (optional)
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  if n_elements(indx) eq 0 then begin
    if keyword_set(all) then return,reform(*(self.sji_info.time)[lwin]) else $
                             return,(*(self.sji_info.time)[lwin])[(self->locsji(lwin))[0]:(self->locsji(lwin))[1]]
  endif else return,(*(self.sji_info.time)[lwin])[indx]
end

; i/o and related methods for loading data

pro iris_data::read,file,sjfile=sjfile,verbose=verbose
  if n_params() eq 0 then begin
    message,'iris_data->read,file1, file2, sjfile=sjfile',/info
    return
  end
  if n_elements(verbose) eq 0 then silent=1 else silent=0
  nfiles=n_elements(file)
  f = nfiles eq 1 ? [file]:file
  self.nfiles=nfiles
  self.filename=file[0]
  self.sji_read=0
  self->close
  for ifile=0,nfiles-1 do begin
    self.file[ifile]=f[ifile]
    if not (file_info(f[ifile])).exists then begin
      message,'file '+f[ifile]+' does not exist, ignoring it',/info
      break
    endif
; read first extension of each file, determine number of windows etc
;    d=readfits(f[ifile],hdr,exten_no=0,silent=silent)
    mrd_head,f[ifile],hdr,extension=0
    ;check iris_prep version used in this file, and if too old, print warning
    if ifile eq 0 then check_cal = iris_prep_version_check(f[ifile],/loud)
    
; say whether window is NUV, FUV, or SJI
    region=strtrim(strupcase(fxpar(hdr,'TDET1')),2) ; was CCDTYPE 
    if fxpar(hdr,'TEC1')-fxpar(hdr,'TSC1')+1 gt 2072 then region='CCD'
; 
; fill object data structure as appropriate
    case region of 
      'SJI': self->read_sji,d,hdr,f[ifile],silent=silent
;      'CCD': self->read_ccd,d,hdr,f[ifile],silent=silent
      else: self->read_lines,d,hdr,f[ifile],silent=silent
    endcase
;
  endfor
  self.nwin=total(self.regtot)
end

pro iris_data::read_sji,d,hdr,f,silent=silent
  sji_id=fxpar(hdr,'TDESC1')
; find out which slit jaw wavelength band is being read
  case strupcase(strtrim(sji_id,2)) of
    'SJI_1330': lwin=0
    'SJI_1400': lwin=1
    'SJI_2796': lwin=2
    'SJI_2832': lwin=3
    'SJI_1600W': lwin=4
    'SJI_5000W': lwin=5
    else: lwin=0
  endcase
  self.filename_sji[lwin]=f
  if self.default_sjiwin eq -1 then self.default_sjiwin = lwin
  self.sji_read[lwin]=1
  self.sji_id[lwin]=sji_id
  self.hdrsji[lwin]=ptr_new(hdr)
  self.naxis1sji[lwin]=fxpar(hdr,'NAXIS1')
  self.naxis2sji[lwin]=fxpar(hdr,'NAXIS2')
  self.naxis3sji[lwin]=fxpar(hdr,'NAXIS3')
;
  self.sji_info[lwin].xs=fxpar(hdr,'TSC1')-1
  self.sji_info[lwin].xw=fxpar(hdr,'TEC1')-fxpar(hdr,'TSC1')+1
  self.sji_info[lwin].ys=fxpar(hdr,'TSR1')-1
  self.sji_info[lwin].yw=fxpar(hdr,'TER1')-fxpar(hdr,'TSR1')+1
;
  a=readfits(f,h,exten_no=1,silent=silent)
  self.sji_info[lwin].time=ptr_new(reform(a[fxpar(h,'TIME'),*]))
  self.sji_info[lwin].pztx=ptr_new(reform(a[fxpar(h,'PZTX'),*]))
  self.sji_info[lwin].pzty=ptr_new(reform(a[fxpar(h,'PZTY'),*]))
  self.sji_info[lwin].exptime=ptr_new(reform(a[fxpar(h,'EXPTIMES'),*]))
  if fxpar(h,'SLIT') ne 0 then begin
    self.sji_info[lwin].slit=ptr_new(reform(a[fxpar(h,'SLIT'),*]))
    self.sji_info[lwin].sltpx1=ptr_new(-1)
    self.sji_info[lwin].sltpx2=ptr_new(-1)
  endif else begin
    self.sji_info[lwin].slit=ptr_new(-1)
    self.sji_info[lwin].sltpx1=ptr_new(reform(a[fxpar(h,'SLTPX1IX'),*]))
    self.sji_info[lwin].sltpx2=ptr_new(reform(a[fxpar(h,'SLTPX2IX'),*]))
   endelse
  self.sji_info[lwin].sumsptrs=ptr_new(reform(a[fxpar(h,'SUMSPTRS'),*]))
  self.sji_info[lwin].sumspats=ptr_new(reform(a[fxpar(h,'SUMSPATS'),*]))
  self.sji_info[lwin].dsrcs=ptr_new(reform(a[fxpar(h,'DSRCSIX'),*]))
  self.sji_info[lwin].lutids=ptr_new(reform(a[fxpar(h,'LUTIDS'),*]))
  self.sji_info[lwin].xcen=ptr_new(reform(a[fxpar(h,'XCENIX'),*]))
  self.sji_info[lwin].ycen=ptr_new(reform(a[fxpar(h,'YCENIX'),*]))
  self.sji_info[lwin].obs_vr=ptr_new(reform(a[fxpar(h,'OBS_VRIX'),*]))
  if fxpar(h,'OPHASEIX') ne 0 then begin
    self.sji_info[lwin].ophase=ptr_new(reform(a[fxpar(h,'OPHASEIX'),*]))
  endif else begin
    self.sji_info[lwin].ophase=ptr_new(-1)
  endelse
;
  a=readfits(f,h,exten_no=2,silent=silent)
  srow=fxpar(h,'FRMID')
  erow=fxpar(h,'LFRMID')+srow-1
  if(erow ge srow) then self.sjiobs_info[lwin].frmid = $
    ptr_new(string(a[srow:erow,*])) else self.sjiobs_info[lwin].frmid=ptr_new(' ')
  srow=fxpar(h,'FDBIDS')
  erow=fxpar(h,'LFDBIDS')+srow-1
  if(erow ge srow) then self.sjiobs_info[lwin].fdbids = $
    ptr_new(string(a[srow:erow,*])) else self.sjiobs_info[lwin].fdbids=ptr_new(' ')
  srow=fxpar(h,'CRSIDS')
  erow=fxpar(h,'LCRSIDS')+srow-1
  if(erow ge srow) then self.sjiobs_info[lwin].crsids = $
    ptr_new(string(a[srow:erow,*])) else self.sjiobs_info[lwin].crsids=ptr_new(' ')
  srow=fxpar(h,'FILES')
  erow=fxpar(h,'LFILES')+srow-1
  if(erow ge srow) then self.sjiobs_info[lwin].files = $
    ptr_new(string(a[srow:erow,*])) else self.sjiobs_info[lwin].files=ptr_new(' ')
;
  sub=where(self->gettime_sji(lwin,/all) ge min(self->gettime()) and $
            self->gettime_sji(lwin,/all) le max(self->gettime()))
  if sub[0] ne -1 then begin
    self.locsji[lwin,*]=[min(sub),max(sub)]
    self.nexpsji[lwin]=n_elements(sub)
  endif else begin
    message,'no slit jaws found within raster time range, using nearest in time',/info
    submin=min(abs(min(self->gettime())-self->gettime_sji(lwin,/all)),indxmin)
    submax=min(abs(max(self->gettime())-self->gettime_sji(lwin,/all)),indxmax)
    if submin lt submax then indx=indxmin else indx=indxmax
    self.locsji[lwin,*]=[indx,indx]
    self.nexpsji[lwin]=1
  endelse
  openr,lu,f,/swap_if_little_endian,/get_lun
  self.lusji[lwin]=lu
  position=iris_find_winpos(lu,0)
  naxis1=self.naxis1sji[lwin]
  naxis2=self.naxis2sji[lwin]
  case fxpar(hdr,'BITPIX') of 
    16: self.wsji[lwin] = $
        ptr_new(assoc(lu,intarr(naxis1,naxis2),position))
   -32: self.wsji[lwin] = $
        ptr_new(assoc(lu,fltarr(naxis1,naxis2),position))
      else: message,'unsupported datatype '+self->getdatatype()
  endcase
;  self.wsji[lwin]=ptr_new(d[*,*,min(sub):max(sub)])
end

pro iris_data::read_lines,d,hdr,f,ifile=ifile,silent=silent

    if n_elements(ifile) eq 0 then ifile=0
;
    uwin=fxpar(hdr,'NWIN')
    self.regtot[ifile]=uwin    
    if ifile eq 0 then lwin=0 else lwin=self.regtot[ifile-1]
    self.mapping[lwin:lwin+uwin-1]=ifile
;
    self.hdr[lwin]=ptr_new(hdr)
    self.region[lwin:lwin+uwin-1]=strtrim(strupcase(fxpar(hdr,'TDET*')),2)
;
    self.line_id[lwin:lwin+uwin-1]=fxpar(hdr,'TDESC*')
    self.win_info[lwin:lwin+uwin-1].xs=fxpar(hdr,'TSC*')-1
    self.win_info[lwin:lwin+uwin-1].xw=fxpar(hdr,'TEC*')-fxpar(hdr,'TSC*')+1
    self.win_info[lwin:lwin+uwin-1].ys=fxpar(hdr,'TSR*')-1
    self.win_info[lwin:lwin+uwin-1].yw=fxpar(hdr,'TER*')-fxpar(hdr,'TSR*')+1
; 
    self.line_wvl=self->getinfo('TWAVE*',0)
;
; find location of line windows in fits file
    openr,lu,f[ifile],/swap_if_little_endian,/get_lun
    self.lu=lu
    position=iris_find_winpos(lu,uwin)
    self.position[0:uwin]=position
;
; set up pointers via the assoc function to the windows in
; fits file
    for iext=1,uwin do begin
;      d=readfits(f[ifile],hdr,exten_no=iext,silent=silent)
;      print,iext,lwin,size(d)
;  extension headers, probably not too useful...except for debugging
      mrd_head,f[ifile],hdr,extension=iext
      case fxpar(hdr,'BITPIX') of 
       16: self.w[lwin] = $
           ptr_new(assoc(lu,intarr(self->getxw(lwin),self->getyw(lwin)),position[iext]))
      -32: self.w[lwin] = $
           ptr_new(assoc(lu,fltarr(self->getxw(lwin),self->getyw(lwin)),position[iext]))
      else: message,'unsupported datatype '+self->getdatatype()
      endcase
      if self.default_win eq -1 then self.default_win = lwin
      self.naxis1[lwin]=fxpar(hdr,'NAXIS1')
      self.naxis2[lwin]=fxpar(hdr,'NAXIS2')      
      self.naxis3[lwin]=fxpar(hdr,'NAXIS3')
      self.hdr[iext]=ptr_new(hdr)
;
      lwin=lwin+1
    endfor
; add virtual window in cases where region is split between FUV1 and
; FUV2, ie (usually) full CCD windows
    if ifile eq 0 then lwin=0 else lwin=self.regtot[ifile-1]
    for i=lwin,lwin+uwin-1 do begin
      ccdsize = 2072 / self->binning_spectral(i)
      if self.win_info[i].xs lt ccdsize and $
         self.win_info[i].xs+self.win_info[i].xw ge ccdsize then begin
        self.mapping[i+1:lwin+uwin]=shift(self.mapping[i+1:lwin+uwin],1)
        self.region[i+1:lwin+uwin]=shift(self.region[i+1:lwin+uwin],1)
        self.line_id[i+1:lwin+uwin]=shift(self.line_id[i+1:lwin+uwin],1)
        self.line_wvl[i+1:lwin+uwin]=shift(self.line_wvl[i+1:lwin+uwin],1)
        self.win_info[i+1:lwin+uwin].xs=shift(self.win_info[i+1:lwin+uwin].xs,1)
        self.win_info[i+1:lwin+uwin].xw=shift(self.win_info[i+1:lwin+uwin].xw,1)
        self.win_info[i+1:lwin+uwin].ys=shift(self.win_info[i+1:lwin+uwin].ys,1)
        self.win_info[i+1:lwin+uwin].yw=shift(self.win_info[i+1:lwin+uwin].yw,1)
        self.w[i+1:lwin+uwin]=shift(self.w[i+1:lwin+uwin],1)
;
        self.mapping[i+1]=self.mapping[i]
        self.region[i]='FUV1'
        self.region[i+1]='FUV2'
        self.line_id[i]='FULL CCD FUV1'
        self.line_id[i+1]='FULL CCD FUV2'
        self.win_info[i+1].xs=ccdsize
        self.win_info[i+1].xw=self.win_info[i].xw-(ccdsize-self.win_info[i].xs)
        self.win_info[i].xw=ccdsize-self.win_info[i].xs
        self.win_info[i+1].ys=self.win_info[i].ys
        self.win_info[i+1].yw=self.win_info[i].yw
;
        xs=self.win_info[i].xs
        xw=self.win_info[i].xw
        lam_fuv1=((self->getcal())->lambda_fuv1())[xs:xs+xw-1]
        xs=self.win_info[i+1].xs
        xw=self.win_info[i+1].xw
        lam_fuv2=((self->getcal())->lambda_fuv2())[xs-ccdsize:xs-ccdsize+xw-1]
        self.line_wvl[i]=mean(lam_fuv1)
        self.line_wvl[i+1]=mean(lam_fuv2)
;
        self.w[i+1]=self.w[i]
;
        uwin=uwin+1
      endif
   endfor
   self.regtot[ifile]=uwin
; read auxilary data contained in last two extensions of each file
   d=readfits(f[ifile],hdr,exten_no=uwin+1,silent=silent)
   self.aux_info[ifile].time=ptr_new(reform(d[fxpar(hdr,'TIME'),*]))
   self.aux_info[ifile].pztx=ptr_new(reform(d[fxpar(hdr,'PZTX'),*]))
   self.aux_info[ifile].pzty=ptr_new(reform(d[fxpar(hdr,'PZTY'),*]))
   self.aux_info[ifile].exptimef=ptr_new(reform(d[fxpar(hdr,'EXPTIMEF'),*]))
   self.aux_info[ifile].exptimen=ptr_new(reform(d[fxpar(hdr,'EXPTIMEN'),*]))
   self.aux_info[ifile].sumsptrf=ptr_new(reform(d[fxpar(hdr,'SUMSPTRF'),*]))
   self.aux_info[ifile].sumsptrn=ptr_new(reform(d[fxpar(hdr,'SUMSPTRN'),*]))
   self.aux_info[ifile].sumspatf=ptr_new(reform(d[fxpar(hdr,'SUMSPATF'),*]))
   self.aux_info[ifile].sumspatn=ptr_new(reform(d[fxpar(hdr,'SUMSPATN'),*]))
   self.aux_info[ifile].dsrcf=ptr_new(reform(d[fxpar(hdr,'DSRCFIX'),*]))
   self.aux_info[ifile].dsrcn=ptr_new(reform(d[fxpar(hdr,'DSRCNIX'),*]))
   self.aux_info[ifile].lutidf=ptr_new(reform(d[fxpar(hdr,'LUTIDF'),*]))
   self.aux_info[ifile].lutidn=ptr_new(reform(d[fxpar(hdr,'LUTIDN'),*]))
   self.aux_info[ifile].xcen=ptr_new(reform(d[fxpar(hdr,'XCENIX'),*]))
   self.aux_info[ifile].ycen=ptr_new(reform(d[fxpar(hdr,'YCENIX'),*]))
   ix=fxpar(hdr,'OBS_VRIX')
   if fxpar(hdr,'YCENIX') eq fxpar(hdr,'OBS_VRIX') then ix=ix+1
   self.aux_info[ifile].obs_vr=ptr_new(reform(d[ix,*]))
   self.aux_info[ifile].ophase=ptr_new(reform(d[fxpar(hdr,'OPHASEIX'),*]))
; frmid, fdbid, crsid, etc in exten_no=uwin+2, not implemented yet
   d=readfits(f[ifile],hdr,exten_no=uwin+2,silent=silent)
   srow=fxpar(hdr,'FRMID')
   erow=fxpar(hdr,'LFRMID')+srow-1
   if(erow ge srow) then self.obs_info[ifile].frmid=ptr_new(string(d[srow:erow,*])) else self.obs_info[ifile].frmid=ptr_new(' ')
   srow=fxpar(hdr,'FDBIDF')
   erow=fxpar(hdr,'LFDBIDF')+srow-1
   if(erow ge srow) then self.obs_info[ifile].fdbidf=ptr_new(string(d[srow:erow,*])) else self.obs_info[ifile].fdbidf=ptr_new(' ')
   srow=fxpar(hdr,'CRSIDF')
   erow=fxpar(hdr,'LCRSIDF')+srow-1
   if(erow ge srow) then self.obs_info[ifile].crsidf=ptr_new(string(d[srow:erow,*])) else self.obs_info[ifile].crsidf=ptr_new(' ')
   srow=fxpar(hdr,'FDBIDN')
   erow=fxpar(hdr,'LFDBIDN')+srow-1
   if(erow ge srow) then self.obs_info[ifile].fdbidn=ptr_new(string(d[srow:erow,*])) else self.obs_info[ifile].fdbidn=ptr_new(' ')
   srow=fxpar(hdr,'CRSIDN')
   erow=fxpar(hdr,'LCRSIDN')+srow-1
   if(erow ge srow) then self.obs_info[ifile].crsidn=ptr_new(string(d[srow:erow,*])) else self.obs_info[ifile].crsidn=ptr_new(' ')
   srow=fxpar(hdr,'FILEF')
   erow=fxpar(hdr,'LFILEF')+srow-1
   if(erow ge srow) then self.obs_info[ifile].filef=ptr_new(string(d[srow:erow,*])) else self.obs_info[ifile].filef=ptr_new(' ')
   srow=fxpar(hdr,'FILEN')
   erow=fxpar(hdr,'LFILEN')+srow-1
   if(erow ge srow) then self.obs_info[ifile].filen=ptr_new(string(d[srow:erow,*])) else self.obs_info[ifile].filen=ptr_new(' ')
end

pro iris_data__define           
mfile=10
mwin=25
nsji=6
wpos=create_struct(name='win_info','xs',0,'xw',0,'ys',0,'yw',0)
obsinf=create_struct(name='obs_info','frmid',ptr_new(), $
                                     'fdbidf',ptr_new(),'crsidf',ptr_new(), $
                                     'fdbidn',ptr_new(),'crsidn',ptr_new(), $
                                     'filef',ptr_new(),'filen',ptr_new())
sjiobsinf=create_struct(name='sjiobs_info','frmid',ptr_new(), $
                                     'fdbids',ptr_new(),'crsids',ptr_new(), $
                                     'files',ptr_new())
auxinf=create_struct(name='aux_info','time',ptr_new(), $
                                     'pztx',ptr_new(),'pzty',ptr_new(), $
                                     'exptimef',ptr_new(),'exptimen',ptr_new(), $
                                     'sumsptrf',ptr_new(),'sumsptrn',ptr_new(), $
                                     'sumspatf',ptr_new(),'sumspatn',ptr_new(), $
                                     'dsrcf',ptr_new(),'dsrcn',ptr_new(), $
                                     'lutidf',ptr_new(),'lutidn',ptr_new(), $
                                     'xcen',ptr_new(),'ycen',ptr_new(), $
                                     'obs_vr',ptr_new(),'ophase',ptr_new())
sjiinf=create_struct(name='sji_info','xs',0,'xw',0,'ys',0,'yw',0,'time',ptr_new(), $
                                     'pztx',ptr_new(),'pzty',ptr_new(), $
                                     'exptime',ptr_new(),'slit',ptr_new(), $
                                     'sumsptrs',ptr_new(),'sumspats',ptr_new(), $
                                     'dsrcs',ptr_new(),'lutids',ptr_new(), $
                                     'xcen',ptr_new(),'ycen',ptr_new(), $
                                     'sltpx1',ptr_new(),'sltpx2',ptr_new(), $
                                     'obs_vr',ptr_new(),'ophase',ptr_new())
wdstruct=create_struct(name = 'wd_def',  'line_px', intarr(2), $
                           'cont_px', intarr(2))
struct={iris_data, title: '  ', $
                 comment:'', $
                 filename:'', $
                 lu:0,$
                 lusji:intarr(nsji), $
                 ver_no: 0, $
                 aux:ptr_new(), $
                 cal:ptr_new(), $
                 iiobslid:'',$
                 nwin: 0, $
                 default_win:0,$
                 regtot: intarr(mfile), $
                 xcen: -9999., $
                 ycen: -9999., $
                 fovx: 0.0, $
                 fovy: 0.0, $
                 file: strarr(mfile), $
                 nfiles: 0, $
                 region: strarr(mwin), $
                 mapping: intarr(mwin), $
                 line_id: strarr(mwin), $
                 naxis1: intarr(mwin), $
                 naxis2: intarr(mwin), $
                 naxis3: intarr(mwin), $
                 position: lon64arr(mwin+1), $ ;position[0] == position of main data cube == NULL NE region 1 data in spectra
                 hdr: ptrarr(mwin+1),$ ;hdr[0] == mainheader NE region 1 header in spectra
                 w: ptrarr(mwin),$
                 win_info:replicate({win_info},mwin), $
                 default_sjiwin:-1,$
                 line_wvl: fltarr(mwin), $
                 aux_info:replicate({aux_info},mfile), $
                 obs_info:replicate({obs_info},mfile), $
                 wd_def:replicate({wd_def}, mwin), $
                 filename_sji:strarr(nsji),$
                 sji_iiobslid:'',$
                 sji_id: strarr(nsji), $
                 naxis1sji: intarr(nsji), $
                 naxis2sji: intarr(nsji), $
                 naxis3sji: intarr(nsji), $
                 nexpsji: intarr(nsji), $
                 locsji: intarr(nsji,2), $
                 hdrsji: ptrarr(nsji),$
                 wsji: ptrarr(nsji),$
                 sji_info: replicate({sji_info},nsji),$
                 sjiobs_info: replicate({sjiobs_info},nsji),$
                 sji_read: intarr(nsji)$
           }
end

