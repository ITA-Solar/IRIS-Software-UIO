; 
;+
; NAME:
;       IRIS_SJI__DEFINE
;
; PURPOSE:
;       iris_sji__define defines the class structure 'iris_sji'.
;       this object is made to look at a set of iris slit jaw 
;       images
;
; CATEGORY:
;       IRIS Data analysis SW
;
; CALLING SEQUENCE:
;       The IRIS_SJI__DEFINE procedure is not called directly. An
;       object of class IRIS_SJI is created with the following
;       statement:
;                   iris_sji = obj_new('iris_sji')
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;       Objects of type IRIS_SJI which describes and contains 
;       level 2 IRIS SJI images
;
; CALLS:
;
; COMMON BLOCKS:
;
; PROCEDURE:
;       The procedure opens an object of class IRIS_SJI. 
;       This procedure includes various functions (methods of
;       class  'iris_sji' whose purpose is to get and/or manipulate
;       the different fields of the object.
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       13-Apr-2013: Viggo Hansteen
;       17-Nov-2015, Peter Young
;         - modified ximovie method to allow sub-regions to be input.
;       06-Oct-2016: Viggo Hansteen, change bscale and bzero in hdr 
;           when descaling, added setvar and sethdrmethods
;
; $Id: iris_sji__define.pro,v 1.87 2017/05/22 09:23:23 mawiesma Exp $
;
;-
function iris_sji::init,file,verbose=verbose
  self.title='IRIS SJI Data'
  self.xcen = 0.
  self.ycen = 0.
;
  self.default_sjiwin=-1
  self.aux=ptr_new(obj_new('iris_aux'))
  self.cal=ptr_new(obj_new('iris_cal'))
  if n_elements(file) ne 0 then begin
    self->read,file,verbose=verbose
  endif
  return,1
end

pro iris_sji::close
; frees pointer to main data array "w" and closes all associated files
  for i=0,self.nwin-1 do begin
    ptr_free,self.w[i]
    ptr_free,self.hdr[i]
  endfor
  for lwin=0,n_elements(self.lu)-1 do begin
    if self.lu[lwin] ge 100 and self.lu[lwin] le 128 then free_lun,self.lu[lwin]
 endfor
end

pro iris_sji::cleanup
; called by obj_destroy, frees all pointers and closes all associated files
  if ptr_valid(self.aux) then begin
    obj_destroy,*self.aux
    ptr_free,self.aux
  endif
  if ptr_valid(self.cal) then begin
    obj_destroy,*self.cal
    ptr_free,self.cal
  endif
  self->close
  for i=0,self.nfiles-1 do begin
    ptr_free,self.sji_info[i].time
    ptr_free,self.sji_info[i].pztx
    ptr_free,self.sji_info[i].pzty
    ptr_free,self.sji_info[i].exptime
    ptr_free,self.sji_info[i].sumsptrs
    ptr_free,self.sji_info[i].sumspats
    ptr_free,self.sji_info[i].dsrcs
    ptr_free,self.sji_info[i].lutids
    ptr_free,self.sji_info[i].xcen
    ptr_free,self.sji_info[i].ycen
    ptr_free,self.sji_info[i].obs_vr
    ptr_free,self.sji_info[i].ophase
;
    ptr_free,self.sjiobs_info[i].frmid
    ptr_free,self.sjiobs_info[i].fdbids
    ptr_free,self.sjiobs_info[i].crsids
    ptr_free,self.sjiobs_info[i].files    
  endfor
  return
end

pro iris_sji::help, description=description
;prints out this help, setting the 'description' keyword will also print the header info
  if arg_present(description) || keyword_set(description) then $
    obj_help,self, description=description $
  else $
    obj_help,self
end

function iris_sji::gettitle
  return,self.title
end

function iris_sji::getfilename,iwin
;returns filename
  if n_elements(iwin) ne 0 then return,self.file[iwin] else return,self.file
end

pro iris_sji::show_sji
;prints information about sji windows
  for lwin=0,5 do begin
    if self->getread_sji(lwin) then begin
     print,lwin,self->getsji_id(lwin),format='(i2,3x,a)'
    endif
  endfor
end

function iris_sji::getread_sji,lwin
  if n_elements(lwin) eq 0 then return,self.sji_read else return,self.sji_read[lwin]
end

function iris_sji::getcomment
  return, self.comment
end

pro iris_sji::setcomment,comment
  self.comment=comment
end

function iris_sji::getaux
;returns the aux data as an iris_aux object
  return,*self.aux
end

function iris_sji::getcal
;returns IRIS specific parameters as an iris_cal object
  return,*self.cal
end

function iris_sji::missing
;returns the missing value
  bscale=self->getinfo('O_BSCALE')
  if datatype(bscale) eq 'INT'and bscale eq 0 then begin
    bscale=self->getinfo('BSCALE')
    if datatype(bscale) eq 'INT'and bscale eq 0 then bscale=1.0
  endif
  bzero=self->getinfo('O_BZERO')
  if datatype(bzero) eq 'INT'and bzero eq 0 then begin
    bzero=self->getinfo('BZERO')
    if datatype(bzero) eq 'INT'and bzero eq 0 then bzero=0.0
  endif
  return,bscale*((*self.cal)->missing())+bzero
end

function iris_sji::getxytitle,axis
  return,*self.aux->getxytitle(axis)
end

function iris_sji::getvariablename
  return,*self.aux->getvariablename()
end

function iris_sji::getdatatype
;returns datatype
  case self->getinfo('BITPIX') of
      8: return,'byte'
     16: return,'int'
    -32: return,'float'
    -64: return,'double'
    else: return,'float'
  endcase
end

function iris_sji::sji_info,lwin
;returns auxiliary information about sji in a structure
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  sji_inf={time:*(self.sji_info[lwin].time), $
           pztx:*(self.sji_info[lwin].pztx), $
           pzty:*(self.sji_info[lwin].pzty), $
           exptimes:*(self.sji_info[lwin].exptime), $
           slit:*(self.sji_info[lwin].slit), $
           sltpx1:*(self.sji_info[lwin].sltpx1), $
           sltpx2:*(self.sji_info[lwin].sltpx2), $
           xcen:*(self.sji_info[lwin].xcen), $
           ycen:*(self.sji_info[lwin].ycen), $
           sumsptrs:*(self.sji_info[lwin].sumsptrs), $
           sumspats:*(self.sji_info[lwin].sumspats), $
           dsrcs:*(self.sji_info[lwin].dsrcs), $
           lutids:*(self.sji_info[lwin].lutids), $
           obs_vr:*(self.sji_info[lwin].obs_vr), $
           ophase:*(self.sji_info[lwin].ophase) $
           }
  return,sji_inf
end

function iris_sji::obs_info_sji,lwin
;returns info from auxiliary extension 2
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  obs_inf={frmid:*(self.sjiobs_info[lwin].frmid), $
           fdbids:*(self.sjiobs_info[lwin].fdbids), $
           crsids:*(self.sjiobs_info[lwin].crsids), $
           files:*(self.sjiobs_info[lwin].files)}
  return,obs_inf
end

function iris_sji::getlam,iwin
  if n_params() eq 0 then begin
    message,'no window nr input',/info
    iwin=-1
  endif
  iwin=(self->getwindx(iwin))[0]
  if iwin eq -1 then return,-1
  xs=(self->getxs())[iwin]
  xw=(self->getxw())[iwin]
  return,(self->getlambda(self->getregion(iwin),wscale='AA'))[xs:xs+xw-1]
end

pro iris_sji::getwin,iwin,wd,pos
; get window iwin, into wd, position pos on ccd
  wd=*self.w[iwin]
  pos=[self->getxs(iwin), self->getxw(iwin), self->getys(iwin), self->getyw(iwin)]
  return
end

function iris_sji::getvar,lwin,noload=noload
; extract sji data or assoc pointer to file with option /noload
  if n_elements(noload) eq 0 then noload=0
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  if ptr_valid(self.w[lwin]) then begin
    if not noload then begin
       w=fltarr(self->getnaxis1(lwin),self->getnaxis2(lwin),self->getnexp())
       for i=0,self->getnexp()-1 do $
         w[*,*,i]=self->descale_array((*(self.w[lwin]))[*,*,i])
       hdr=self->gethdr(lwin)
       sxaddpar,hdr,'BSCALE',1.0
       sxaddpar,hdr,'BZERO',0.0
       *(self.hdr[lwin])=hdr
       return,w 
    endif else return,*self.w[lwin]
  endif else return,-1
end

pro iris_sji::sethdr,hdr,lwin
; populate object hdr [lwin or default]
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  *(self.hdr[lwin])=hdr
end

pro iris_sji::setvar,w,lwin
; populate object data [lwin or default]
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  *self.w[lwin]=w
end

function iris_sji::getdata
; extract entire object into structure
  s=self->gethdr(/struct)
  for lwin=0,3 do begin
    d=self->getvar(lwin)
    if d[0] ne -1 then begin
      s=boost_tag(temporary(s),d,self->getsji_id(lwin))
      label=(strsplit(self->getsji_id(lwin),'_',/extract))[1]
      s=boost_tag(temporary(s),reform(self->getpztx(lwin)),'pztx'+label)
      s=boost_tag(temporary(s),reform(self->getpzty(lwin)),'pzty'+label)
      s=boost_tag(temporary(s),reform(self->getexp(iwin=lwin)),'exp'+label)
      s=boost_tag(temporary(s),reform(self->gettime(lwin)),'time'+label)
    endif
  endfor
  return,s
end

function iris_sji::gethdr,iwin,struct=struct
;returns header of iwin, if struct is set then structure is returned
  if n_elements(iwin) eq 0 then begin
    iwin=self.default_sjiwin
  endif else begin
    if ~ptr_valid(self.hdr[iwin]) then begin
      print,'invalid index (iwin), valid indices are:'
      self->show_sji
      return, -1
    endif
  endelse
  if n_elements(struct) eq 0 then struct=0
  if struct then return,fitshead2struct(*(self.hdr[iwin])) else return,*(self.hdr[iwin])
end

function iris_sji::getnwin
  return,self.nwin
end

function iris_sji::getpos,iwin
  if n_elements(iwin) eq 0 then iwin=self.default_sjiwin
  return,{xs:self->getxs(iwin),xw:self->getxw(iwin),ys:self->getys(iwin),yw:self->getyw(iwin)}
end

function iris_sji::getnaxis1,iwin
  if n_elements(iwin) eq 0 then iwin=self.default_sjiwin
  return,self.naxis1[iwin]
end

function iris_sji::getnaxis2,iwin
  if n_elements(iwin) eq 0 then iwin=self.default_sjiwin
  return,self.naxis2[iwin]
end

function iris_sji::getxw,iwin
  nwin=self->getnwin()
  if n_params() eq 0 then begin
    return,(self.sji_info.xw)[0:nwin-1]
  endif else begin
    return,self.sji_info[iwin].xw
  endelse
end

function iris_sji::getyw,iwin
  nwin=self->getnwin()
  if n_params() eq 0 then begin
    return,(self.sji_info.yw)[0:nwin-1]
  endif else begin
    return,self.sji_info[iwin].yw
  endelse
end

function iris_sji::getxs,iwin
  nwin=self->getnwin()
  if n_params() eq 0 then begin
    return,(self.sji_info.xs)[0:nwin-1]
  endif else begin
    return,self.sji_info[iwin].xs
  endelse
end

function iris_sji::getys,iwin
  nwin=self->getnwin()
  if n_params() eq 0 then begin
    return,(self.sji_info.ys)[0:nwin-1]
  endif else begin
    return,self.sji_info[iwin].ys
  endelse
end

function iris_sji::getnslit,iwin
  nwin=self->getnwin()
  if n_params() eq 0 then begin
    return,(self.naxis2)[0:nwin-1]
  endif else begin
    return,self.naxis2[iwin]
  endelse
end

function iris_sji::getnraster,lwin
  if n_params() eq 0 then begin
    return,(self.naxis3)
  endif else begin
    lwin=self.default_sjiwin
    return,self.naxis3[lwin]
  endelse
end

function iris_sji::getnexp,iwin
  return,self->getnraster(iwin)
end

function iris_sji::getntime,iwin
  return,self->getnraster(iwin)
end

function iris_sji::lwin_read
  return,self.lwin
end

function iris_sji::getsji_id,iwin
  nwin=4
  if n_params() eq 0 then begin
    return,(self.sji_id)[0:nwin-1]
  endif else begin
    return,self.sji_id[iwin]
  endelse
end

function iris_sji::find_slitpos0,iwin
  if n_elements(iwin) eq 0 then begin
    message,'no slit jaw index given, assuming '+string(self.default_sjiwin,format='(I1)'),/info
    iwin=self.default_sjiwin
  endif
  case strupcase(strtrim(self->getsji_id(iwin),2)) of
    'FUV': begin
       slitxs0=((self->getcal())->getsji_slitpos()).fuv_xs0
       slitys0=((self->getcal())->getsji_slitpos()).fuv_ys0
       return,{xs0:slitxs0-(self->getxs_sji(iwin)),ys0:slitys0-(self->getys_sji(iwin))}
           end
    'NUV': begin
       slitxs0=((self->getcal())->getsji_slitpos()).nuv_xs0
       slitys0=((self->getcal())->getsji_slitpos()).nuv_ys0
       return,{xs0:slitxs0-(self->getxs_sji(iwin)),ys0:slitys0-(self->getys_sji(iwin))}
           end
    else: begin
       message,'SJI ID: '+strupcase(strtrim(self->getsji_id(iwin),2))+' is unknown, assuming FUV',/info
       slitxs0=((self->getcal())->getsji_slitpos()).fuv_xs0
       slitys0=((self->getcal())->getsji_slitpos()).fuv_ys0
       return,{xs0:slitxs0-(self->getxs_sji(iwin)),ys0:slitys0-(self->getys_sji(iwin))}
           end
  endcase
end

function iris_sji::getcrsid,iwin
  nwin=self->getnwin()
  if n_params() eq 0 then begin
    return,self.crsid[self.mapping[0:nwin-1]]
  endif else begin
    return,self.crsid[self.mapping[iwin]]
  endelse
end

function iris_sji::getobsid,iwin
  nwin=self->getnwin()
  if n_params() eq 0 then begin
    return,self.obsid[self.mapping[0:nwin-1]]
  endif else begin
    return,self.obsid[self.mapping[iwin]]
  endelse
end

function iris_sji::getfdbid,iwin
  nwin=self->getnwin()
  if n_params() eq 0 then begin
    return,self.fdbid[self.mapping[0:nwin-1]]
  endif else begin
    return,self.fdbid[self.mapping[iwin]]
  endelse
end

function iris_sji::getinfo,tag,iwin,missing=missing,found=found,_extra=_extra
;returns tag from the header
  return,(gt_tagval(self->gethdr(iwin),tag,missing=missing,found=found,_extra=_extra))[0]
end

; time and temporal coordinates

function iris_sji::gettime,iwin,indx
;returns the time of indx
  if n_elements(iwin) eq 0 then iwin=self.default_sjiwin
  if n_elements(indx) eq 0 then return,*(self.sji_info.time)[iwin] $
  else return,(*((self.sji_info)[iwin]).time)[indx]
end

function iris_sji::getdate_obs
  return,self->getinfo('DATE_OBS')
end

function iris_sji::getstartobs
;returns start time of the OBS
  return,self->getinfo('STARTOBS')
end

function iris_sji::ti2tai,ti
  if n_elements(ti) eq 0 then ti=self->gettime()
  return,anytim2tai(self->getstartobs())+ti
end

function iris_sji::ti2utc,ti,time_only=time_only
  if n_elements(ti) eq 0 then ti=self->gettime()
  return,anytim2utc(self->ti2tai(ti),time_only=time_only,/ccsds)
end

function iris_sji::getexp,iexp,iwin=iwin
  if n_elements(iwin) eq 0 then iwin=self.default_sjiwin
  exp=*self.sji_info[iwin].exptime
  if n_params() gt 0 then return,exp[iexp] else return,exp
end

; positions and spatial coordinates

function iris_sji::xscale,lwin
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  cdelt1=fxpar(self->gethdr(lwin),'CDELT1')
  crval1=fxpar(self->gethdr(lwin),'CRVAL1')
  crpix1=fxpar(self->gethdr(lwin),'CRPIX1')
  return,crval1+(findgen(self->getnaxis1(lwin))+1.0-crpix1)*cdelt1
end

function iris_sji::yscale,lwin
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  cdelt2=fxpar(self->gethdr(lwin),'CDELT2')
  crval2=fxpar(self->gethdr(lwin),'CRVAL2')
  crpix2=fxpar(self->gethdr(lwin),'CRPIX2')
  return,crval2+(findgen(self->getnaxis2(lwin))+1.0-crpix2)*cdelt2
end

function iris_sji::datamin,lwin
  ;returns DATAP01
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  dmin=self->getinfo('DATAP01')
  case datatype(dmin) of
    'STR': begin
      message,tag+' keyword set to NAN, setting datamin to 0',/info
      return,0
    endcase
    else: return,dmin
  endcase
end

function iris_sji::getxpos,iwin
  if n_params() eq 0 then begin
    iwin=self.default_sjiwin
  endif
  return,self->getxcen()+self->getpztx(iwin)
end

function iris_sji::getypos,iwin
  if n_params() eq 0 then begin
    iwin=self.default_sjiwin
  endif
  return,self->getycen()+indgen(self->getyw(iwin))*self->getresy()
end

function iris_sji::getxcen
  return,mean(self->xscale())
end

function iris_sji::getycen
  return,mean(self->yscale())
end

function iris_sji::getfovx,iwin
  if n_params() eq 0 then begin
    iwin=self.default_sjiwin
  endif
  return,self->getxw(iwin)*self->getresx(iwin)
end

function iris_sji::getfovy,iwin
  if n_params() eq 0 then begin
    return,self->getyw()*self->getresy()
  endif else begin
    return,self.getyw(iwin)*self->getresy(iwin)
  endelse
end

function iris_sji::getresx,iwin
  if n_elements(iwin) eq 0 then iwin=self.default_sjiwin
  cdelt1=fxpar(self->gethdr(iwin),'CDELT1')
  if cdelt1 eq 0 then begin
    message,'No cdelt1 found in header, using default resolution',/info
    ib=where(self->getsji_id(iwin) eq (self->getcal())->getid())
    return,*(self.cal)->getresx(ib)
  endif
  return,cdelt1
end

function iris_sji::getresy,iwin
  if n_elements(iwin) eq 0 then iwin=self.default_sjiwin
  cdelt2=fxpar(self->gethdr(iwin),'CDELT2')
  if cdelt2 eq 0 then begin
    message,'No cdelt2 found in header, using default resolution',/info
    return,*(self.cal)->getresy()
  endif
  return,cdelt2
end

function iris_sji::getpztx,iwin,indx=indx
  if n_elements(iwin) eq 0 then iwin=self.default_sjiwin
  if n_elements(indx) eq 0 then return,*(self.sji_info.pztx)[iwin] $
  else return,(*(self.sji_info.pztx)[iwin])[indx]
end

function iris_sji::getpzty,iwin,indx
  if n_elements(iwin) eq 0 then iwin=self.default_sjiwin
  if n_elements(indx) eq 0 then return,*(self.sji_info.pzty)[iwin] $
  else return,(*(self.sji_info.pzty)[iwin])[indx]
end

function iris_sji::getslit,iwin,indx=indx
  if n_elements(iwin) eq 0 then iwin=self.default_sjiwin
  if n_elements(indx) eq 0 then return,*(self.sji_info.slit)[iwin] $
  else return,(*(self.sji_info.slit)[iwin])[indx]
end

pro iris_sji::ximovie,iwin,normexp=normexp,dither=dither,log=log,unslit=unslit, $
                      group_leader=group_leader,  $  
                      offset=offset, pos=pos,  $   ; PRY, 17-Nov-2015 added keywords
                      magnification=magnification, time=time, imin=imin, imax=imax, $
                      first_im=first_im, last_im=last_im, start_im=start_im
;opens a window with a movie of the sji data
  forward_function iris_dither_undust
  if n_elements(normexp) eq 0 then normexp=0
  if n_elements(dither) eq 0 then dither=0
  if n_elements(unslit) eq 0 then unslit=0
  if n_elements(log) eq 0 then log=0
  if n_elements(iwin) eq 0 then iwin=self.default_sjiwin
  minsize=400.0
  maxsize=800.0
;
; PRY, 17-Nov-2015
; Find size of image
;   - POS is used if it has been input
  IF n_elements(pos) EQ 0 THEN BEGIN 
    xsize=self->getnaxis1(iwin[0])
    ysize=self->getyw(iwin[0])
  ENDIF ELSE BEGIN
    xsize=pos[1]-pos[0]
    ysize=pos[3]-pos[2]
  ENDELSE 
;
; PRY, 17-Nov-2015
; Find magnification automatically
;   - if magnification has been supplied as input then this code is ignored.
  IF n_elements(magnification) EQ 0 THEN BEGIN
    magnification=1.0
    if xsize lt minsize then magnification=minsize/xsize
    if xsize gt maxsize then magnification=maxsize/xsize
    if ysize*magnification lt minsize then magnification=minsize/ysize
    if ysize*magnification gt maxsize then magnification=maxsize/ysize
  ENDIF 
; if normexp normalize exposures and write temp file 
  if normexp then begin
    im=self->getvar(iwin[0])
    if (size(im))[0] eq 3 then $
      nt=(size(im))[3] $
    else nt=1
    for it=0,nt-1 do begin
      im[*,*,it]=im[*,*,it]/self->getexp(it,iwin=iwin[0])
    endfor
    if dither then begin
      for it=0,nt-1 do begin
        im[*,*,it]=iris_dither_undust(im[*,*,it], im[*,*,(it+1)<(nt-1)],unslit=unslit)
      endfor
    endif
    im_max=mean(im)*4.
;    im=iris_histo_opt(im)
    if log then im=alog10(im>0.1)
; construct temp file name
    ct=0
    repeat begin
      ct=ct+1
      assoc_file = IRISxfiles_appReadme()+'/iris_sji_ximovie_'+strtrim(string(ct),2)+'.tmp'
    endrep until ((findfile(assoc_file))[0] eq '')
    if ct gt 99 then begin
      message,'more than 100 temporary assoc files stored in',/info
      message,IRISxfiles_appReadme()+'/iris_sji_ximovie_XX.tmp. Consider purge!',/info
    endif
;
    openw,lu,assoc_file,/get_lun                                               
    rec=assoc(lu,im)                                                          
    rec[0]=im                                                         
    close,lu
    free_lun,lu
    iris_ximovie,assoc_file,xsize,ysize,/float,title=self->getfilename(iwin[0]), $
         nframes=self->getntime(iwin[0]),group_leader=group_leader, $
                 time=self->ti2utc(),/clock,missing=self->missing(), $
                      first_im=first_im, last_im=last_im, start_im=start_im, $
                 pos=pos, offset=offset, imin=imin, imax=imax, $    ; PRY, 17-Nov-2015
         magnification=magnification,/fdelete ; delete file when closing ximovie session

;else if an image has been aligned use current data and construct temp file
  endif else if ptr_valid(self.align_dxy[iwin]) then begin
    ct=-1
    repeat begin
      ct=ct+1
      assoc_file = IRISxfiles_appReadme()+'/iris_sji_ximovie_'+strtrim(string(ct),2)+'.tmp'
    endrep until ((findfile(assoc_file))[0] eq '')
    if ct gt 99 then begin
      message,'more than 100 temporary assoc files stored in',/info
      message,IRISxfiles_appReadme()+'/iris_sji_ximovie_XX.tmp. Consider purge!',/info
    endif
;
    openw,lu,assoc_file,/get_lun                                               
    rec=assoc(lu,self->getvar(iwin[0]))                                                          
    rec[0]=self->getvar(iwin[0])                                                         
    close,lu
    free_lun,lu
    iris_ximovie,assoc_file,xsize,ysize,/float,title=self->getfilename(iwin[0]), $
         nframes=self->getntime(iwin[0]),group_leader=group_leader, $
         time=self->ti2utc(),/clock,missing=self->missing(), $
                      first_im=first_im, last_im=last_im, start_im=start_im, $
                 pos=pos, offset=offset, imin=imin, imax=imax, $    ; PRY, 17-Nov-2015
         magnification=magnification,/fdelete ; delete file when closing ximovie session

; else ximovie uses fits file directly
  endif else begin 
    nmax=n_elements(self->gethdr(iwin[0]))
;  check to see if descaled, if so subract 2 from header length
;  (o_bscale and o_bzero)
    o_bscale=self->getinfo('O_BSCALE',iwin[0],found=found)
    if found then begin
      nmax=nmax-2
      scaling1=[o_bscale,self->getinfo('O_BZERO')]
    endif else begin
      scaling1=[self->getinfo('BSCALE',iwin[0]),self->getinfo('BZERO')]
      if scaling1[0] eq 0 then scaling1=[1.,0.]
   endelse
    nblock=(nmax-1)*80/2880+1
    offset=nblock*2880
    if 1.0 eq swap_endian(1.0,/swap_if_big_endian) then swap=1
    if n_elements(iwin) eq 1 then begin
      iris_ximovie,self->getfilename(iwin),self->getnaxis1(iwin),self->getyw(iwin), $
        nframes=self->getntime(iwin),type=self->getdatatype(), $
        offset=offset,swap=swap,magnification=magnification,group_leader=group_leader, $
                      first_im=first_im, last_im=last_im, start_im=start_im, $
                 pos=pos,  $    ; PRY, 17-Nov-2015
        time=self->ti2utc(),/clock,missing=self->missing(),title=self->getfilename(iwin), $
        scaling1=scaling1,imax=self->getinfo('DATAP90',iwin)
    endif else begin 
      if self->getread_sji(iwin[0]) eq 0 or self->getread_sji(iwin[1]) eq 0 then begin 
        message,'One or both of these channels have not been read. Returning',/info
        message,'Currently read channels are:',/info
        self->show_sji
        return
      endif
      if self->getnaxis1(iwin[0]) ne self->getnaxis1(iwin[1]) or $
         self->getyw(iwin[0]) ne self->getyw(iwin[1]) then begin
        message,'Slit jaw movies are not the same dimension. Returning',/info
        return
      endif
      iris_ximovie,self->getfilename(iwin[0]),self->getnaxis1(iwin[0]),self->getyw(iwin[0]), $
        afile2=self->getfilename(iwin[1]), $
        nframes=self->getntime(iwin[0]),type=self->getdatatype(), $
                      first_im=first_im, last_im=last_im, start_im=start_im, $
                 pos=pos, $    ; PRY, 17-Nov-2015
        offset=offset,swap=swap,magnification=magnification,group_leader=group_leader, $
        time=self->ti2utc(),/clock,missing=self->missing(),title=self->getfilename(iwin[0]), $
        scaling1=scaling1,scaling2=scaling1, $
        imax=self->getinfo('DATAP90',iwin[0]),i2max=self->getinfo('DATAP90',iwin[1])
    endelse
  endelse
end

function iris_sji::descale_array,var
; descale data in var: data=var*bscale+bzero
  bscale=self->getinfo('BSCALE')
  if datatype(bscale) eq 'INT'and bscale eq 0 then bscale=1.0
  bzero=self->getinfo('BZERO')
  return,var*bscale+bzero
end

; i/o and related methods

function iris_sji::badfile,iwin
  if n_params() eq 0 then begin
    iwin=self.default_sjiwin
  endif
  return,self.badfile[iwin]
end

pro iris_sji::read,file,verbose=verbose
  if n_params() eq 0 then begin
    message,'iris_sji->read,files',/info
    return
  end
  if n_elements(verbose) eq 0 then silent=1 else silent=0
  nfiles=n_elements(file)
  f = nfiles eq 1 ? [file]:file
  self.nfiles=nfiles
  self.sji_read=0
  nwin=0
  for ifile=0,nfiles-1 do begin
    if not (file_info(f[ifile])).exists then begin
      message,f[ifile]+' does not exist, exiting',/info
      return
    endif
; read first extension of each file, determine number of windows etc
;    d=readfits(f[ifile],hdr,exten_no=0,silent=silent)
    mrd_head,f[ifile],hdr,extension=0
    ;check iris_prep version used in this file, and if too old, print warning
    if ifile eq 0 then check_cal = iris_prep_version_check(f[ifile],/loud)
; say whether window is NUV, FUV, or SJI
    self.region[ifile]=strtrim(strupcase(fxpar(hdr,'TDET1')),2)
; 
; fill object data structure as appropriate
    case self.region[ifile] of 
      'SJI': begin
         self->read_sji,d,hdr,f[ifile],verbose=verbose
         nwin=nwin+1
             end
      else: message,file[ifile]+' not a slit jaw file, not reading',/info
    endcase
;
  endfor
  self.nwin=nwin
end

pro iris_sji::read_sji,d,hdr,f,verbose=verbose
  if n_elements(verbose) eq 0 then silent=1 else silent=0
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
  if self.default_sjiwin eq -1 then self.default_sjiwin=lwin
  self.sji_read[lwin]=1
  self.badfile[lwin]=0
  self.lwin[lwin]=1
  self.file[lwin]=f
  self.sji_id[lwin]=sji_id
  self.hdr[lwin]=ptr_new(hdr)
  self.naxis1[lwin]=fxpar(hdr,'NAXIS1')
  self.naxis2[lwin]=fxpar(hdr,'NAXIS2')
  self.naxis3[lwin]=fxpar(hdr,'NAXIS3')
;
  self.sji_info[lwin].xs=fxpar(hdr,'TSC1')-1
  self.sji_info[lwin].xw=fxpar(hdr,'TEC1')-fxpar(hdr,'TSC1')+1
  self.sji_info[lwin].ys=fxpar(hdr,'TSR1')-1
  self.sji_info[lwin].yw=fxpar(hdr,'TER1')-fxpar(hdr,'TSR1')+1
; quick validity check of file
  if self->getnaxis1(lwin) eq 3 and $
     self->getnaxis2(lwin) eq 3 then begin
    message,'No data in SJI file!! Status set to "badfile"',/info
    self.badfile[lwin]=1
    return
  endif
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
  self.sjiobs_info[lwin].frmid=ptr_new(string(a[srow:erow,*]))
  srow=fxpar(h,'FDBIDS')
  erow=fxpar(h,'LFDBIDS')+srow-1
  self.sjiobs_info[lwin].fdbids=ptr_new(string(a[srow:erow,*]))
  srow=fxpar(h,'CRSIDS')
  erow=fxpar(h,'LCRSIDS')+srow-1
  self.sjiobs_info[lwin].crsids=ptr_new(string(a[srow:erow,*]))
  srow=fxpar(h,'FILES')
  erow=fxpar(h,'LFILES')+srow-1
  if(erow ge srow) then self.sjiobs_info[lwin].files = $
    ptr_new(string(a[srow:erow,*])) else self.sjiobs_info[lwin].files=ptr_new(' ')
;
  openr,lu,f,/swap_if_little_endian,/get_lun
  self.lu[lwin]=lu
  position=iris_find_winpos(lu,0)
  naxis1=self.naxis1[lwin]
  naxis2=self.naxis2[lwin]
  case fxpar(hdr,'BITPIX') of 
    16: self.w[lwin] = $
        ptr_new(assoc(lu,intarr(naxis1,naxis2),position))
   -32: self.w[lwin] = $
        ptr_new(assoc(lu,fltarr(naxis1,naxis2),position))
      else: message,'unsupported datatype '+self->getdatatype()
  endcase
;  self.w[lwin]=ptr_new(d)
end


pro iris_sji::align_limb
;calls the widget iris_align_limb, all changes made within this widget are applied to this object
  s=iris_align_limb(self)
end


pro iris_sji::align, lwin, region=region, dxy=dxy
;aligns the images using iris_align (input: region, output: dxy)
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  iris_align, self->getfilename(lwin), dxy, region=region
  self->shift,lwin,dxy,/cumulative
end


pro iris_sji::shift, lwin, dxy, quiet=quiet, cumulative=cumulative
;shifts the original image by dxy (dxy must be vector [2,(nr. of exposures)])
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  if n_elements(dxy) eq 0 then begin
    message,'dxy must be provided',/info
    return
  endif
  szdxy=size(dxy)
  szim=size(*self.w[lwin])
  if (szdxy[0] ne 2) || (szdxy[1] ne 2) || (szdxy[2] ne szim[3]) then begin
    message,'dxy has wrong size ([2,(nr. of exposures)])',/info
    return
  endif
  if keyword_set(cumulative) then begin
    dxy[0,*]=total(reform(dxy[0,*]),/cum)
    dxy[1,*]=total(reform(dxy[1,*]),/cum)
  endif
  if ~ptr_valid(self.align_dxy[lwin]) then $
    self.align_dxy[lwin] = ptr_new(fltarr(2,(size(*self.w[lwin]))[3]))
  *self.align_dxy[lwin] = *self.align_dxy[lwin] + dxy
  ptr_free,self.w[lwin]
  self.w[lwin]=ptr_new(iris_shift(self->getfilename(lwin),*self.align_dxy[lwin],quiet=quiet,/noncumulative))
end


pro iris_sji::align_finetune, lwin, expnr, dxy, quiet=quiet
;aligns the image expnr using the provided dxy vector [x,y]
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  if n_elements(expnr) eq 0 || n_elements(dxy) eq 0 then begin
    message,'expnr and dxy must be provided',/info
    return
  endif
  szdxy=size(dxy)
  if (szdxy[0] ne 1) || (szdxy[1] ne 2) then begin
    message,'dxy has wrong size ([dx,dy])',/info
    return
  endif
  if ~ptr_valid(self.align_dxy[lwin]) then $
    self.align_dxy[lwin] = ptr_new(fltarr(2,(size(*self.w[lwin]))[3]))
  (*self.align_dxy[lwin])[*,expnr] = (*self.align_dxy[lwin])[*,expnr] + dxy
  (*self.w[lwin])[*,*,expnr] = iris_shiftf((*self.w[lwin])[*,*,expnr], dxy[0], dxy[1])
end


pro iris_sji::realign, lwin, quiet=quiet
;realigns the original with current dxy, recommended after usage of align_finetune
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  if ptr_valid(self.align_dxy[lwin]) then begin
    ptr_free,self.w[lwin]
    self.w[lwin]=ptr_new(iris_shift(self->getfilename(lwin),*self.align_dxy[lwin],quiet=quiet,/noncumulative))
  endif
end


function iris_sji::get_alignshift, lwin
;returns the shift vector dxy (-1 if not available)
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  if ptr_valid(self.align_dxy[lwin]) then return, *self.align_dxy[lwin] $
  else return,-1
end


pro iris_sji::savefits, filename, lwin
;saves the data in a fits-file (same format and keywords as the original)
  if n_elements(filename) eq 0 then begin
    message, 'filename required as input',/info
    return
  endif
  if n_elements(lwin) eq 0 then lwin=self.default_sjiwin
  hdr = self->gethdr(lwin)
  writefits, filename, self->getvar(lwin), hdr
  ofile = self->getfilename(lwin)
  data = readfits(ofile, hdraux1, ext=1)
  writefits, filename, data, hdraux1, /append
  data = readfits(ofile, hdraux2, ext=2)
  writefits, filename, data, hdraux2, /append
end



pro iris_sji__define           
mfile=6
mwin=25
wpos=create_struct(name='win_info','xs',0,'xw',0,'ys',0,'yw',0)
sjiobsinf=create_struct(name='sjiobs_info','frmid',ptr_new(), $
                                     'fdbids',ptr_new(),'crsids',ptr_new(), $
                                     'files',ptr_new())
sjiinf=create_struct(name='sji_info','xs',0,'xw',0,'ys',0,'yw',0,'time',ptr_new(), $
                                     'pztx',ptr_new(),'pzty',ptr_new(), $
                                     'exptime',ptr_new(),'slit',ptr_new(), $
                                     'sumsptrs',ptr_new(),'sumspats',ptr_new(), $
                                     'dsrcs',ptr_new(),'lutids',ptr_new(), $
                                     'xcen',ptr_new(),'ycen',ptr_new(), $
                                     'sltpx1',ptr_new(),'sltpx2',ptr_new(), $
                                     'obs_vr',ptr_new(),'ophase',ptr_new())
struct={iris_sji, title: '  ', $
                 comment:'', $
                 ver_no: 0, $
                 aux:ptr_new(), $
                 cal:ptr_new(), $
                 date: '2012-12-31', $
                 nwin: 0, $
                 regtot: intarr(mfile), $
                 obsid: strarr(mfile), $
                 fdbid: strarr(mfile), $
                 crsid: strarr(mfile), $
                 xcen: -9999., $
                 ycen: -9999., $
                 file: strarr(mfile), $
                 lu:intarr(mfile), $
                 badfile: intarr(mfile), $
                 nfiles: 0, $
                 lwin: intarr(mfile), $
                 default_sjiwin:-1,$
                 region: strarr(mfile), $
                 sji_id: strarr(mfile), $
                 naxis1: intarr(mfile), $
                 naxis2: intarr(mfile), $
                 naxis3: intarr(mfile), $
                 hdr: ptrarr(mfile),$
                 w: ptrarr(mfile),$ ;actual data
                 align_dxy:ptrarr(mfile),$
                 sji_info: replicate({sji_info},mfile),$
                 sjiobs_info: replicate({sjiobs_info},mfile),$
                 nuv_sz:[0.,0.], $
                 fuv_sz:[0.,0.], $
                 sji_read:intarr(mfile) $
           }
end

