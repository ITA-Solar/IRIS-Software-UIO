; 
;+
; NAME:
;       SDO_ICON__DEFINE
;
; PURPOSE:
;       Retrieves and stores small 't' SDO images
;
; CATEGORY:
;       IRIS Data analysis SW
;
; CALLING SEQUENCE:
;       The IRIS_SJI__DEFINE procedure is not called directly. An
;       object of class IRIS_SJI is created with the following
;       statement:
;                   sdo_icon = obj_new('sdo_icon',date,line=line)
;
; INPUTS:
;       date : required date of retrieved sdo image
;
; KEYWORD PARAMETERS:
;       line : which sdo channel to retrieve
;
; OUTPUTS:
;       Objects of type SDO_ICON 
;
; CALLS:
;       sock_ping, sock_copy, limb_info
;
; COMMON BLOCKS:
;
; PROCEDURE:
;       The procedure opens an object of class SDO_ICON. 
;       A search is made of the current working directory for the 
;       requested files, e.g. t0171.jpg and t4500.jpg (for limb
;       fitting), if these are not found a query (sock_copy) goes to
;       the LMSAL web site for download.
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       13-Jun-2013: Viggo Hansteen
;
; $Id: sdo_icon__define.pro,v 1.16 2016/06/17 08:55:47 mawiesma Exp $
;
;-
function sdo_icon::init,date,line=line,quality=quality
  self.title='SDO Icon'
  if n_elements(line) eq 0 then self.line='0171' else self.line=line
  if n_elements(quality) eq 0 then quality='low'
;
  self.size=200
  self.url='http://sdowww.lmsal.com'
  self.dir='/sdomedia/SunInTime/'
;
  if n_elements(date) ne 0 then begin
    self->read,date,line,quality=quality
  endif
  return,1
end

pro sdo_icon::cleanup
  return
end

pro sdo_icon::help, description=description
;prints out this help, setting the 'description' keyword will also print the header info
  if arg_present(description) || keyword_set(description) then $
    obj_help,self, description=description $
  else $
    obj_help,self
end

pro sdo_icon::setsize,size
  self.size=size
end

pro sdo_icon::read,date,line,quality=quality
  if n_elements(line) eq 0 then line=self.line
  case quality of 
    'high':head='l'
    'low':head='t'
    else: head='t'
 endcase
  ntimes=n_elements(self->read_times(date))
  self.times[0:ntimes-1]=(self->read_times(date))[0:ntimes-1]
  if where(strmid(self.times,0,5) eq line+':') ne -1 then begin
    self.im=ptr_new(self->read_im(date,head+line+'.jpg'))
    time=(strsplit(self.times[where(strmid(self.times,0,5) eq line+':')],' ',/extract))[1]
    self.date=anytim2cal(time,form=1)
  endif else begin
    self.im=ptr_new(-1)
    message,'cound not read '+quality+' quality '+line+'AA image',/info
 endelse
  if (where(strmid(self.times,0,5) eq '4500:'))[0] ne -1 then begin
    self.im_4500=ptr_new(self->read_im(date,head+'4500.jpg'))
    time=(strsplit(self.times[(where(strmid(self.times,0,5) eq '4500:'))[0]],' ',/extract))[1] 
  endif else begin
    self.im_4500=ptr_new(-1)
    message,'cound not read '+quality+' quality '+'4500'+'AA image',/info
  endelse
  if (*self.im_4500)[0] ne -1 then begin
    limb_info,reform((*self.im_4500)[0,*,*]),time,x0,y0,scale,r0
    if n_elements(x0) ne 0 then begin
      dum=(size(*self.im_4500))[2]
      self.x0=x0
      self.y0=y0
      self.scale=scale
      self.r0=r0
   endif else begin
      message,'Limb fit failed, position of raster may be slightly wrong',/info
      dum=(size(*self.im_4500))[2]
      self.x0=dum/2.
      self.y0=dum/2.
      self.scale=9.6192475*(256./dum) ; just guessing
      self.r0=get_rb0p(time,/radius,/quiet)*self.scale
   endelse
  endif
  return
end

function sdo_icon::read_im,date,file  
  if not (file_info(self->sdo_appreadme(date)+path_sep()+file)).exists then begin
    sock_ping,self.url,status
    if status then begin
      sock_copy,self.url+self.dir+anytim2cal(date,/date,form=11)+'/'+file,out_dir=self->sdo_appreadme(date)
    endif else begin
      message,'no file '+file+' nor internet connection found, returning',/info
      return,-1
    endelse 
  endif
  if (file_info(self->sdo_appreadme(date)+path_sep()+file)).exists then begin
    read_jpeg,self->sdo_appreadme(date)+path_sep()+file,im,true=1
  endif else begin
    message,' could not download '+self.url+self.dir+anytim2cal(date,/date,form=11)+'/'+file+' returning',/info
    return,-1
 endelse
   return,im
;  return,congrid(im,3,self.size,self.size)
end

function sdo_icon::read_times,date
  file='times.txt'
  if not (file_info(self->sdo_appreadme(date)+path_sep()+file)).exists then begin
    sock_ping,self.url,status
    if status then begin
      sock_copy,self.url+self.dir+anytim2cal(date,/date,form=11)+'/'+file,out_dir=self->sdo_appreadme(date)
    endif else begin
      message,'no file nor internet connection found, returning',/info
      return,-1
    endelse 
  endif
  return,rd_ascii(self->sdo_appreadme(date)+path_sep()+file)
end

pro sdo_icon::rotate,date,keep_limb=keep_limb
  if n_elements(keep_limb) eq 0 then keep_limb=0
  maptime=anytim2utc(self.date)
  obstime=anytim2utc(date)
  dtime=(obstime.mjd-maptime.mjd)*24.d0+(obstime.time-maptime.time)*0.001d0/3600.d0
;  im_rot=bytarr(3,self.size,self.size)
  im_rot=(*self.im)
  dum=(size(*self.im))[2]
  for i=0,2 do begin
     map=make_map(reform((*self.im)[i,*,*]),dx=self.scale,dy=self.scale, $
                                         xc=(dum/2.-self.x0)/self.scale,yc=(dum/2.-self.y0)/self.scale, $
                                         time=maptime)
    dmap=drot_map(map,dtime,keep_limb=keep_limb)
    im_rot[i,*,*]=dmap.data
  end
  *self.im=im_rot
  for i=0,2 do begin
    map=make_map(reform((*self.im_4500)[i,*,*]),dx=self.scale,dy=self.scale, $
                                         xc=(dum/2.-self.x0)/self.scale,yc=(dum/2.-self.y0)/self.scale, $
                                         time=maptime)
    dmap=drot_map(map,dtime,keep_limb=keep_limb)
    im_rot[i,*,*]=dmap.data
  end
  *self.im_4500=im_rot
end

function sdo_icon::gettitle
  return,self.title
end

function sdo_icon::getsize
  return,self.size
end

function sdo_icon::getlimb
  return,{x0:self.x0,y0:self.y0,r0:self.r0,scale:self.scale,size:self.size}
end

function sdo_icon::getim
  return,*self.im
end

function sdo_icon::getim_4500
  return,*self.im_4500
end

function sdo_icon::getdate
  return,self.date
end

function sdo_icon::sdo_appreadme,date
; This function initializes some readme-files for the user, which are
; saved in the hidden .idl folder in the home directory of the user
; It returns the path to that directory, which is used to save the
; sdo thumbnails for the given date
;
    AuthorDirname = 'viggoh'
    AuthorDesc = 'Viggo Hansteen, Institute of Theoretical Astrophysics, University of Oslo'
    AppDirname = 'sdo_icon'+path_sep()+anytim2cal(date,/date,form=11)
    AppDesc = 'SDO Icon'
    AppReadmeText = 'sdo_icon_define, written by Viggo Hansteen, ITA, University of Oslo ' + $
                    '(viggo.hansteen@astro.uio.no)' + $
                    '/n these files can be deleted without restrictions.'
    AppReadmeVersion=1
    return, APP_USER_DIR(AuthorDirname, AuthorDesc, AppDirname, AppDesc, AppReadmeText, AppReadmeVersion)
end

function sdo_icon::getdata
  im=ptrarr(2)
  im[0]=ptr_new(*self.im)
  im[1]=ptr_new(*self.im_4500)
  return,{im:im,id:[self.line,'4500'],nr:2}
end

function sdo_icon::raster_coords,xcen,ycen,dx,dy,theta
  theta=theta*!pi/180.
  limb=self->getlimb()
  xcen=limb.x0+xcen/limb.scale
  ycen=limb.y0+ycen/limb.scale
  dx=dx/limb.scale
  dy=dy/limb.scale
  return,{x1:xcen-dx*cos(theta)/2.+dy*sin(theta)/2., $
          x2:xcen+dx*cos(theta)/2.+dy*sin(theta)/2., $
          x3:xcen+dx*cos(theta)/2.-dy*sin(theta)/2., $
          x4:xcen-dx*cos(theta)/2.-dy*sin(theta)/2., $
          y1:ycen-dy*cos(theta)/2.-dx*sin(theta)/2., $
          y2:ycen-dy*cos(theta)/2.+dx*sin(theta)/2., $
          y3:ycen+dy*cos(theta)/2.+dx*sin(theta)/2., $
          y4:ycen+dy*cos(theta)/2.-dx*sin(theta)/2.}
end

pro sdo_icon__define           
struct={sdo_icon, title: '', $
                 date: '2012-12-31', $
                 url: 'http://sdowww.lmsal.com', $
                 dir: '/sdomedia/SunInTime/', $
                 line:'',$
                 x0: 0.0, $
                 y0: 0.0, $
                 r0: 0.0, $
                 size: 0.0, $
                 scale: 0.0, $
                 times: strarr(20), $
                 im: ptr_new(), $
                 im_4500: ptr_new() $
        }
end


