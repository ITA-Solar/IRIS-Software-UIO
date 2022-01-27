; $Id: iris_l1to2aia.pro,v 1.54 2018/05/18 09:55:23 mawiesma Exp $  ;

pro iris_l1to2aia, aiadir, obs2fov, waves=waves, $
  deletetempfiles=deletetempfiles, method=method, aiafilesl1=aiafilesl1, nfilesl1=nfilesl1, $
  outdir=outdir, aialog=aialog, sswjobid=sswjobid, debug=debug, _extra=_extra

  version = 'A2I-2018-05-18'
  aialog.version = version

  GET_UTC, Date_RF2, /CCSDS

  if N_PARAMS() lt 1 then begin
    print,'need AIA DIR'
    return
    ;method=1
    ;
    ;aiadir='~/iris/aia/aia/20140409_055055_3820259453/'
    ;obsdir='~/iris/aia/20140409_055055_3820259453/'
    ;aiadir='~/iris/aial1p5s/20140409_055055_3820259453/'
    ;aiadir='~/iris/aia1p5/20140209_055024_3882010144/'
    ;obsdir='/mn/xsan/d2/iris/data/level2/2014/02/09/20140209_055024_3882010144/'

    ;aiadir = '~/iris/aiadirect/'
    ;aiadir='~/iris/aia/20140410_073954_3820259453/'
    ;obsdir = '/mn/xsan/d2/iris/data/level2/2014/02/01/20140201_090500_3880012095/' ;64-step-raster  399.4 /  -77.9  ;  -0
    ;obsdir = '/mn/xsan/d2/iris/data/level2/2014/04/10/20140410_051024_3882010194/' ;64-step-raster  148.3 /  291.1  ; -20
    ;obsdir = '/mn/xsan/d2/iris/data/level2/2014/04/10/20140410_073954_3820259453/' ;sit-and-stare, -600.8 / -776.5  ; -52
    ;obsdir = '/mn/xsan/d2/iris/data/level2/2014/04/09/20140409_010946_3820007253/' ;sit-and-stare,   -3.5/   965.5  ; -90
    ;macbook:
    ;obsdir = '~/iris/data/level2/20140201_090500_3880012095/' ;64-step-raster  399.4 /  -77.9  ;  -0
    ;obsdir = '~/iris/data/level2/20140410_051024_3882010194/' ;64-step-raster  148.3 /  291.1  ; -20
    ;obsdir = '~/iris/data/level2/20140410_073954_3820259453/' ;sit-and-stare, -600.8 / -776.5  ; -52
    ;obsdir = '~/iris/data/level2/20140409_010946_3820007253/' ;sit-and-stare,   -3.5/   965.5  ; -90
  endif
  if strmid(aiadir, 0,1, /reverse_offset) ne path_sep() then aiadir = aiadir+path_sep()
  if ~keyword_set(waves) then waves=['94','131','171','193','211','304','335','1600','1700','4500']
  if ~keyword_set(outdir) then outdir=aiadir
  if strmid(outdir, 0,1, /reverse_offset) ne path_sep() then outdir = outdir+path_sep()

  if N_ELEMENTS(waves) eq 1 then waves=strsplit(waves,',',/extract)




  ;calculate regression of xcen and ycen of IRIS SJI
  obs2fov->get_xycenreg, regxslope, regxconst, regyslope, regyconst




  ;AIA data

  ;window,/free,xsize=1000,ysize=1000
  for iwave=0,N_ELEMENTS(waves)-1 do begin
    case method of
      0: begin
        aiafiles = file_search(aiadir+'original/*_'+waves[iwave]+'_.fts', count=nfiles)
        if nfiles eq 0 then aiafiles = file_search(aiadir+'*_'+waves[iwave]+'_.fts', count=nfiles)
      end

      1: begin
        if N_ELEMENTS(nfilesl1) gt iwave then begin
          nfiles = nfilesl1[iwave]
          if nfiles gt 0 then aiafiles=*aiafilesl1[iwave]
        endif else begin
          aiafiles = file_search(aiadir+'original/*.'+waves[iwave]+'A_*', count=nfiles)
          if nfiles eq 0 then aiafiles = file_search(aiadir+'*.'+waves[iwave]+'A_*', count=nfiles)
        endelse
      end

      2: begin
        if N_ELEMENTS(nfilesl1) gt iwave then begin
          nfiles = nfilesl1[iwave]
          if nfiles gt 0 then aiafiles=*aiafilesl1[iwave]
        endif
      end

      3: begin
        if N_ELEMENTS(nfilesl1) gt iwave then begin
          nfiles = nfilesl1[iwave]
          if nfiles gt 0 then aiafiles=*aiafilesl1[iwave]
        endif
      end

      else: begin
        print,'unknown method'
        return
      end
    endcase
    print, 'processing wave '+strtrim(string(iwave+1),2)+' of '+strtrim(string(N_ELEMENTS(waves)),2)+'  ; wave = '+waves[iwave]
    print, 'number of files: ',nfiles
    aialog.filesreceived = aialog.filesreceived + nfiles
    if nfiles gt 0 then begin
      ;version 0 of reading  ;uses a lot of memory
      ;      case method of
      ;        0:mreadfits_header, aiafiles, hdrall;, only_tags='DATE_OBS,CDELT1,CDELT2,NAXIS1,NAXIS2' ;,CRVAL1,CRPIX1,CRVAL2,CRPIX2,CROTA2
      ;        1:aia_prep, aiafiles, -1, hdrall, dataall
      ;        2:aia_prep, aiafiles, -1, hdrall, dataall
      ;      endcase


      startobs = obs2fov->get_startobs()
      endobs = obs2fov->get_endobs()
      tstart = str2utc(startobs)
      timeaia = dblarr(nfiles)
      crval1aia = fltarr(nfiles)
      crval2aia = fltarr(nfiles)
      pc1_1 = fltarr(nfiles)
      pc1_2 = fltarr(nfiles)
      pc2_1 = fltarr(nfiles)
      pc2_2 = fltarr(nfiles)
      pc3_1 = fltarr(nfiles)
      pc3_2 = fltarr(nfiles)
      lvl_num = fltarr(nfiles)


      ;version 2 of reading  ;uses a lot of memory
      ;      if method eq 2 then begin
      ;        read_sdo, aiafiles, hdrme2, datame2, /use_lib
      ;        ; Use AIA-specific 3h cadence master pointing series. Must have access
      ;        ; to jsoc2
      ;        aia_prep, hdrme2, datame2, hdrme2b, datame2b, /verbose, ds='aia_test.master_pointing3h', /refresh, /use_sswmp
      ;        hdrme2 = !NULL
      ;        datame2 = !NULL
      ;      endif

      ;version 4 of reading
      maxl1files = 100
      l1group=0

      index_ref=!NULL
      quality_old=0
      for ifile=0,nfiles-1 do begin
        t1=systime(1)
        case method of
          0: begin
            read_sdo, aiafiles[ifile], hdr, dataone, /use_shared, /noshell, /uncomp_delete
            sizedata = size(dataone)
          end
          1: begin
            ;version 0 of reading  ;uses a lot of memory
            ;dataone=dataall[*,*,ifile]
            aia_prep, aiafiles[ifile], -1, hdr, dataone, index_ref=index_ref, $
              /use_shared, /noshell, /uncomp_delete, _extra=_extra
            sizedata = size(dataone)
          end
          2: begin
            ;version 0 of reading  ;uses a lot of memory and incompatible with 3h cadence
            ;dataone=dataall[*,*,ifile]
            ;version 1 of reading  ;incompatible with 3h cadence
            ;aia_prep, aiafiles[ifile], -1, hdr, dataone, index_ref=index_ref, _extra=_extra
            ;version 2 of reading  ;uses a lot of memory
            ;dataone = datame2b[*,*,ifile]
            ;hdr = hdrme2b[ifile]
            ;version 3 of reading  ;is half as fast as version 2
            ;aia_prep, aiafiles[ifile], -1, hdr, dataone, index_ref=index_ref, /verbose, $
            ;  ds='aia_test.master_pointing3h', /refresh, /use_sswmp, _extra=_extra
            ;version 4 of reading
            if ifile MOD maxl1files eq 0 then begin
              if (l1group+1)*maxl1files gt nfiles then nnewfiles = nfiles - l1group*maxl1files $
              else nnewfiles = maxl1files
              indcur = indgen(nnewfiles) + l1group*maxl1files
              l1group = l1group+1
              aia_prep, aiafiles[indcur], -1, hdrcur, datacur, index_ref=index_ref, $
                ds='aia_test.master_pointing3h', /use_sswmp, /use_shared, /noshell, /uncomp_delete, _extra=_extra
            endif
            indcur = ifile - (l1group-1)*maxl1files
            hdr = hdrcur[indcur]
            dataone = datacur[*,*,indcur]
            sizedata = size(dataone)
          end
          3: begin
            read_sdo, aiafiles[ifile], hdr, dataone, /use_shared, /noshell, /uncomp_delete
            sizedata = size(dataone)
          end
        endcase
        t2=systime(1)
        aialog.tread = aialog.tread + t2-t1

        if quality_old ne 0 && ifile gt 0 then quality_old=quality
        if required_tags(hdr, /quality) then quality=ulong(hdr.quality) $
        else quality=0UL
        if ifile eq 0 then quality_old=quality
        if quality_old ne 0 then index_ref=hdr

        if ifile eq 0 then begin
          index_ref = hdr
          hdrall = hdr
          qualityall = quality
          fovexpand = obs2fov->get_fovexpand()
          cdelt1 = hdr.cdelt1
          cdelt2 = hdr.cdelt2
          naxis1 = ceil((obs2fov->get_fovx()+fovexpand[0])/hdr.cdelt1)
          naxis2 = ceil((obs2fov->get_fovy()+fovexpand[1])/hdr.cdelt2)
          crpix1 = naxis1/2.0+1.0
          crpix2 = naxis2/2.0+1.0
          fovx = naxis1 * hdr.cdelt1
          fovy = naxis2 * hdr.cdelt2
          aiacube = make_array(naxis1, naxis2, nfiles, /float, value=!values.f_nan)
          ;          if method eq 0 then $
          ;            datatemp = fltarr(ceil(sqrt(hdrall[0].naxis1^2+hdrall[0].naxis2^2)),ceil(sqrt(hdrall[0].naxis1^2+hdrall[0].naxis2^2)))
        endif else begin
          hdrall = concat_struct(hdrall, hdr)
          qualityall = [qualityall, quality]
          if abs(hdr.cdelt1-cdelt1) gt 0.001 then begin
            box_message,['cdelt1 differs in this image compared to first image',$
              'first  : '+string(cdelt1, format='(f7.4)'), $
              'current: '+string(hdr.cdelt1, format='(f7.4)'), $
              string(ifile)+'  '+aiafiles[ifile], $
              'using the first one']
            if aialog.nbadcdelt1 eq 0 then begin
              aialog.cdelt1 = ptr_new(cdelt1)
              aialog.badcdelt1 = ptr_new(hdr.cdelt1)
              aialog.badcdelt1_file = ptr_new(aiafiles[ifile])
            endif else begin
              *aialog.cdelt1 = [*aialog.cdelt1, cdelt1]
              *aialog.badcdelt1 = [*aialog.badcdelt1, hdr.cdelt1]
              *aialog.badcdelt1_file = [*aialog.badcdelt1_file, aiafiles[ifile]]
            endelse
            aialog.nbadcdelt1 = aialog.nbadcdelt1 + 1
          endif
          if abs(hdr.cdelt2-cdelt2) gt 0.001 then begin
            box_message,['cdelt2 differs in this image compared to first image',$
              'first  : '+string(cdelt2, format='(f7.4)'), $
              'current: '+string(hdr.cdelt2, format='(f7.4)'), $
              string(ifile)+'  '+aiafiles[ifile], $
              'using the first one']
            if aialog.nbadcdelt2 eq 0 then begin
              aialog.cdelt2 = ptr_new(cdelt2)
              aialog.badcdelt2 = ptr_new(hdr.cdelt2)
              aialog.badcdelt2_file = ptr_new(aiafiles[ifile])
            endif else begin
              *aialog.cdelt2 = [*aialog.cdelt2, cdelt2]
              *aialog.badcdelt2 = [*aialog.badcdelt2, hdr.cdelt2]
              *aialog.badcdelt2_file = [*aialog.badcdelt2_file, aiafiles[ifile]]
            endelse
            aialog.nbadcdelt2 = aialog.nbadcdelt2 + 1
          endif
        endelse

        timeaiatemp = str2utc(hdr.date_obs)
        timeaia[ifile] = (timeaiatemp.mjd-tstart.mjd)*86400L + (timeaiatemp.time-tstart.time)/1000d

        ;get xcen/ycen from the regression result
        xcenaia = regxconst + timeaia[ifile] * regxslope
        ycenaia = regyconst + timeaia[ifile] * regyslope

        if keyword_set(debug) then begin
          wcstemp = fitshead2wcs(hdr)
          examine_image, dataone, wcstemp, 'Original'
        endif


        ;first roll the image to iris roll angle
        ;datatemp=rot(data[*,*,ifile], obs2fov->get_rollangle()-crota2orig[ifile], 1, x, y, /pivot, cubic=-0.5, missing=0)
        ;sizetemp = ceil(5*sqrt(sizedata[1]*sizedata[1]+sizedata[2]*sizedata[2]))
        sizetemp = ceil(3 * (max(abs([hdr.crpix1, hdr.crpix2])) / hdr.cdelt1 + max([sizedata[1], sizedata[2]])))
        datatemp = make_array(sizetemp, sizetemp, /float, value=!values.f_nan)
        xshifttemp = fix((sizetemp-sizedata[1])/2.0)
        yshifttemp = fix((sizetemp-sizedata[2])/2.0)
        crpix1temp = hdr.crpix1 + xshifttemp
        crpix2temp = hdr.crpix2 + yshifttemp
        datatemp[xshifttemp:xshifttemp+sizedata[1]-1, yshifttemp:yshifttemp+sizedata[2]-1] = dataone
        
        if keyword_set(debug) then begin
          ;this step is good
          hdrtemp = hdr
          hdrtemp.naxis1 = sizetemp
          hdrtemp.naxis2 = sizetemp
          hdrtemp.crpix1 = crpix1temp
          hdrtemp.crpix2 = crpix2temp
          wcstemp = fitshead2wcs(hdrtemp)
          examine_image, datatemp, wcstemp, 'Original padded', show_coord=[crpix1temp,crpix2temp]
        endif
        datatemp = rot(datatemp, obs2fov->get_rollangle()-hdr.crota2, 1, crpix1temp, crpix2temp, /pivot, cubic=-0.5, missing=0)
        if keyword_set(debug) then begin
          ;this step is good
          hdrtemp.crota2 = obs2fov->get_rollangle()
          wcstemp = fitshead2wcs(hdrtemp)
          examine_image, datatemp, wcstemp, 'Original padded and rotated', show_coord=[crpix1temp,crpix2temp]
        endif


        crval1aia[ifile] = xcenaia
        crval2aia[ifile] = ycenaia

        hdr.naxis1 = sizetemp;;;
        hdr.naxis2 = sizetemp;;;
        hdr.crpix1 = crpix1temp;;;
        hdr.crpix2 = crpix2temp;;;
        hdr.CROTA2 = obs2fov->get_rollangle()
        wcsaia = fitshead2wcs(hdr)
        pc1_1[ifile] = wcsaia.pc[0,0]
        pc1_2[ifile] = wcsaia.pc[0,1]
        pc2_1[ifile] = wcsaia.pc[1,0]
        pc2_2[ifile] = wcsaia.pc[1,1]


        ;calculate x/y coordinates of the four corners
        xx1start = hdr.crval1 + hdr.cdelt1 * (pc1_1[ifile] * (1-crpix1temp) $
          + pc1_2[ifile] * (1-crpix2temp))
        xx1end = hdr.crval1 + hdr.cdelt1 * (pc1_1[ifile] * (sizetemp-crpix1temp) $
          + pc1_2[ifile] * (1-crpix2temp))
        xx2start = hdr.crval1 + hdr.cdelt1 * (pc1_1[ifile] * (1-crpix1temp) $
          + pc1_2[ifile] * (sizetemp-crpix2temp))
        xx2end = hdr.crval1 + hdr.cdelt1 * (pc1_1[ifile] * (sizetemp-crpix1temp) $
          + pc1_2[ifile] * (sizetemp-crpix2temp))
        yy1start = hdr.crval2 + hdr.cdelt2 * (pc2_2[ifile] * (1-crpix2temp) $
          + pc2_1[ifile] * (1-crpix1temp))
        yy1end = hdr.crval2 + hdr.cdelt2 * (pc2_2[ifile] * (sizetemp-crpix2temp) $
          + pc2_1[ifile] * (1-crpix1temp))
        yy2start = hdr.crval2 + hdr.cdelt2 * (pc2_2[ifile] * (1-crpix2temp) $
          + pc2_1[ifile] * (sizetemp-crpix1temp))
        yy2end = hdr.crval2 + hdr.cdelt2 * (pc2_2[ifile] * (sizetemp-crpix2temp) $
          + pc2_1[ifile] * (sizetemp-crpix1temp))

        ;calculate where x/y are at the edges
        x1 = (xcenaia-xx1start) / (xx1end-xx1start) * (sizetemp-1)
        y1 = 0.0
        x2 = (xcenaia-xx2start) / (xx2end-xx2start) * (sizetemp-1)
        y2 = float(sizetemp-1)
        x3 = 0.0
        y3 = (ycenaia-yy1start) / (yy1end-yy1start) * (sizetemp-1)
        x4 = float(sizetemp-1)
        y4 = (ycenaia-yy2start) / (yy2end-yy2start) * (sizetemp-1)

        ;calculate point of intersection
        x = ((x4-x3) * (x2*y1 - x1*y2) - (x2-x1) * (x4*y3 - x3*y4)) / $
          ((y4-y3) * (x2-x1) - (y2-y1) * (x4-x3))
        y = ((y1-y2) * (x4*y3 - x3*y4) - (y3-y4) * (x2*y1 - x1*y2)) / $
          ((y4-y3) * (x2-x1) - (y2-y1) * (x4-x3))

        if keyword_set(debug) then begin
          ;this step is now also good
          examine_image, datatemp, wcstemp, 'Original padded and rotated with x and y', show_coord=[x,y]
          box_message,['        pixel   arcsec', $
            'X : '+string(x, format='(F9.2)')+string(xcenaia, format='(F9.2)'), $
            'Y : '+string(y, format='(F9.2)')+string(ycenaia, format='(F9.2)')]
          c = wcs_get_coord(wcsaia, [x, y])
          print,xcenaia,ycenaia
          print,c
          ;stop
        endif


        xs=x-naxis1/2.0
        ys=y-naxis2/2.0

        ;sub pixel shift
        ;shiftxsub to the left
        xss = fix(xs)
        shiftxsub = xs-xss
        xs = xss
        pcoeff = [shiftxsub,0,1,0]
        ;shitysub downwards
        yss = fix(ys)
        shiftysub = ys-yss
        ys=yss
        qcoeff =[shiftysub,1,0,0]

        ;create an array of number such that the j-th elementh as bit j set to 1 and all others set to 0
        ;i.e. 1,2,4,8,...,2^J,...
        BitArray=2UL^ulindgen(32)
        BitSet=(quality AND BitArray) NE 0
        ForbiddenBits=[0,1,3,4,7,12,13,14,15,16,17,18,20,21,31];if any of these bits is set - reject the image
        IF total(BitSet[ForbiddenBits]) GT 0 THEN BEGIN
          print, 'Forbidden Quality Bits set'
          if aialog.nbadquality eq 0 then begin
            aialog.badquality = ptr_new(quality)
            aialog.badquality_file = ptr_new(aiafiles[ifile])
          endif else begin
            *aialog.badquality = [*aialog.badquality, quality]
            *aialog.badquality_file = [*aialog.badquality_file, aiafiles[ifile]]
          endelse
          aialog.nbadquality = aialog.nbadquality + 1
          lvl_num[ifile] = -1
        ENDIF else begin
          aiacube[*,*,ifile] = INF_POLY_2D(datatemp[xss:xss+NAXIS1-1,yss:yss+naxis2-1],pcoeff,qcoeff,2,cubic=-0.5)
          if required_tags(hdr, /lvl_num) then lvl_num[ifile]=hdr.lvl_num $
          else lvl_num[ifile]=0
        endelse

        if keyword_set(debug) then begin
          oplot, [xss,xss], [yss,yss+naxis2-1]
          oplot, [xss+NAXIS1-1,xss+NAXIS1-1], [yss,yss+naxis2-1]
          oplot, [xss,xss+NAXIS1-1], [yss,yss]
          oplot, [xss,xss+NAXIS1-1], [yss+naxis2-1,yss+naxis2-1]
          
          hdrtemp.naxis1 = naxis1
          hdrtemp.naxis2 = naxis2
          hdrtemp.crpix1 = crpix1
          hdrtemp.crpix2 = crpix2
          hdrtemp.crval1 = crval1aia[ifile]
          hdrtemp.crval2 = crval2aia[ifile]
          hdrtemp = rem_tag(hdrtemp, ['crota1','crota2'])
          hdrtemp = create_struct(hdrtemp, 'PC1_1', pc1_1[ifile], 'PC1_2', pc1_2[ifile], $
            'PC2_1', pc2_1[ifile], 'PC2_2', pc2_2[ifile])
          wcstemp = fitshead2wcs(hdrtemp)
          examine_image, aiacube[*,*,ifile], wcstemp, 'AIA cube'
          print,'quality', quality eq 0 || quality eq nrt_bit_set
          stop
        endif

        ;stop







      endfor ;ifile

      ;calculate basic step size
      if nfiles gt 1 then begin
        cdelt3 = dblarr(nfiles-1)
        for i=1,nfiles-1 do begin
          cdelt3[i-1] = timeaia[i] - timeaia[i-1]
        endfor
      endif else cdelt3=0.0
      cdelt3 = mean(cdelt3)

      ind = floor(nfiles/2)
      xcenall = crval1aia[ind]
      ycenall = crval2aia[ind]
      timeall = timeaia[ind]
      crpix3 = ind+1.0


      statistics = iris_cube_statistics(aiacube)
      
      ;convert to integers
      ind = where(aiacube ne aiacube, count)
      if count gt 0 then aiacube[ind] = -200
      aiacube = fix(round(aiacube))

      ;stop

      t1=systime(1)

      ;first region is written into primary block
      mkhdr, mainheader, aiacube, /extend
      ;add some information to the primary header
      if required_tags(hdrall, /telescop) then telescop=IRISl12_mostcommonvalue(hdrall.Telescop) $
      else telescop=''
      sxaddpar, mainheader, 'TELESCOP', telescop
      if required_tags(hdrall, /instrume) then instrume=IRISl12_mostcommonvalue(hdrall.instrume) $
      else instrume=''
      sxaddpar, mainheader, 'INSTRUME', instrume
      sxaddpar, mainheader, 'DATA_LEV', 2.0;OBSvars.DATA_LEV
      sxaddpar, mainheader, 'LVL_NUM', 2.0;OBSvars.LVL_NUM
      sxaddpar, mainheader, 'VER_RF2', version;OBSvars.version
      sxaddpar, mainheader, 'DATE_RF2', Date_RF2;OBSvars.Date_RF2
      indlvl = where(lvl_num ge 0, count)
      if count gt 0 then data_src = mean(lvl_num[indlvl]) $
      else data_src = -1
      sxaddpar, mainheader, 'DATA_SRC', data_src;mean(lvl_num);OBSvars.DATA_SRC
      if required_tags(hdrall, /ORIGIN) then ORIGIN=IRISl12_mostcommonvalue(hdrall.ORIGIN) $
      else ORIGIN=''
      sxaddpar, mainheader, 'ORIGIN', ORIGIN
      if required_tags(hdrall, /BLD_VERS) then BLD_VERS=IRISl12_mostcommonvalue(hdrall.BLD_VERS) $
      else BLD_VERS=''
      sxaddpar, mainheader, 'BLD_VERS', BLD_VERS
      sxaddpar, mainheader, 'LUTID', 0;OBSvars.LUTID
      sxaddpar, mainheader, 'OBSID', obs2fov->get_obsid();OBSvars.OBSid
      sxaddpar, mainheader, 'OBS_DESC', '';OBSvars.OBS_Desc
      sxaddpar, mainheader, 'OBSLABEL', '';OBSvars.OBSLABEL
      sxaddpar, mainheader, 'OBSTITLE', '';OBSvars.OBSTITLE
      sxaddpar, mainheader, 'DATE_OBS', hdrall[0].Date_OBS
      sxaddpar, mainheader, 'DATE_END', hdrall[N_ELEMENTS(hdr_all)-1].Date_Obs;OBSvars.Date_End
      sxaddpar, mainheader, 'STARTOBS', startobs
      sxaddpar, mainheader, 'ENDOBS', endobs;OBSvars.OBSend
      sxaddpar, mainheader, 'OBSREP', 1;OBSvars.OBSrep
      if required_tags(hdrall, /CAMERA) then CAMERA=IRISl12_mostcommonvalue(hdrall.CAMERA) $
      else CAMERA=0
      sxaddpar, mainheader, 'CAMERA', CAMERA
      sxaddpar, mainheader, 'STATUS', '';OBSvars.STATUS
      if required_tags(hdrall, /BTYPE) then BTYPE=IRISl12_mostcommonvalue(hdrall.BTYPE) $
      else BTYPE=''
      sxaddpar, mainheader, 'BTYPE', BTYPE;OBSvars.btype
      if required_tags(hdrall, /BUNIT) then BUNIT=IRISl12_mostcommonvalue(hdrall.BUNIT) $
      else BUNIT=''
      sxaddpar, mainheader, 'BUNIT', BUNIT;OBSvars.bunit
      sxaddpar, mainheader, 'BSCALE', 1.0, format="f4.2";, ' True_value = BZERO + BSCALE*Array_value', after='BZERO'
      sxaddpar, mainheader, 'BZERO', 0;, ' True_value = BZERO + BSCALE*Array_value', after='BTYPE'
      sxaddpar, mainheader, 'HLZ', '';OBSvars.HLZ
      sxaddpar, mainheader, 'SAA', '';OBSvars.SAA
      sxaddpar, mainheader, 'SAT_ROT',  obs2fov->get_satrot();mean(hdrall.crota2);OBSvars.SAT_ROT
      ;sxaddpar, mainheader, 'AECNOBS', 0;OBSvars.AECNOBS
      ;sxaddpar, mainheader, 'AECNRAS', 0;OBSvars.AECNRAS
      ;sxaddpar, mainheader, 'ACS_ECLP', OBSvars.ACS_ECLP
      ;sxaddpar, mainheader, 'ACS_MODE', OBSvars.ACS_MODE
      ;sxaddpar, mainheader, 'ACS_SAFE', OBSvars.ACS_SAFE
      ;sxaddpar, mainheader, 'ACS_SUNP', OBSvars.ACS_SUNP
      ;sxaddpar, mainheader, 'ASD_REC', OBSvars.ASD_REC
      ;sxaddpar, mainheader, 'DATE', OBSvars.DAT
      if required_tags(hdrall, /DSUN_OBS) then DSUN_OBS=mean(hdrall.DSUN_OBS) $
      else DSUN_OBS=0d
      sxaddpar, mainheader, 'DSUN_OBS', DSUN_OBS
      ;if required_tags(hdrall, /DSUN_REF) then DSUN_OBS=mean(hdrall.DSUN_REF) $
      ;else DSUN_REF=0d
      ;sxaddpar, mainheader, 'DSUN_REF', DSUN_REF

      ;sxaddpar, mainheader, 'IAECEVFL', '';OBSvars.IAECEVFL
      ;sxaddpar, mainheader, 'IAECFLAG', '';OBSvars.IAECFLAG
      ;sxaddpar, mainheader, 'IAECFLFL', '';OBSvars.IAECFLFL
      sxaddpar, mainheader, 'TR_MODE', '';OBSvars.TR_MODE

      sxaddpar, mainheader, 'FOVY', fovy+fovexpand[N_ELEMENTS(fovexpand)-1]
      sxaddpar, mainheader, 'FOVX', fovx+fovexpand[0]
      sxaddpar, mainheader, 'XCEN', xcenall
      sxaddpar, mainheader, 'YCEN', ycenall
      sxaddpar, mainheader, 'SUMSPTRL', 1;OBSvars.sumspecmin
      sxaddpar, mainheader, 'SUMSPAT', 1;OBSvars.sumspatmin
      if required_tags(hdrall, /EXPTIME) then EXPTIME=hdrall.EXPTIME $
      else EXPTIME=dblarr(N_ELEMENTS(hdrall))
      sxaddpar, mainheader, 'EXPTIME', mean(exptime);OBSvars.exptime
      sxaddpar, mainheader, 'EXPMIN', min(exptime, max=maxexp);OBSvars.expmin
      sxaddpar, mainheader, 'EXPMAX', maxexp;OBSvars.expmax
      sxaddpar, mainheader, 'DATAMEAN', statistics.datamean
      sxaddpar, mainheader, 'DATARMS', statistics.datarms
      sxaddpar, mainheader, 'DATAMEDN', statistics.datamedn
      sxaddpar, mainheader, 'DATAMIN', statistics.datamin
      sxaddpar, mainheader, 'DATAMAX', statistics.datamax
      sxaddpar, mainheader, 'DATAVALS', statistics.datavals
      sxaddpar, mainheader, 'MISSVALS', 0;cmissing
      sxaddpar, mainheader, 'NSATPIX', 0;csaturated
      sxaddpar, mainheader, 'NSPIKES', 0
      sxaddpar, mainheader, 'TOTVALS', statistics.datavals;+cmissing+csaturated+0
      sxaddpar, mainheader, 'PERCENTD', 100.0;float(statistics.vals) / (statistics.vals+cmissing+csaturated+0) *100
      sxaddpar, mainheader, 'DATASKEW', statistics.dataskew
      sxaddpar, mainheader, 'DATAKURT', statistics.kurtosis
      sxaddpar, mainheader, 'DATAP01', statistics.datap01
      sxaddpar, mainheader, 'DATAP10', statistics.datap10
      sxaddpar, mainheader, 'DATAP25', statistics.datap25
      sxaddpar, mainheader, 'DATAP75', statistics.datap75
      sxaddpar, mainheader, 'DATAP90', statistics.datap90
      sxaddpar, mainheader, 'DATAP95', statistics.datap95
      sxaddpar, mainheader, 'DATAP98', statistics.datap98
      sxaddpar, mainheader, 'DATAP99', statistics.datap99
      ;
      ;      sxaddpar, mainheader, 'NEXP_PRP', float(nsteps)/float((*(rastersSJI[nrFW].rastercrs[nrCRS])).rasterPos)
      sxaddpar, mainheader, 'NEXP', nfiles;nsteps
      ;      sxaddpar, mainheader, 'NEXPOBS', l1to2log.filesexpected
      ;      sxaddpar, mainheader, 'NRASTERP', (*(rastersSJI[nrFW].rastercrs[nrCRS])).rasterPos
      ;      sxaddpar, mainheader, 'RASTYPDX', 1;nrCRS+1
      ;      sxaddpar, mainheader, 'RASTYPNX', 1;rastersSJI[nrFW].nrasters
      ;      sxaddpar, mainheader, 'RASRPT', 1
      ;      sxaddpar, mainheader, 'RASNRPT', 1
      ;      sxaddpar, mainheader, 'CADPL_AV', cadplav
      ;      sxaddpar, mainheader, 'CADPL_DV', cadpldv
      ;      sxaddpar, mainheader, 'CADEX_AV', cadexav
      ;      sxaddpar, mainheader, 'CADEX_DV', cadexdv
      ;      sxaddpar, mainheader, 'MISSOBS', l1to2log.nmissing
      sxaddpar, mainheader, 'MISSRAS', 0
      ;
      sxaddpar, mainheader, 'PC1_1', mean(pc1_1);OBSvars.PC1_1
      sxaddpar, mainheader, 'PC1_2', mean(pc1_2);OBSvars.PC1_2
      sxaddpar, mainheader, 'PC2_1', mean(pc2_1);OBSvars.PC2_1
      sxaddpar, mainheader, 'PC2_2', mean(pc2_2);OBSvars.PC2_2
      sxaddpar, mainheader, 'PC3_1', mean(pc3_1);OBSvars.PC3_1
      sxaddpar, mainheader, 'PC3_2', mean(pc3_2);OBSvars.PC3_2

      sxaddpar, mainheader, 'NWIN', 1

      ;add window-specific keywords with counter to mainheader
      number=string(1, format='(I1)')
      sxaddpar, mainheader, 'TDET'+number, 'SJI';OBSvars.SJIdet[0]
      if required_tags(hdrall, /WAVE_STR) then WAVE_STR=hdrall.WAVE_STR $
      else WAVE_STR=''
      sxaddpar, mainheader, 'TDESC'+number, IRISl12_mostcommonvalue(WAVE_STR);OBSvars.SJIdesc[0]
      if required_tags(hdrall, /WAVELNTH) then WAVELNTH=hdrall.WAVELNTH $
      else WAVELNTH=0
      sxaddpar, mainheader, 'TWAVE'+number, IRISl12_mostcommonvalue(WAVELNTH);OBSvars.SJIwave[0]
      ;      sxaddpar, mainheader, 'TWMIN'+number, OBSvars.SJIwavemin[0]
      ;      sxaddpar, mainheader, 'TWMAX'+number, OBSvars.SJIwavemax[0]
      sxaddpar, mainheader, 'TDMEAN'+number, statistics.datamean
      sxaddpar, mainheader, 'TDRMS'+number, statistics.datarms
      sxaddpar, mainheader, 'TDMEDN'+number, statistics.datamedn
      sxaddpar, mainheader, 'TDMIN'+number, statistics.datamin
      sxaddpar, mainheader, 'TDMAX'+number, statistics.datamax
      sxaddpar, mainheader, 'TDVALS'+number, statistics.datavals
      sxaddpar, mainheader, 'TMISSV'+number, 0;cmissing
      sxaddpar, mainheader, 'TSATPX'+number, 0;csaturated
      sxaddpar, mainheader, 'TSPIKE'+number, 0
      sxaddpar, mainheader, 'TTOTV'+number, statistics.datavals;+cmissing+csaturated+0
      sxaddpar, mainheader, 'TPCTD'+number, 100.0;float(statistics.vals) / (statistics.vals+cmissing+csaturated+0) *100
      sxaddpar, mainheader, 'TDSKEW'+number, statistics.dataskew
      sxaddpar, mainheader, 'TDKURT'+number, statistics.kurtosis
      sxaddpar, mainheader, 'TDP01_'+number, statistics.datap01
      sxaddpar, mainheader, 'TDP10_'+number, statistics.datap10
      sxaddpar, mainheader, 'TDP25_'+number, statistics.datap25
      sxaddpar, mainheader, 'TDP75_'+number, statistics.datap75
      sxaddpar, mainheader, 'TDP90_'+number, statistics.datap90
      sxaddpar, mainheader, 'TDP95_'+number, statistics.datap95
      sxaddpar, mainheader, 'TDP98_'+number, statistics.datap98
      sxaddpar, mainheader, 'TDP99_'+number, statistics.datap99
      sxaddpar, mainheader, 'TSR'+number, 1;OBSvars.tsr+1
      sxaddpar, mainheader, 'TER'+number, naxis2;OBSvars.ter+1
      sxaddpar, mainheader, 'TSC'+number, 1;OBSvars.SJIsc_rtype[0]+1
      sxaddpar, mainheader, 'TEC'+number, naxis1;OBSvars.SJIec_rtype[0]+1
      ;
      ;      ;sxaddpar, mainheader, 'CCDTYPE', 'SJI'
      ;
      fxaddpar, mainheader, 'CDELT1', cdelt1;OBSvars.CDELT1SJI     ;image scale in the x-direction
      fxaddpar, mainheader, 'CDELT2', cdelt2;OBSvars.CDELT2SJI     ;image scale in the y-direction
      fxaddpar, mainheader, 'CDELT3', cdelt3      ;slit width for FUV,NUV


      fxaddpar, mainheader, 'CRPIX1', crpix1      ;CRPIX1: location of sun/wave center in CCD x direction
      fxaddpar, mainheader, 'CRPIX2', crpix2      ;CRPIX2: location of sun/wave center in CCD y direction
      fxaddpar, mainheader, 'CRPIX3', crpix3;OBSvars.CRPIX3SJI      ;"1" for FUV/NUV

      ;fxaddpar, mainheader, 'CRS_TYPE', hdr[sjiind[0]].CRS_TYPE   ;CRS Type

      fxaddpar, mainheader, 'CRVAL1', xcenall     ;SOLARX (SJI), wavelength (FUV&NUV)
      fxaddpar, mainheader, 'CRVAL2', ycenall     ;SOLARY
      fxaddpar, mainheader, 'CRVAL3', timeall;OBSvars.CRVAL3SJI     ;SOLARX (FUV/NUV), time (SJI)

      fxaddpar, mainheader, 'CTYPE1', 'HPLN-TAN' ;IRISl12_mostcommonvalue(hdrall.CTYPE1)     ;HPLN-TAN (SOLARX); WAVE for FUV/NUV
      fxaddpar, mainheader, 'CTYPE2', 'HPLT-TAN' ;IRISl12_mostcommonvalue(hdrall.CTYPE2)     ;HPLT-TAN (SOLARY)
      fxaddpar, mainheader, 'CTYPE3', 'Time'     ;HPLN-TAN (SOLARX) for FUV/NUV

      fxaddpar, mainheader, 'CUNIT1', IRISl12_mostcommonvalue(hdrall.cunit1);OBSvars.CUNIT1SJI     ;arcsec for SJI, Angstrom for second FUV CCD
      fxaddpar, mainheader, 'CUNIT2', IRISl12_mostcommonvalue(hdrall.cunit2);OBSvars.CUNIT2SJI
      fxaddpar, mainheader, 'CUNIT3', 'seconds'

      if required_tags(hdrall, /KEYWDDOC) then KEYWDDOC=hdrall.KEYWDDOC $
      else KEYWDDOC=''
      sxaddpar, mainheader, 'KEYWDDOC', IRISl12_mostcommonvalue(KEYWDDOC)
      if required_tags(hdrall, /HISTORY) then HISTORY=hdrall[0].HISTORY $
      else HISTORY=''
      for i=0,N_ELEMENTS(HISTORY)-1 do begin
        if strcompress(HISTORY[i], /remove_all) ne '' then sxaddpar, mainheader, 'HISTORY', HISTORY[i]
      endfor
      sxaddpar, mainheader, 'HISTORY', 'IRIS-AIA RF2'



      file = outdir+'aia_l2_'+obs2fov->get_obsid()+'_'+waves[iwave]+'.fits'
      writefits, file, aiacube, mainheader

      ;write extension with auxiliary data (time, PZTX, PZTY, Exposure duration, etc.)
      auxdata = dblarr(20, N_ELEMENTS(hdrall))
      auxdata[ 0, *] = timeaia
      auxdata[ 1, *] = 0;pztx
      auxdata[ 2, *] = 0;pzty
      auxdata[ 3, *] = exptime;exptime
      auxdata[ 4, *] = 0;slitx
      auxdata[ 5, *] = 0;slity
      auxdata[ 6, *] = 1;sumsptrlSJI
      auxdata[ 7, *] = 1;sumspatSJI
      auxdata[ 8, *] = lvl_num;DataSrcSJI
      auxdata[ 9, *] = 0;LUTIDsji
      auxdata[10, *] = crval1aia
      auxdata[11, *] = crval2aia
      auxdata[12, *] = 0;obs_vr
      auxdata[13, *] = 0;ophase
      auxdata[14, *] = pc1_1
      auxdata[15, *] = pc1_2
      auxdata[16, *] = pc2_1
      auxdata[17, *] = pc2_2
      auxdata[18, *] = pc3_1
      auxdata[19, *] = pc3_2
      mkhdr, header, auxdata, /image
      sxaddpar, header, 'TIME', 0, 'time of each exposure in s after start of OBS (rowindex)'
      sxaddpar, header, 'PZTX', 1, 'PZTX of each exposure in arcsec (rowindex)'
      sxaddpar, header, 'PZTY', 2, 'PZTY of each exposure in arcsec (rowindex)'
      sxaddpar, header, 'EXPTIMES', 3, 'SJI Exposure duration of each exposure in s (rowindex)'
      sxaddpar, header, 'SLTPX1IX', 4, 'Slit center in X of each exposure in window-pixels (rowindex)'
      sxaddpar, header, 'SLTPX2IX', 5, 'Slit center in Y of each exposure in window-pixels (rowindex)'
      sxaddpar, header, 'SUMSPTRS', 6, 'SJI spectral summing (rowindex)'
      sxaddpar, header, 'SUMSPATS', 7, 'SJI spatial summing (rowindex)'
      sxaddpar, header, 'DSRCSIX', 8, 'SJI data source level (rowindex)'
      sxaddpar, header, 'LUTIDS', 9, 'SJI LUT ID (rowindex)'
      sxaddpar, header, 'XCENIX', 10, 'XCEN (rowindex)'
      sxaddpar, header, 'YCENIX', 11, 'YCEN (rowindex)'
      sxaddpar, header, 'OBS_VRIX', 12, 'Speed of observer in radial direction (rowindex)'
      sxaddpar, header, 'OPHASEIX', 13, 'Orbital phase (rowindex)'
      sxaddpar, header, 'PC1_1IX', 14, 'PC1_1 (rowindex)'
      sxaddpar, header, 'PC1_2IX', 15, 'PC1_2 (rowindex)'
      sxaddpar, header, 'PC2_1IX', 16, 'PC2_1 (rowindex)'
      sxaddpar, header, 'PC2_2IX', 17, 'PC2_2 (rowindex)'
      sxaddpar, header, 'PC3_1IX', 18, 'PC3_1 (rowindex)'
      sxaddpar, header, 'PC3_2IX', 19, 'PC3_2 (rowindex)'
      writefits, file, auxdata, header, /append










      FRMid=strarr(nfiles)
      FDBid=strarr(nfiles)
      CRSid=strarr(nfiles)
      filenameSJI=aiafiles



      ;now let's add another extension with a few string vectors
      lFRMid = max(strlen(FRMid))
      lFDBid = max(strlen(FDBid))
      lCRSid = max(strlen(CRSid))
      lfilenameSJI = max(strlen(filenameSJI))
      ltot = lFRMid + lFDBid + lCRSid + lfilenameSJI

      if ltot gt 0 then begin
        ftcreate, ltot, nfiles, header, auxdata2

        for step=0,nfiles-1 do begin
          start=0
          if strlen(FRMid[step]) gt 0 then $
            auxdata2[start:start+strlen(FRMid[step])-1, step] = byte(FRMid[step])
          start=start+lFRMid
          if strlen(FDBid[step]) gt 0 then $
            auxdata2[start:start+strlen(FDBid[step])-1, step] = byte(FDBid[step])
          start=start+lFDBid
          if strlen(CRSid[step]) gt 0 then $
            auxdata2[start:start+strlen(CRSid[step])-1, step] = byte(CRSid[step])
          start=start+lCRSid
          if strlen(filenameSJI[step]) gt 0 then $
            auxdata2[start:start+strlen(filenameSJI[step])-1, step] = byte(filenameSJI[step])
        endfor

      endif else ftcreate, 1, nfiles, header, auxdata2

      sxaddpar, header, 'TFIELDS', 4
      ix=0
      sxaddpar, header, 'TBCOL1', ix+1
      sxaddpar, header, 'TFORM1', 'A'+strcompress(string(lFRMid),/remove_all)
      sxaddpar, header, 'TTYPE1', 'FRMID'
      sxaddpar, header, 'FRMID', ix, 'FRM ID of each exposure (rowindex)'
      sxaddpar, header, 'LFRMID', lFRMid, 'Length of FRM ID (rows)'
      ix=ix+lFRMid
      sxaddpar, header, 'TBCOL2', ix+1
      sxaddpar, header, 'TFORM2', 'A'+strcompress(string(lFDBid),/remove_all)
      sxaddpar, header, 'TTYPE2', 'SJIFDBID'
      sxaddpar, header, 'FDBIDS', ix, 'SJI FDB ID of each exposure (rowindex)'
      sxaddpar, header, 'LFDBIDS', lFDBid, 'Length of SJI FDB ID (rows)'
      ix=ix+lFDBid
      sxaddpar, header, 'TBCOL3', ix+1
      sxaddpar, header, 'TFORM3', 'A'+strcompress(string(lCRSid),/remove_all)
      sxaddpar, header, 'TTYPE3', 'SJICRSID'
      sxaddpar, header, 'CRSIDS', ix, 'SJI CRS ID of each exposure (rowindex)'
      sxaddpar, header, 'LCRSIDS', lCRSid, 'Length of SJI CRS ID (rows)'
      ix=ix+lCRSid
      sxaddpar, header, 'TBCOL4', ix+1
      sxaddpar, header, 'TFORM4', 'A'+strcompress(string(lfilenameSJI),/remove_all)
      sxaddpar, header, 'TTYPE4', 'SJIfilename'
      sxaddpar, header, 'FILES', ix, 'SJI filename of each exposure (rowindex)'
      sxaddpar, header, 'LFILES', lfilenameSJI, 'Length of SJI filename (rows)'

      writefits, file, auxdata2, header, /append

      t2=systime(1)
      aialog.twrite = aialog.twrite + t2-t1
      aialog.filescreated = aialog.filescreated + 1



      if keyword_set(deletetempfiles) && method ne 2 then file_delete,aiafiles


    endif ;files found for this wavelength
  endfor ;iwave

  if keyword_set(deletetempfiles) && method ne 2 then file_delete,aiadir+'original/'

end
