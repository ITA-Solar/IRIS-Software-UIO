;+
; NAME:
;       IRIS_l1to2SotSP
;
; PURPOSE:
;       IRIS_l1to2SotSP transforms SOT SP product files into l2-IRIS-like files
;
; CATEGORY:
;       Martin Wiesmann / IRIS Data processing
;       IRIS_getSotSPdata
;
; CALLING SEQUENCE:
;       IRIS_l1to2SotSP, sot_sp_events, files, outdir, obs2fov, sotsplog=sotsplog [, fullfov=fullfov, deletetempfiles=deletetempfiles, $
;       debug=debug, _extra=_extra]
;
; INPUTS:
;       sot_sp_events: modified version of result from event download for SOT SP, provided by IRIS_getSotSPdata
;       files: a pointer array, one pointer per event, each pointer points to a string array of filenames
;       outdir: The path into which the resulting files should be saved.
;       obs2fov: An obs2fov object with the IRIS OBS as input
;       sotsplog: a structure containing some log information, provided by IRIS_processSotSPrequest
;
; OPTIONAL KEYWORD PARAMETERS:
;       fullfov: If set, the original FOV of the HINODE umode will be transfered, regardless of IRIS FOV
;       deletetempfiles: if set, the downloaded files will be deleted after use
;       _extra: not used
;
; OUTPUTS:
;
; CALLS:
;       This process is not called by the user, but by IRIS_processSotSPrequest
;
; COMMON BLOCKS:
;
; RESTRICTIONS:
;
; MODIFICATION HISTORY:
;       31-Oct-2019: Martin Wiesmann (ITA, UIO)
;
; $Id: iris_l1to2sotsp.pro,v 1.6 2020/01/15 14:44:20 mawiesma Exp $  ;



pro IRIS_l1to2SotSP, sot_sp_events, files, outdir, obs2fov, fullfov=fullfov, deletetempfiles=deletetempfiles, $
  debug=debug, sotsplog=sotsplog, _extra=_extra, view_images=view_images

  version = 'S2I-2019-10-31'
  sotsplog.version = version

  GET_UTC, Date_RF2, /CCSDS

  for ievent=0,N_ELEMENTS(sot_sp_events)-1 do begin

    help,sot_sp_events[ievent]
    files_event = *files[ievent]
    nmodes = N_ELEMENTS(files_event)
    sotsplog.filesreceived = sotsplog.filesreceived + nmodes

    for imode=0,nmodes-1 do begin

      t1=systime(1)
      IRIS_rdfits_struct, files_event[imode], fcontent, n_extensions=n_extensions, /pointer_array, /fitsheadstruct
      t2=systime(1)
      sotsplog.tread = sotsplog.tread + t2-t1

      infile = file_basename(files_event[imode])
      infile = strmid(infile, 8)
      file = outdir + 'sotsp_l2_' + obs2fov->get_obsid() + '_' + infile

      for i=1,n_extensions do begin
        if i eq 1 then begin
          hdrall = *fcontent.hdr[i]
          data = *fcontent.data[i]
        endif else begin
          hdrall = [hdrall, *fcontent.hdr[i]]
          data = [ [[data]], [[*fcontent.data[i] ]] ]
        endelse
      endfor
      nImages = n_extensions

      ;;;; TEST
      test = 0
      if test then begin
        if n_extensions eq 1 then begin
          for i=0,3 do begin
            hdrall = [hdrall, *fcontent.hdr[1]]
            data = [[[data]], [[*fcontent.data[1]]]]
          endfor
        endif
        nImages = N_ELEMENTS(hdrall)
      endif
      ;;;; TEST

      startobs = obs2fov->get_startobs()
      endobs = obs2fov->get_endobs()
      tstart = str2utc(startobs)
      tend = str2utc(endobs)
      tend = (tend.mjd-tstart.mjd)*86400L + (tend.time-tstart.time)/1000d
      timehinode = str2utc(hdrall.date_obs)
      timehinode = (timehinode.mjd-tstart.mjd)*86400L + (timehinode.time-tstart.time)/1000d
      tstart_hinode = str2utc(hdrall.tstart)
      tstart_hinode = (tstart_hinode.mjd-tstart.mjd)*86400L + (tstart_hinode.time-tstart.time)/1000d
      tend_hinode = str2utc(hdrall.tend)
      tend_hinode = (tend_hinode.mjd-tstart.mjd)*86400L + (tend_hinode.time-tstart.time)/1000d
      timeexpand = obs2fov->get_timeexpand()
      t0 = timeexpand[0] * (-60)
      t1 = timeexpand[1] * 60 + tend
      within_timewindow = intarr(nImages)
      for i=0,nImages-1 do begin
        if i gt 0 then begin
          ;calculate beginning of exposure of this extension/image
          tstart_hinode[i] = (timehinode[i] + timehinode[i-1]) / 2.0
        endif
        if i lt nImages-1 then begin
          ;calculate end of exposure of this extension/image
          tend_hinode[i] = (timehinode[i] + timehinode[i+1]) / 2.0
        endif
        ;check if this exposure of this extension/image overlaps with IRIS time window
        if tstart_hinode[i] lt t0 then begin
          if tend_hinode[i] gt t0 then within_timewindow[i] = 1
        endif else if tstart_hinode[i] lt t1 then within_timewindow[i] = 1
        ;if timehinode[i] gt t1 then within_timewindow[i] = 0
        ;if i lt nImages-1 then begin
        ;  if timehinode[i+1] lt t0 then within_timewindow[i] = 0
        ;endif
      endfor
      ind = where(within_timewindow eq 1, count)
      if count eq 0 then begin
        ; all images are outside of the desired timewindow
        box_message,'no images are within IRIS time window (including extension)'
        sotsplog.filesunused = sotsplog.filesunused + 1
        continue
      endif
      if N_ELEMENTS(ind) ne nImages then begin
        hdrall = hdrall[ind]
        data = data[*,*,ind]
        nImages = N_ELEMENTS(ind)
        timehinode = timehinode[ind]
      endif

      ;calculate basic step size
      if nImages gt 1 then begin
        cdelt3 = dblarr(nImages-1)
        for i=1,nImages-1 do begin
          cdelt3[i-1] = timehinode[i] - timehinode[i-1]
        endfor
      endif else cdelt3=0
      cdelt3 = mean(cdelt3)


      crval1hinode = hdrall.crval1
      crval2hinode = hdrall.crval2
      cdelt1 = IRISl12_mostcommonvalue(hdrall.cdelt1)
      cdelt2 = IRISl12_mostcommonvalue(hdrall.cdelt2)
      sumsptrl = hdrall.SCN_SUM
      sumspat = hdrall.SCN_SUM
      crota2orig = hdrall.crota2
      obs2fov->get_xycenreg, regxslope, regxconst, regyslope, regyconst


      pc1_1 = fltarr(nImages)
      pc1_2 = fltarr(nImages)
      pc2_1 = fltarr(nImages)
      pc2_2 = fltarr(nImages)
      pc3_1 = fltarr(nImages)
      pc3_2 = fltarr(nImages)

      if keyword_set(fullfov) then begin
        hinodecube = data
        fovx = mean(hdrall.fovx)
        fovy = mean(hdrall.fovy)
        crpix1 = IRISl12_mostcommonvalue(hdrall.CRPIX1)
        crpix2 = IRISl12_mostcommonvalue(hdrall.CRPIX2)
        wcshinode = fitshead2wcs(hdrall[0])
        pc1_1[*] = wcshinode.pc[0,0]
        pc1_2[*] = wcshinode.pc[0,1]
        pc2_1[*] = wcshinode.pc[1,0]
        pc2_2[*] = wcshinode.pc[1,1]

      endif else begin

        fovexpand = obs2fov->get_fovexpand()
        naxis1 = ceil((obs2fov->get_fovx()+fovexpand[0])/cdelt1)
        naxis2 = ceil((obs2fov->get_fovy()+fovexpand[1])/cdelt2)
        crpix1 = (naxis1+1)/2.0
        crpix2 = (naxis2+1)/2.0
        fovx = naxis1 * cdelt1
        fovy = naxis2 * cdelt2

        hinodecube = make_array(naxis1, naxis2, nImages, /float, value=!values.f_nan)
        sizedata = size(data)


        for i_image=0,nImages-1 do begin

          ;cut out the desired window
          ;get xcen/ycen from the regression result
          xcenhinode = regxconst + timehinode[i_image] * regxslope
          ycenhinode = regyconst + timehinode[i_image] * regyslope

          if keyword_set(view_images) then begin
            wcstemp = fitshead2wcs(hdrall[i_image])
            examine_image, data[*,*,i_image], wcstemp, 'Original'
            help,hdrall[i_image]
            stop
          endif



          ;first roll the image to iris roll angle
          ;datatemp=rot(data[*,*,i_image], obs2fov->get_rollangle()-crota2orig[i_image], 1, x, y, /pivot, cubic=-0.5, missing=0)
          sizetemp = ceil(1.5*sqrt(sizedata[1]*sizedata[1]+sizedata[2]*sizedata[2]))
          datatemp = make_array(sizetemp, sizetemp, /float, value=!values.f_nan)
          xshifttemp = fix((sizetemp-sizedata[1])/2.0)
          yshifttemp = fix((sizetemp-sizedata[2])/2.0)
          crpix1temp = hdrall[i_image].crpix1 + xshifttemp
          crpix2temp = hdrall[i_image].crpix2 + yshifttemp
          datatemp[xshifttemp:xshifttemp+sizedata[1]-1, yshifttemp:yshifttemp+sizedata[2]-1] = data[*,*,i_image]
          if keyword_set(view_images) then begin
            ;this step is good
            hdrtemp = hdrall[i_image]
            hdrtemp.naxis1 = sizetemp
            hdrtemp.naxis2 = sizetemp
            hdrtemp.crpix1 = crpix1temp
            hdrtemp.crpix2 = crpix2temp
            wcstemp = fitshead2wcs(hdrtemp)
            examine_image, datatemp, wcstemp, 'Original padded'
          endif
          datatemp = rot(datatemp, obs2fov->get_rollangle()-crota2orig[i_image], 1, crpix1temp, crpix2temp, /pivot, cubic=-0.5, missing=0)
          if keyword_set(view_images) then begin
            ;this step is good
            hdrtemp.crota1 = obs2fov->get_rollangle()
            hdrtemp.crota2 = obs2fov->get_rollangle()
            wcstemp = fitshead2wcs(hdrtemp)
            examine_image, datatemp, wcstemp, 'Original padded and rotated'
          endif



          ;get original hinode sat_rot first for calculations
          a=crota2orig[i_image]/!radeg

          crval1hinode[i_image] = xcenhinode
          crval2hinode[i_image] = ycenhinode

          hdrall[i_image].naxis1 = sizetemp;;;
          hdrall[i_image].naxis2 = sizetemp;;;
          hdrall[i_image].crpix1 = crpix1temp;;;
          hdrall[i_image].crpix2 = crpix2temp;;;
          hdrall[i_image].CROTA1 = obs2fov->get_rollangle()
          hdrall[i_image].CROTA2 = obs2fov->get_rollangle()
          wcshinode = fitshead2wcs(hdrall[i_image])
          pc1_1[i_image] = wcshinode.pc[0,0]
          pc1_2[i_image] = wcshinode.pc[0,1]
          pc2_1[i_image] = wcshinode.pc[1,0]
          pc2_2[i_image] = wcshinode.pc[1,1]



          ;calculate x/y coordinates of the four corners
          xx1start = hdrall[i_image].crval1 + hdrall[i_image].cdelt1 * (pc1_1[i_image] * (1-crpix1temp) $
            + pc1_2[i_image] * (1-crpix2temp))
          xx1end = hdrall[i_image].crval1 + hdrall[i_image].cdelt1 * (pc1_1[i_image] * (sizetemp-crpix1temp) $
            + pc1_2[i_image] * (1-crpix2temp))
          xx2start = hdrall[i_image].crval1 + hdrall[i_image].cdelt1 * (pc1_1[i_image] * (1-crpix1temp) $
            + pc1_2[i_image] * (sizetemp-crpix2temp))
          xx2end = hdrall[i_image].crval1 + hdrall[i_image].cdelt1 * (pc1_1[i_image] * (sizetemp-crpix1temp) $
            + pc1_2[i_image] * (sizetemp-crpix2temp))
          yy1start = hdrall[i_image].crval2 + hdrall[i_image].cdelt2 * (pc2_2[i_image] * (1-crpix2temp) $
            + pc2_1[i_image] * (1-crpix1temp))
          yy1end = hdrall[i_image].crval2 + hdrall[i_image].cdelt2 * (pc2_2[i_image] * (sizetemp-crpix2temp) $
            + pc2_1[i_image] * (1-crpix1temp))
          yy2start = hdrall[i_image].crval2 + hdrall[i_image].cdelt2 * (pc2_2[i_image] * (1-crpix2temp) $
            + pc2_1[i_image] * (sizetemp-crpix1temp))
          yy2end = hdrall[i_image].crval2 + hdrall[i_image].cdelt2 * (pc2_2[i_image] * (sizetemp-crpix2temp) $
            + pc2_1[i_image] * (sizetemp-crpix1temp))

          ;calculate where x/y are at the edges
          x1 = (xcenhinode-xx1start) / (xx1end-xx1start) * (sizetemp-1)
          y1 = 0.0
          x2 = (xcenhinode-xx2start) / (xx2end-xx2start) * (sizetemp-1)
          y2 = float(sizetemp-1)
          x3 = 0.0
          y3 = (ycenhinode-yy1start) / (yy1end-yy1start) * (sizetemp-1)
          x4 = float(sizetemp-1)
          y4 = (ycenhinode-yy2start) / (yy2end-yy2start) * (sizetemp-1)

          ;calculate point of intersection
          x = ((x4-x3) * (x2*y1 - x1*y2) - (x2-x1) * (x4*y3 - x3*y4)) / $
            ((y4-y3) * (x2-x1) - (y2-y1) * (x4-x3))
          y = ((y1-y2) * (x4*y3 - x3*y4) - (y3-y4) * (x2*y1 - x1*y2)) / $
            ((y4-y3) * (x2-x1) - (y2-y1) * (x4-x3))

          if keyword_set(view_images) then begin
            ;this step is now also good
            examine_image, datatemp, wcstemp, 'Original padded and rotated with x and y', show_coord=[x,y]
            box_message,['        pixel   arcsec', $
              'X : '+string(x, format='(F9.2)')+string(xcenhinode, format='(F9.2)'), $
              'Y : '+string(y, format='(F9.2)')+string(ycenhinode, format='(F9.2)')]
            c = wcs_get_coord(wcshinode, [x, y])
            print,xcenhinode,ycenhinode
            print,c
            stop
          endif


          ;calculate the start and end of the window
          xs=x-naxis1/2.0
          ys=y-naxis2/2.0
          ;sub pixel shift
          ;shiftxsub to the left
          xss = fix(xs)
          shiftxsub = xs-xss
          pcoeff = [shiftxsub,0,1,0]
          xstart = xss
          xend = xstart+naxis1-1
          xstartcube = 0
          xendcube = naxis1-1
          if xstart lt 0 then begin
            xstartcube = xstartcube - xstart
            xstart = 0
          endif
          if xstart ge sizetemp then begin
            box_message,['no overlap of HINODE and IRIS',$
              string(i_image)+'  '+files_event[imode],$
              'xstart: '+string(xstart),$
              'x/ycen',$
              'IRIS:   '+string(xcenhinode,format='(f7.1)')+'  '+string(ycenhinode,format='(f7.1)')+$
              '  FOV: '+string(fovx,format='(f7.1)')+'  '+string(fovy,format='(f7.1)'),$
              'HINODE: '+string(hdrall[i_image].crval1,format='(f7.1)')+'  '+string(hdrall[i_image].crval2,format='(f7.1)')+$
              '  CRPIX HINODE: '+string(hdrall[i_image].crpix1,format='(f7.1)')+'  '+string(hdrall[i_image].crpix2,format='(f7.1)')]
            continue
          endif
          if xend lt 0 then begin
            box_message,['no overlap of HINODE and IRIS',$
              string(i_image)+'  '+files_event[imode],$
              'xend: '+string(xend),$
              'x/ycen',$
              'IRIS:   '+string(xcenhinode,format='(f7.1)')+'  '+string(ycenhinode,format='(f7.1)')+$
              '  FOV: '+string(fovx,format='(f7.1)')+'  '+string(fovy,format='(f7.1)'),$
              'HINODE: '+string(hdrall[i_image].crval1,format='(f7.1)')+'  '+string(hdrall[i_image].crval2,format='(f7.1)')+$
              '  CRPIX HINODE: '+string(hdrall[i_image].crpix1,format='(f7.1)')+'  '+string(hdrall[i_image].crpix2,format='(f7.1)')]
            continue
          endif
          if xend ge sizetemp then begin
            xendcube = xendcube - (xend - (sizetemp-1))
            xend = sizetemp-1
          endif
          if xstart eq xend then begin
            box_message,['no overlap of HINODE and IRIS',$
              string(i_image)+'  '+files_event[imode],$
              'xstart=xend '+string(xstart)+'  '+string(xend),$
              'x/ycen',$
              'IRIS:   '+string(xcenhinode,format='(f7.1)')+'  '+string(ycenhinode,format='(f7.1)')+$
              '  FOV: '+string(fovx,format='(f7.1)')+'  '+string(fovy,format='(f7.1)'),$
              'HINODE: '+string(hdrall[i_image].crval1,format='(f7.1)')+'  '+string(hdrall[i_image].crval2,format='(f7.1)')+$
              '  CRPIX HINODE: '+string(hdrall[i_image].crpix1,format='(f7.1)')+'  '+string(hdrall[i_image].crpix2,format='(f7.1)')]
            continue
          endif
          ;shiftysub downwards
          yss = fix(ys)
          shiftysub = ys-yss
          qcoeff =[shiftysub,1,0,0]
          ystart=yss
          yend=ystart+naxis2-1
          ystartcube = 0
          yendcube = naxis2-1
          if ystart lt 0 then begin
            ystartcube = ystartcube - ystart
            ystart = 0
          endif
          if ystart ge sizetemp then begin
            box_message,['no overlap of HINODE and IRIS',$
              string(i_image)+'  '+files_event[imode],$
              'ystart: '+string(ystart),$
              'x/ycen',$
              'IRIS:   '+string(xcenhinode,format='(f7.1)')+'  '+string(ycenhinode,format='(f7.1)')+$
              '  FOV: '+string(fovx,format='(f7.1)')+'  '+string(fovy,format='(f7.1)'),$
              'HINODE: '+string(hdrall[i_image].crval1,format='(f7.1)')+'  '+string(hdrall[i_image].crval2,format='(f7.1)')+$
              '  CRPIX HINODE: '+string(hdrall[i_image].crpix1,format='(f7.1)')+'  '+string(hdrall[i_image].crpix2,format='(f7.1)')]
            continue
          endif
          if yend lt 0 then begin
            box_message,['no overlap of HINODE and IRIS',$
              string(i_image)+'  '+files_event[imode],$
              'yend: '+string(yend),$
              'x/ycen',$
              'IRIS:   '+string(xcenhinode,format='(f7.1)')+'  '+string(ycenhinode,format='(f7.1)')+$
              '  FOV: '+string(fovx,format='(f7.1)')+'  '+string(fovy,format='(f7.1)'),$
              'HINODE: '+string(hdrall[i_image].crval1,format='(f7.1)')+'  '+string(hdrall[i_image].crval2,format='(f7.1)')+$
              '  CRPIX HINODE: '+string(hdrall[i_image].crpix1,format='(f7.1)')+'  '+string(hdrall[i_image].crpix2,format='(f7.1)')]
            continue
          endif
          if yend ge sizetemp then begin
            yendcube = yendcube - (yend - (sizetemp-1))
            yend = sizetemp-1
          endif
          if ystart eq yend then begin
            box_message,['no overlap of HINODE and IRIS',$
              string(i_image)+'  '+files_event[imode],$
              'ystart=yend '+string(ystart)+'  '+string(yend),$
              'x/ycen',$
              'IRIS:   '+string(xcenhinode,format='(f7.1)')+'  '+string(ycenhinode,format='(f7.1)')+$
              '  FOV: '+string(fovx,format='(f7.1)')+'  '+string(fovy,format='(f7.1)'),$
              'HINODE: '+string(hdrall[i_image].crval1,format='(f7.1)')+'  '+string(hdrall[i_image].crval2,format='(f7.1)')+$
              '  CRPIX HINODE: '+string(hdrall[i_image].crpix1,format='(f7.1)')+'  '+string(hdrall[i_image].crpix2,format='(f7.1)')]
            continue
          endif

          if keyword_set(view_images) then begin
            ;testing, show original data with cutout window and new xcen/ycen
            window,0,xs=sizedata[1]*1.3,ys=sizedata[2]*1.3, retain=2
            pih,datatemp,xstyle=5,ystyle=5
            oplot,[xstart,xend],[ystart,ystart],color=250
            oplot,[xstart,xend],[yend,yend],color=250
            oplot,[xstart,xstart],[ystart,yend],color=250
            oplot,[xend,xend],[ystart,yend],color=250
            oplot,[0,sizetemp],[y,y],color=250
            oplot,[x,x],[0,sizetemp],color=250
            ;a=45/!radeg
            axis, xaxis=0, xrange=[xx1start,xx1end],xstyle=1
            axis, yaxis=0, yrange=[yy1start,yy1end],ystyle=1
            axis, xaxis=1, xrange=[xx2start,xx2end],xstyle=1
            axis, yaxis=1, yrange=[yy2start,yy2end],ystyle=1
            ;stop
          endif


          ;do the subpixel shift
          hinodecube[xstartcube:xendcube, ystartcube:yendcube, i_image] = INF_POLY_2D(datatemp[xstart:xend,ystart:yend],pcoeff,qcoeff,2,cubic=-0.5)

          if keyword_set(view_images) then begin
            hdrtemp.naxis1 = naxis1
            hdrtemp.naxis2 = naxis2
            hdrtemp.crpix1 = crpix1
            hdrtemp.crpix2 = crpix2
            hdrtemp.crval1 = crval1hinode[i_image]
            hdrtemp.crval2 = crval2hinode[i_image]
            hdrtemp = rem_tag(hdrtemp, ['crota1','crota2'])
            hdrtemp = create_struct(hdrtemp, 'PC1_1', pc1_1[i_image], 'PC1_2', pc1_2[i_image], $
              'PC2_1', pc2_1[i_image], 'PC2_2', pc2_2[i_image])
            wcstemp = fitshead2wcs(hdrtemp)
            examine_image, hinodecube[*,*,i_image], wcstemp, 'Hinode cube'
            stop
          endif



        endfor ;i_image=0,nImages-1
      endelse ;~fullfov

      ; ignore this data cube if all pixels are NAN, i.e no overlap with IRIS
      ind = where(hinodecube eq hinodecube, count)
      if count eq 0 then begin
        box_message, 'only NAN in the data, skipping this'
        continue
      endif

      ind = floor(nImages/2)
      xcenall = crval1hinode[ind]
      ycenall = crval2hinode[ind]
      timeall = timehinode[ind]
      crpix3 = ind+1

      statistics = iris_cube_statistics(hinodecube)


      ;calculate percentage of coverage
      ;SJI
      xpixel1 = fovexpand[0]/cdelt1/2.0
      xpixel2 = ceil(naxis1-1-xpixel1)
      xpix1 = fix(xpixel1)
      ypixel1 = fovexpand[1]/cdelt2/2.0
      ypixel2 = ceil(naxis2-1-ypixel1)
      ypixel1 = fix(ypixel1)
      ind = where(hinodecube[xpix1:xpixel2, ypixel1:ypixel2, *] EQ hinodecube[xpix1:xpixel2, ypixel1:ypixel2, *], countrealdata)
      PSJIADAT = 100.0 * countrealdata / ((xpixel2-xpix1+1)*(ypixel2-ypixel1+1)*nImages)

      ;raster
      slitpix = obs2fov->get_slitpix() / cdelt1
      xpix1 = xpixel1 + fix(slitpix[0])
      xpix2 = xpixel1 + ceil(slitpix[1])
      ind = where(hinodecube[xpix1:xpix2, ypixel1:ypixel2, *] EQ hinodecube[xpix1:xpix2, ypixel1:ypixel2, *], countrealdata)
      PRASADAT = 100.0 * countrealdata / ((xpix2-xpix1+1)*(ypixel2-ypixel1+1)*nImages)

      ;calculate percentage of coverage in time
      tmin = min(tstart_hinode)
      tmax = max(tend_hinode)
      if tmin lt 0 then tmin=0
      if tmax gt tend then tmax=tend
      timepercentage = 100.0 * (tmax-tmin) / tend


      t1=systime(1)
      ;first region is written into primary block
      mkhdr, mainheader, hinodecube, /extend
      ;add some information to the primary header

      sdata = size(hinodecube)
      sxaddpar, mainheader, 'NAXIS', 3, 'Number of dimensions'
      sxaddpar, mainheader, 'NAXIS1', sdata[1]
      sxaddpar, mainheader, 'NAXIS2', sdata[2]
      sxaddpar, mainheader, 'NAXIS3', nImages
      if required_tags(hdrall, /telescop) then telescop=IRISl12_mostcommonvalue(hdrall.Telescop) $
      else telescop=''
      sxaddpar, mainheader, 'TELESCOP', telescop
      if required_tags(hdrall, /instrume) then instrume=IRISl12_mostcommonvalue(hdrall.instrume) $
      else instrume=''
      sxaddpar, mainheader, 'INSTRUME', instrume
      sxaddpar, mainheader, 'DATA_LEV', 2.0;mean(lvl_num);OBSvars.DATA_LEV
      sxaddpar, mainheader, 'LVL_NUM', 2.0;mean(lvl_num);OBSvars.LVL_NUM
      sxaddpar, mainheader, 'VER_RF2', version;OBSvars.version
      sxaddpar, mainheader, 'DATE_RF2', Date_RF2;OBSvars.Date_RF2
      if required_tags(hdrall, /DATA_LEV) then lvl_num=hdrall.DATA_LEV $
      else lvl_num=dblarr(N_ELEMENTS(hdrall))
      sxaddpar, mainheader, 'DATA_SRC', mean(lvl_num);OBSvars.DATA_SRC
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
      sxaddpar, mainheader, 'RASOLAP', sot_sp_events[ievent].rasterOverlap, 'PRASAHCR GT specified lower limit'
      sxaddpar, mainheader, 'PRASAHCR', sot_sp_events[ievent].rasterOverlapPercent, 'Percent raster area coverage, from HCR'
      sxaddpar, mainheader, 'PSJIAHCR', sot_sp_events[ievent].SjiOverlapPercent, 'Percent SJI area coverage, from HCR'
      sxaddpar, mainheader, 'PSOTAHCR', sot_sp_events[ievent].SotOverlapPercent, 'Percent SOTSP area coverage, from HCR'
      sxaddpar, mainheader, 'PRASADAT', PRASADAT, 'Percent raster area coverage, from data'
      sxaddpar, mainheader, 'PSJIADAT', PSJIADAT, 'Percent SJI area coverage, from data'
      sxaddpar, mainheader, 'PTIMELAP', timepercentage, 'Percent IRIS OBS coverage in time'
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
      sxaddpar, mainheader, 'SAT_ROT', obs2fov->get_satrot();mean(hdrall.crota2);satrot;OBSvars.SAT_ROT
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

      sxaddpar, mainheader, 'FOVY', fovy
      sxaddpar, mainheader, 'FOVX', fovx
      sxaddpar, mainheader, 'XCEN', xcenall
      sxaddpar, mainheader, 'YCEN', ycenall
      sxaddpar, mainheader, 'SUMSPTRL', IRISl12_mostcommonvalue(sumsptrl);OBSvars.sumspecmin
      sxaddpar, mainheader, 'SUMSPAT', IRISl12_mostcommonvalue(sumspat);OBSvars.sumspatmin
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
      sxaddpar, mainheader, 'NEXP', nImages;nsteps
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
      ;      sxaddpar, mainheader, 'MISSRAS', 0
      ;
      sxaddpar, mainheader, 'PC1_1', mean(pc1_1);OBSvars.PC1_1
      sxaddpar, mainheader, 'PC1_2', mean(pc1_2);OBSvars.PC1_2
      sxaddpar, mainheader, 'PC2_1', mean(pc2_1);OBSvars.PC2_1
      sxaddpar, mainheader, 'PC2_2', mean(pc2_2);OBSvars.PC2_2
      sxaddpar, mainheader, 'PC3_1', mean(pc3_1);OBSvars.PC3_1
      sxaddpar, mainheader, 'PC3_2', mean(pc3_2);OBSvars.PC3_2

      ;        sxaddpar, mainheader, 'WAVEOFF', waves[indwave]
      sxaddpar, mainheader, 'SOTSTART', sot_sp_events[ievent].sotstart
      sxaddpar, mainheader, 'SOTSEND', sot_sp_events[ievent].sotend
      sxaddpar, mainheader, 'SOTEVENT', sot_sp_events[ievent].eventid
      link = strtrim(sot_sp_events[ievent].eventlink, 2)
      linklength = strlen(link)
      number=1
      maxlength=68
      while linklength gt 0 do begin
        sxaddpar, mainheader, 'SOTLINK'+string(number, format='(I1)'), strmid(link, 0, maxlength)
        if linklength gt maxlength then begin
          link = strmid(link, maxlength)
          linklength = strlen(link)
          number=number+1
        endif else linklength=0
      endwhile

      sxaddpar, mainheader, 'NWIN', 1

      ;add window-specific keywords with counter to mainheader
      number=string(1, format='(I1)')
      sxaddpar, mainheader, 'TDET'+number, 'SJI';OBSvars.SJIdet[0]
      if required_tags(hdrall, /WAVE) then WAVE_STR=hdrall.WAVE $
      else WAVE_STR=''
      sxaddpar, mainheader, 'TDESC'+number, IRISl12_mostcommonvalue(WAVE_STR);OBSvars.SJIdesc[0]
      if required_tags(hdrall, /WAVELNTH) then WAVELNTH=hdrall.WAVELNTH $
      else WAVELNTH=0
      sxaddpar, mainheader, 'TWAVE'+number, IRISl12_mostcommonvalue(WAVELNTH);OBSvars.SJIwave[0]
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
      sxaddpar, mainheader, 'TER'+number, (size(hinodecube))[2];OBSvars.ter+1
      sxaddpar, mainheader, 'TSC'+number, 1;OBSvars.SJIsc_rtype[0]+1
      sxaddpar, mainheader, 'TEC'+number, (size(hinodecube))[1];OBSvars.SJIec_rtype[0]+1

      fxaddpar, mainheader, 'CDELT1', cdelt1;OBSvars.CDELT1SJI     ;image scale in the x-direction
      fxaddpar, mainheader, 'CDELT2', cdelt2;OBSvars.CDELT2SJI     ;image scale in the y-direction
      fxaddpar, mainheader, 'CDELT3', cdelt3      ;slit width for FUV,NUV;;;;;;;;;;;;;;to be changed

      fxaddpar, mainheader, 'CRPIX1', crpix1      ;CRPIX1: location of sun/wave center in CCD x direction
      fxaddpar, mainheader, 'CRPIX2', crpix2      ;CRPIX2: location of sun/wave center in CCD y direction
      fxaddpar, mainheader, 'CRPIX3', crpix3;OBSvars.CRPIX3SJI      ;"1" for FUV/NUV

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
      sxaddpar, mainheader, 'HISTORY', 'IRIS-SOT RF2'

      writefits, file, hinodecube, mainheader



      ;write extension with auxiliary data (time, PZTX, PZTY, Exposure duration, etc.)
      auxdata = dblarr(20, N_ELEMENTS(hdrall))
      auxdata[ 0, *] = timehinode
      auxdata[ 1, *] = 0;pztx
      auxdata[ 2, *] = 0;pzty
      auxdata[ 3, *] = exptime;exptime
      auxdata[ 4, *] = 0;slitx
      auxdata[ 5, *] = 0;slity
      auxdata[ 6, *] = sumsptrl
      auxdata[ 7, *] = sumspat
      auxdata[ 8, *] = 1;lvl_num;DataSrcSJI
      auxdata[ 9, *] = 0;LUTIDsji
      auxdata[10, *] = crval1hinode
      auxdata[11, *] = crval2hinode
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




      FRMid=strarr(nImages)
      FDBid=strarr(nImages)
      CRSid=strarr(nImages)
      filenameSJI=strarr(nImages)
      filenameSJI[*] = files_event[imode]



      ;now let's add another extension with a few string vectors
      lFRMid = max(strlen(FRMid))
      lFDBid = max(strlen(FDBid))
      lCRSid = max(strlen(CRSid))
      lfilenameSJI = max(strlen(filenameSJI))
      ltot = lFRMid + lFDBid + lCRSid + lfilenameSJI

      if ltot gt 0 then begin
        ftcreate, ltot, nImages, header, auxdata2

        for step=0,nImages-1 do begin
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

      endif else ftcreate, 1, nImages, header, auxdata2

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
      sotsplog.twrite = sotsplog.twrite + t2-t1
      sotsplog.filescreated = sotsplog.filescreated + 1

      file_delete, files_event[imode]

    endfor ;imode=0,nmodes-1

  endfor ;ievent=0,N_ELEMENTS(sot_sp_events)-1

  if keyword_set(deletetempfiles) then file_delete, outdir+'original'

end
