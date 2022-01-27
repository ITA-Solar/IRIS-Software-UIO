; $Id: iris_l1to2hinode.pro,v 1.50 2017/12/11 09:58:36 mawiesma Exp $  ;

pro iris_l1to2hinode, umodes, files, outdir, obs2fov, fullfov=fullfov, deletetempfiles=deletetempfiles, $
  noprep=noprep, debug=debug, hinodelog=hinodelog, _extra=_extra
  ;debug=0
  version = 'H2I-2017-05-10'
  hinodelog.version = version

  GET_UTC, Date_RF2, /CCSDS


  for iumode=0,N_ELEMENTS(umodes)-1 do begin
    help,umodes[iumode]

    case umodes[iumode].obs_type of
      'FG (simple)': begin
        case umodes[iumode].wave of
          'CN bandhead 3883': doit=1
          'Ca II H line': doit=1
          'G band 4305': doit=1
          'blue cont 4504': doit=1
          'green cont 5550': doit=1
          'red cont 6684': doit=1
          'TF H I 6563': doit=1
          'TF H I 6563 base': doit=1
          else: doit=0
        endcase
     end

                                ;RPT fix, the inner method
                                ;iris_download hinode could do this /
                                ;- fix and then this case missed it.

      'FG MG4 V-I': doit=1

      'FG MG4 V/I': begin
        doit=1
        umodes[iumode].obs_type = 'FG MG4 V-I' ;needs to be changed, because filenames with / are a nuisance
      end

      'FG shuttered I and V': doit=2
      'FG shuttered IV+DG': doit=1
      'FG shutterless I and V': doit=2
      'FG shutterless I and V with 0.2s intervals': doit=2
      'FG shutterless Stokes': doit=2

      else: doit=0
    endcase

    if doit eq 0 then begin
      box_message, 'this mode is not (yet) implemented'
      continue
    endif


    hinodelog.filesreceived = hinodelog.filesreceived + umodes[iumode].files_win
    if umodes[iumode].files_win le 0 then continue

    file0 = outdir + 'sot_l2_' + obs2fov->get_obsid() + '_' + $
      strcompress(umodes[iumode].sotid,/remove_all) + '_' + $
;      strcompress(umodes[iumode].obs_type,/remove_all) + '_' + $
      strcompress(umodes[iumode].wave,/remove_all)
    ind = where((umodes.sotid eq umodes[iumode].sotid) AND $
      (umodes.obs_type eq umodes[iumode].obs_type) AND (umodes.wave eq umodes[iumode].wave), count)
    if count gt 1 then begin
      file0 = file0 + '_' + strcompress(string(umodes[iumode].naxis1),/remove_all) + 'x' + strcompress(string(umodes[iumode].naxis2),/remove_all)
    endif

    t1=systime(1)
    if keyword_set(noprep) then begin
      read_sot, *files[iumode], hdrall, dataall ; read only This mode ->
    endif else begin
      fg_prep, *files[iumode], -1, hdrall, dataall, _extra=_extra
    endelse
    t2=systime(1)
    hinodelog.tread = hinodelog.tread + t2-t1

    ;get information about the images
    doc = fg_documentation_label(hdrall, label=label, type=type, unit=unit)

    ndims = size(dataall)
    if umodes[iumode].files_win eq 1 then begin
      case ndims[0] of
        2: nimages=1
        3: nimages=ndims[3]
        else: nimages=0
      endcase
    endif else begin
      case ndims[0] of
        3: nimages=1
        4: nimages=ndims[3]
        else: nimages=0
      endcase
    endelse


    nfiles = N_ELEMENTS(hdrall)

    startobs = obs2fov->get_startobs()
    endobs = obs2fov->get_endobs()
    tstart = str2utc(startobs)
    tend = str2utc(endobs)
    tend = (tend.mjd-tstart.mjd)*86400L + (tend.time-tstart.time)/1000d
    timehinode = str2utc(hdrall.date_obs)
    timehinode = (timehinode.mjd-tstart.mjd)*86400L + (timehinode.time-tstart.time)/1000d
    ;calculate basic step size
    if nfiles gt 1 then begin
      cdelt3 = dblarr(nfiles-1)
      for i=1,nfiles-1 do begin
        cdelt3[i-1] = timehinode[i] - timehinode[i-1]
      endfor
    endif else cdelt3=0
    cdelt3 = mean(cdelt3)


    crval1hinode = hdrall.crval1
    crval2hinode = hdrall.crval2
    cdelt1 = IRISl12_mostcommonvalue(hdrall.cdelt1)
    cdelt2 = IRISl12_mostcommonvalue(hdrall.cdelt2)
    sumsptrl = hdrall.fgbinx
    sumspat = hdrall.fgbiny
    crota2orig = hdrall.crota2
    obs2fov->get_xycenreg, regxslope, regxconst, regyslope, regyconst


    ;check if there are more than 1 wavelengths defined in waveoff
    if doit gt 1 && required_tags(hdrall, /WAVEOFF) then begin
      waves = hdrall.waveoff
      waves = waves[uniq(waves, sort(waves))]
      nwaves = N_ELEMENTS(waves)
    endif else begin
      nwaves=1
      if required_tags(hdrall, /WAVEOFF) then begin
        waves = IRISl12_mostcommonvalue(hdrall.waveoff)
      endif else waves=0
    endelse


    ;create a file for each image within a level 1 file
    for indimage=0,nimages-1 do begin

      if indimage eq 0 then begin
        hdrall_original = hdrall
      endif else begin
        hdrall = hdrall_original
      endelse
;stop

      ;create a file for each wavelength, if there are more than 1 in waveoff
      for indwave=0,nwaves-1 do begin
        
        file1 = file0 + '_' + label[indimage]

        if nimages gt 1 then begin
          if umodes[iumode].files_win eq 1 then data = reform(dataall[*,*,indimage]) $
          else data = reform(dataall[*,*,indimage,*])
        endif else begin
          data = dataall
        endelse

        if nwaves gt 1 then begin

          if indwave eq 0 then begin
            hdrall_original = hdrall
          endif else begin
            hdrall = hdrall_original
          endelse

          if nimages gt 1 then begin
            if umodes[iumode].files_win eq 1 then data = reform(dataall[*,*,indimage]) $
            else data = reform(dataall[*,*,indimage,*])
          endif else begin
            data = dataall
          endelse

          induse = where(hdrall.waveoff eq waves[indwave], countwave)
          hdrall = hdrall[induse]
          data = data[*,*,induse]
          file = file1 + '_' + strcompress(string(waves[indwave]),/remove_all)

          ;recalculate a few things
          nfiles = N_ELEMENTS(hdrall)

          timehinode = str2utc(hdrall.date_obs)
          timehinode = (timehinode.mjd-tstart.mjd)*86400L + (timehinode.time-tstart.time)/1000d
          ;calculate basic step size
          if nfiles gt 1 then begin
            cdelt3 = dblarr(nfiles-1)
            for i=1,nfiles-1 do begin
              cdelt3[i-1] = timehinode[i] - timehinode[i-1]
            endfor
          endif else cdelt3=0
          cdelt3 = mean(cdelt3)

          crval1hinode = hdrall.crval1
          crval2hinode = hdrall.crval2
          cdelt1 = IRISl12_mostcommonvalue(hdrall.cdelt1)
          cdelt2 = IRISl12_mostcommonvalue(hdrall.cdelt2)
          sumsptrl = hdrall.fgbinx
          sumspat = hdrall.fgbiny
          crota2orig = hdrall.crota2
        endif else file = file1
        file = file + '.fits'


        if N_ELEMENTS(unit) gt 1 then bunit = unit[indimage] $
        else bunit = unit[0]
        if N_ELEMENTS(type) gt 1 then btype = type[indimage] $
        else btype = type[0]


        pc1_1 = fltarr(nfiles)
        pc1_2 = fltarr(nfiles)
        pc2_1 = fltarr(nfiles)
        pc2_2 = fltarr(nfiles)
        pc3_1 = fltarr(nfiles)
        pc3_2 = fltarr(nfiles)

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

          hinodecube = make_array(naxis1, naxis2, nfiles, /float, value=!values.f_nan)
          sizedata = size(data)


          for ifile=0,nfiles-1 do begin

            ;cut out the desired window
            ;get xcen/ycen from the regression result
            xcenhinode = regxconst + timehinode[ifile] * regxslope
            ycenhinode = regyconst + timehinode[ifile] * regyslope

            if keyword_set(debug) then begin
              wcstemp = fitshead2wcs(hdrall[ifile])
              examine_image, data[*,*,ifile], wcstemp, 'Original'
              help,hdrall[ifile]
              stop
            endif



            ;first roll the image to iris roll angle
            ;datatemp=rot(data[*,*,ifile], obs2fov->get_rollangle()-crota2orig[ifile], 1, x, y, /pivot, cubic=-0.5, missing=0)
            sizetemp = ceil(1.5*sqrt(sizedata[1]*sizedata[1]+sizedata[2]*sizedata[2]))
            datatemp = make_array(sizetemp, sizetemp, /float, value=!values.f_nan)
            xshifttemp = fix((sizetemp-sizedata[1])/2.0)
            yshifttemp = fix((sizetemp-sizedata[2])/2.0)
            crpix1temp = hdrall[ifile].crpix1 + xshifttemp
            crpix2temp = hdrall[ifile].crpix2 + yshifttemp
            datatemp[xshifttemp:xshifttemp+sizedata[1]-1, yshifttemp:yshifttemp+sizedata[2]-1] = data[*,*,ifile]
            if keyword_set(debug) then begin
              ;this step is good
              hdrtemp = hdrall[ifile]
              hdrtemp.naxis1 = sizetemp
              hdrtemp.naxis2 = sizetemp
              hdrtemp.crpix1 = crpix1temp
              hdrtemp.crpix2 = crpix2temp
              wcstemp = fitshead2wcs(hdrtemp)
              examine_image, datatemp, wcstemp, 'Original padded'
            endif
            datatemp = rot(datatemp, obs2fov->get_rollangle()-crota2orig[ifile], 1, crpix1temp, crpix2temp, /pivot, cubic=-0.5, missing=0)
            if keyword_set(debug) then begin
              ;this step is good
              hdrtemp.crota1 = obs2fov->get_rollangle()
              hdrtemp.crota2 = obs2fov->get_rollangle()
              wcstemp = fitshead2wcs(hdrtemp)
              examine_image, datatemp, wcstemp, 'Original padded and rotated'
            endif



            ;get original hinode sat_rot first for calculations
            a=crota2orig[ifile]/!radeg

            crval1hinode[ifile] = xcenhinode
            crval2hinode[ifile] = ycenhinode

            hdrall[ifile].naxis1 = sizetemp;;;
            hdrall[ifile].naxis2 = sizetemp;;;
            hdrall[ifile].crpix1 = crpix1temp;;;
            hdrall[ifile].crpix2 = crpix2temp;;;
            hdrall[ifile].CROTA1 = obs2fov->get_rollangle()
            hdrall[ifile].CROTA2 = obs2fov->get_rollangle()
            wcshinode = fitshead2wcs(hdrall[ifile])
            pc1_1[ifile] = wcshinode.pc[0,0]
            pc1_2[ifile] = wcshinode.pc[0,1]
            pc2_1[ifile] = wcshinode.pc[1,0]
            pc2_2[ifile] = wcshinode.pc[1,1]



            ;and calculate to which x/y that corresponds in the hinode image

            ;easy way
            ;only accurate if crota2=0
            ;xold = (xcenhinode-hdrall[ifile].crval1) / (hdrall[ifile].cdelt1*cos(a)) + hdrall[ifile].crpix1 - 1
            ;yold = (ycenhinode-hdrall[ifile].crval2) / (hdrall[ifile].cdelt2*cos(a)) + hdrall[ifile].crpix2 $
            ;  + (xold-hdrall[ifile].crpix1)*sin(a) - 1

            ;hard way, should be accurate
            ;calculate x/y coordinates of the four corners
            xx1start = hdrall[ifile].crval1 + hdrall[ifile].cdelt1 * (pc1_1[ifile] * (1-crpix1temp) $
              + pc1_2[ifile] * (1-crpix2temp))
            xx1end = hdrall[ifile].crval1 + hdrall[ifile].cdelt1 * (pc1_1[ifile] * (sizetemp-crpix1temp) $
              + pc1_2[ifile] * (1-crpix2temp))
            xx2start = hdrall[ifile].crval1 + hdrall[ifile].cdelt1 * (pc1_1[ifile] * (1-crpix1temp) $
              + pc1_2[ifile] * (sizetemp-crpix2temp))
            xx2end = hdrall[ifile].crval1 + hdrall[ifile].cdelt1 * (pc1_1[ifile] * (sizetemp-crpix1temp) $
              + pc1_2[ifile] * (sizetemp-crpix2temp))
            yy1start = hdrall[ifile].crval2 + hdrall[ifile].cdelt2 * (pc2_2[ifile] * (1-crpix2temp) $
              + pc2_1[ifile] * (1-crpix1temp))
            yy1end = hdrall[ifile].crval2 + hdrall[ifile].cdelt2 * (pc2_2[ifile] * (sizetemp-crpix2temp) $
              + pc2_1[ifile] * (1-crpix1temp))
            yy2start = hdrall[ifile].crval2 + hdrall[ifile].cdelt2 * (pc2_2[ifile] * (1-crpix2temp) $
              + pc2_1[ifile] * (sizetemp-crpix1temp))
            yy2end = hdrall[ifile].crval2 + hdrall[ifile].cdelt2 * (pc2_2[ifile] * (sizetemp-crpix2temp) $
              + pc2_1[ifile] * (sizetemp-crpix1temp))

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

            if keyword_set(debug) then begin
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
                string(ifile)+'  '+(*files[iumode])[ifile],$
                'xstart: '+string(xstart),$
                'x/ycen',$
                'IRIS:   '+string(xcenhinode,format='(f7.1)')+'  '+string(ycenhinode,format='(f7.1)')+$
                '  FOV: '+string(fovx,format='(f7.1)')+'  '+string(fovy,format='(f7.1)'),$
                'HINODE: '+string(hdrall[ifile].crval1,format='(f7.1)')+'  '+string(hdrall[ifile].crval2,format='(f7.1)')+$
                '  CRPIX HINODE: '+string(hdrall[ifile].crpix1,format='(f7.1)')+'  '+string(hdrall[ifile].crpix2,format='(f7.1)')]
              continue
            endif
            if xend lt 0 then begin
              box_message,['no overlap of HINODE and IRIS',$
                string(ifile)+'  '+(*files[iumode])[ifile],$
                'xend: '+string(xend),$
                'x/ycen',$
                'IRIS:   '+string(xcenhinode,format='(f7.1)')+'  '+string(ycenhinode,format='(f7.1)')+$
                '  FOV: '+string(fovx,format='(f7.1)')+'  '+string(fovy,format='(f7.1)'),$
                'HINODE: '+string(hdrall[ifile].crval1,format='(f7.1)')+'  '+string(hdrall[ifile].crval2,format='(f7.1)')+$
                '  CRPIX HINODE: '+string(hdrall[ifile].crpix1,format='(f7.1)')+'  '+string(hdrall[ifile].crpix2,format='(f7.1)')]
              continue
            endif
            if xend ge sizetemp then begin
              xendcube = xendcube - (xend - (sizetemp-1))
              xend = sizetemp-1
            endif
            if xstart eq xend then begin
              box_message,['no overlap of HINODE and IRIS',$
                string(ifile)+'  '+(*files[iumode])[ifile],$
                'xstart=xend '+string(xstart)+'  '+string(xend),$
                'x/ycen',$
                'IRIS:   '+string(xcenhinode,format='(f7.1)')+'  '+string(ycenhinode,format='(f7.1)')+$
                '  FOV: '+string(fovx,format='(f7.1)')+'  '+string(fovy,format='(f7.1)'),$
                'HINODE: '+string(hdrall[ifile].crval1,format='(f7.1)')+'  '+string(hdrall[ifile].crval2,format='(f7.1)')+$
                '  CRPIX HINODE: '+string(hdrall[ifile].crpix1,format='(f7.1)')+'  '+string(hdrall[ifile].crpix2,format='(f7.1)')]
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
                string(ifile)+'  '+(*files[iumode])[ifile],$
                'ystart: '+string(ystart),$
                'x/ycen',$
                'IRIS:   '+string(xcenhinode,format='(f7.1)')+'  '+string(ycenhinode,format='(f7.1)')+$
                '  FOV: '+string(fovx,format='(f7.1)')+'  '+string(fovy,format='(f7.1)'),$
                'HINODE: '+string(hdrall[ifile].crval1,format='(f7.1)')+'  '+string(hdrall[ifile].crval2,format='(f7.1)')+$
                '  CRPIX HINODE: '+string(hdrall[ifile].crpix1,format='(f7.1)')+'  '+string(hdrall[ifile].crpix2,format='(f7.1)')]
              continue
            endif
            if yend lt 0 then begin
              box_message,['no overlap of HINODE and IRIS',$
                string(ifile)+'  '+(*files[iumode])[ifile],$
                'yend: '+string(yend),$
                'x/ycen',$
                'IRIS:   '+string(xcenhinode,format='(f7.1)')+'  '+string(ycenhinode,format='(f7.1)')+$
                '  FOV: '+string(fovx,format='(f7.1)')+'  '+string(fovy,format='(f7.1)'),$
                'HINODE: '+string(hdrall[ifile].crval1,format='(f7.1)')+'  '+string(hdrall[ifile].crval2,format='(f7.1)')+$
                '  CRPIX HINODE: '+string(hdrall[ifile].crpix1,format='(f7.1)')+'  '+string(hdrall[ifile].crpix2,format='(f7.1)')]
              continue
            endif
            if yend ge sizetemp then begin
              yendcube = yendcube - (yend - (sizetemp-1))
              yend = sizetemp-1
            endif
            if ystart eq yend then begin
              box_message,['no overlap of HINODE and IRIS',$
                string(ifile)+'  '+(*files[iumode])[ifile],$
                'ystart=yend '+string(ystart)+'  '+string(yend),$
                'x/ycen',$
                'IRIS:   '+string(xcenhinode,format='(f7.1)')+'  '+string(ycenhinode,format='(f7.1)')+$
                '  FOV: '+string(fovx,format='(f7.1)')+'  '+string(fovy,format='(f7.1)'),$
                'HINODE: '+string(hdrall[ifile].crval1,format='(f7.1)')+'  '+string(hdrall[ifile].crval2,format='(f7.1)')+$
                '  CRPIX HINODE: '+string(hdrall[ifile].crpix1,format='(f7.1)')+'  '+string(hdrall[ifile].crpix2,format='(f7.1)')]
              continue
            endif

            if keyword_set(debug) then begin
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

            ;rotate the hinode data to the IRIS rotation
            ;datatemp=rot(data[*,*,ifile], obs2fov->get_rollangle()-crota2orig[ifile], 1, x, y, /pivot, cubic=-0.5, missing=0)
            ;datatemp=rot(data[*,*,ifile], -45, 1, x, y, /pivot, cubic=-0.5,missing=0)

            ;          if keyword_set(debug) then begin
            ;            ;testing, show rotated data with cutout window and new xcen/ycen
            ;            window,1,xs=600,ys=600, retain=2
            ;            pih,datatemp
            ;            oplot,[xstart,xend],[ystart,ystart],color=250
            ;            oplot,[xstart,xend],[yend,yend],color=250
            ;            oplot,[xstart,xstart],[ystart,yend],color=250
            ;            oplot,[xend,xend],[ystart,yend],color=250
            ;            oplot,[x-50,x+50],[y,y],color=250
            ;            oplot,[x,x],[y-50,y+50],color=250
            ;          endif

            ;do the subpixel shift
            hinodecube[xstartcube:xendcube, ystartcube:yendcube, ifile] = INF_POLY_2D(datatemp[xstart:xend,ystart:yend],pcoeff,qcoeff,2,cubic=-0.5)

            if keyword_set(debug) then begin
              hdrtemp.naxis1 = naxis1
              hdrtemp.naxis2 = naxis2
              hdrtemp.crpix1 = crpix1
              hdrtemp.crpix2 = crpix2
              hdrtemp.crval1 = crval1hinode[ifile]
              hdrtemp.crval2 = crval2hinode[ifile]
              hdrtemp = rem_tag(hdrtemp, ['crota1','crota2'])
              hdrtemp = create_struct(hdrtemp, 'PC1_1', pc1_1[ifile], 'PC1_2', pc1_2[ifile], $
                'PC2_1', pc2_1[ifile], 'PC2_2', pc2_2[ifile])
              wcstemp = fitshead2wcs(hdrtemp)
              examine_image, hinodecube[*,*,ifile], wcstemp, 'Hinode cube'
              stop
            endif



          endfor
        endelse ;~fullfov

        ind = floor(nfiles/2)
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
        PSJIADAT = 100.0 * countrealdata / ((xpixel2-xpix1+1)*(ypixel2-ypixel1+1)*nfiles)
        
        ;raster
        slitpix = obs2fov->get_slitpix() / cdelt1
        xpix1 = xpixel1 + fix(slitpix[0])
        xpix2 = xpixel1 + ceil(slitpix[1])
        ind = where(hinodecube[xpix1:xpix2, ypixel1:ypixel2, *] EQ hinodecube[xpix1:xpix2, ypixel1:ypixel2, *], countrealdata)
        PRASADAT = 100.0 * countrealdata / ((xpix2-xpix1+1)*(ypixel2-ypixel1+1)*nfiles)
        
        ;calculate percentage of coverage in time
        tmin = min(timehinode, max=tmax)
        if tmin lt 0 then tmin=0
        if tmax gt tend then tmax=tend
        timepercentage = 100.0 * (tmax-tmin) / tend


        t1=systime(1)
        ;first region is written into primary block
        mkhdr, mainheader, hinodecube, /extend
        ;add some information to the primary header
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
        sxaddpar, mainheader, 'RASOLAP', umodes[iumode].rasterOverlap, 'PRASAHCR GT specified lower limit'
        sxaddpar, mainheader, 'PRASAHCR', umodes[iumode].rasterOverlapPercent, 'Percentage of raster area coverage, according to HCR'
        sxaddpar, mainheader, 'PSJIAHCR', umodes[iumode].SjiOverlapPercent, 'Percentage of SJI area coverage, according to HCR'
        sxaddpar, mainheader, 'PRASADAT', PRASADAT, 'Percentage of raster area coverage, calculated from data'
        sxaddpar, mainheader, 'PSJIADAT', PSJIADAT, 'Percentage of SJI area coverage, calculated from data'
        sxaddpar, mainheader, 'PTIMELAP', timepercentage, 'Percentage IRIS OBS coverage in time'
        ;if required_tags(hdrall, /BTYPE) then BTYPE=IRISl12_mostcommonvalue(hdrall.BTYPE) $
        ;else BTYPE=''
        sxaddpar, mainheader, 'BTYPE', BTYPE;OBSvars.btype
        ;if required_tags(hdrall, /BUNIT) then BUNIT=IRISl12_mostcommonvalue(hdrall.BUNIT) $
        ;else BUNIT=''
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
        ;      sxaddpar, mainheader, 'MISSRAS', 0
        ;
        sxaddpar, mainheader, 'PC1_1', mean(pc1_1);OBSvars.PC1_1
        sxaddpar, mainheader, 'PC1_2', mean(pc1_2);OBSvars.PC1_2
        sxaddpar, mainheader, 'PC2_1', mean(pc2_1);OBSvars.PC2_1
        sxaddpar, mainheader, 'PC2_2', mean(pc2_2);OBSvars.PC2_2
        sxaddpar, mainheader, 'PC3_1', mean(pc3_1);OBSvars.PC3_1
        sxaddpar, mainheader, 'PC3_2', mean(pc3_2);OBSvars.PC3_2

        sxaddpar, mainheader, 'WAVEOFF', waves[indwave]
        sxaddpar, mainheader, 'SOTSTART', umodes[iumode].sotstart
        sxaddpar, mainheader, 'SOTSEND', umodes[iumode].sotend
        sxaddpar, mainheader, 'SOTEVENT', umodes[iumode].eventid
        link = strtrim(umodes[iumode].eventlink, 2)
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
        sxaddpar, mainheader, 'TER'+number, (size(hinodecube))[2];OBSvars.ter+1
        sxaddpar, mainheader, 'TSC'+number, 1;OBSvars.SJIsc_rtype[0]+1
        sxaddpar, mainheader, 'TEC'+number, (size(hinodecube))[1];OBSvars.SJIec_rtype[0]+1
        ;
        ;      ;sxaddpar, mainheader, 'CCDTYPE', 'SJI'
        ;
        fxaddpar, mainheader, 'CDELT1', cdelt1;OBSvars.CDELT1SJI     ;image scale in the x-direction
        fxaddpar, mainheader, 'CDELT2', cdelt2;OBSvars.CDELT2SJI     ;image scale in the y-direction
        fxaddpar, mainheader, 'CDELT3', cdelt3      ;slit width for FUV,NUV;;;;;;;;;;;;;;to be changed


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










        FRMid=strarr(nfiles)
        FDBid=strarr(nfiles)
        CRSid=strarr(nfiles)
        filenameSJI=*files[iumode]



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
        hinodelog.twrite = hinodelog.twrite + t2-t1
        hinodelog.filescreated = hinodelog.filescreated + 1
      endfor ;indwave

    endfor ;indimage

    if keyword_set(deletetempfiles) then file_delete, *files[iumode]
  endfor ;iumode
  if keyword_set(deletetempfiles) then file_delete, outdir+'original'
end
