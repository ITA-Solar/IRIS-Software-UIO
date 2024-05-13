;RPT fixed version finalized on 11/22 based on Martin's emails;
;commented out the added debug prints.
;NOTE that this was quasi-onlined in hackish way copying to , pending
;fix to the Martin CVS repo->SSW online workflow which has multiple
;snags.  

PRO IRISl12_saveSJIData, hdr, files, hdrmap, rastersSJI, filename, OBSvars, l1to2log, _extra=_extra $
  , timemeas=timemeas, scaled=scaled, preserveNAN=preserveNAN, preserveINF=preserveINF, l1p5=l1p5, maxl1files=maxl1files, rollangle=rollangle
  ; $Id: irisl12_savesjidata.pro,v 1.100 2018/03/23 10:06:33 mawiesma Exp $  ;

  constants = obj_new('IRISsim_constants')

  tstart = str2utc(OBSvars.OBSstart)

  ;for padding of SJI window, we assume spectral summing is the same for all SJI
  ;and window coordinates don't differ more than maxdeviation
  OBSvars.SJIsc_rtype[*] = 0
  OBSvars.SJIec_rtype[*] = 0
  OBSvars.SJIok_rtype[*] = 0
  OBSvars.sumspecmin = 8
  SJIloginds = 0
  for nrFW=0,N_ELEMENTS(rastersSJI)-1 do begin
    for nrCRS=0,rastersSJI[nrFW].nrasters-1 do begin
      OBSvars.sumspecmin = min([OBSvars.sumspecmin, *(*(rastersSJI[nrFW].rastercrs[nrCRS])).sumspec])
      SJIloginds = [SJIloginds, *(*(rastersSJI[nrFW].rastercrs[nrCRS])).SJIlogind]
    endfor
  endfor

  SJIloginds = SJIloginds[0:*]
  if N_ELEMENTS(SJIloginds) gt 1 then begin
    SJIloginds = SJIloginds[1:*]
    if max(SJIloginds) ge 0 then begin
      slogindgood = where(SJIloginds ge 0, countslog)
      if countslog gt 0 then begin
        SJIloginds2 = SJIloginds[slogindgood]
        SJIfiles = hdrmap[SJIloginds2]
        sfilegood = where(SJIfiles ge 0, countsfile)
        if countsfile gt 0 then begin
          SJIfiles = SJIfiles[sfilegood]
         IRISl12_getkeywords, 2, hdr[SJIfiles], OBSvars, /sji
      endif
    endif
  endif
endif
  
  



  for nrFW=0,N_ELEMENTS(rastersSJI)-1 do begin
    print, 'Processing filterwheel position ' + strtrim(string(nrFW+1),2) + ' of ' + strtrim(string(N_ELEMENTS(rastersSJI)),2)
    print, 'Filter wheel position: ' + (*rastersSJI[nrFW].rastercrs).fwdesc

    for nrCRS=0,rastersSJI[nrFW].nrasters-1 do begin
      print, 'Processing CRS ' + strtrim(string(nrCRS+1),2) + ' of ' + strtrim(string(rastersSJI[nrFW].nrasters),2)
      print, 'Number of steps: ' + strtrim(string((*(rastersSJI[nrFW].rastercrs[nrCRS])).nsteps),2)

      nmissraster=0 ;number of missing files in this raster
      startval1=!NULL

      ;OBSvars.sumspecmin = min(*(*(rastersSJI[nrFW].rastercrs[nrCRS])).sumspec)
      ;OBSvars.sumspatmin=0
      OBSvars.nregSJI = 0
      ;OBSvars.SJIsr_rtype[*] = 0
      ;OBSvars.SJIer_rtype[*] = 0
      ;OBSvars.SJIsc_rtype[*] = 0
      ;OBSvars.SJIec_rtype[*] = 0
      ;OBSvars.SJIok_rtype[*] = 0
      OBSvars.SJIdet[*] = 'SJI'
      OBSvars.SJIwavemin[*] = 0
      OBSvars.SJIwavemax[*] = 0
      OBSvars.SJIdesc[*] = ''
      OBSvars.SJIwave[*] = 0
      OBSvars.Date_OBS=''
      OBSvars.Date_End=''
      OBSvars.SAT_ROT = 0

      statistics={mean:0.0, $
        median:0.0, $
        stdev:0.0, $
        min:0.0, $
        max:0.0, $
        vals:0UL, $
        kurtosis:0.0, $
        skewness:0.0, $
        datap01:0.0, $
        datap10:0.0, $
        datap25:0.0, $
        datap75:0.0, $
        datap90:0.0, $
        datap95:0.0, $
        datap98:0.0, $
        datap99:0.0}

      ;logind: index of this entry in the log from the simulation
      ;   -1 if no image of this type (sji,fuv,nuv) has been taken
      ;hdrmap: is a translation of index of log from simulation to index of file list
      ;   -1 if file is missing
      nsteps = (*(rastersSJI[nrFW].rastercrs[nrCRS])).nsteps
      SJIloginds = *(*(rastersSJI[nrFW].rastercrs[nrCRS])).SJIlogind
      if max(SJIloginds) ge 0 then begin ;see if there actually was any exposure of this type in this raster
        slogindgood = where(SJIloginds ge 0, countslog) ;only get the steps with exposure of this type
        if countslog gt 0 then begin
          SJIloginds2 = SJIloginds[slogindgood]
          SJIfiles = hdrmap[SJIloginds2]
          sfilegood = where(SJIfiles ge 0, countsfile)
          if countsfile gt 0 then begin
            ;SJIfiles = SJIfiles[sfilegood]
            ;IRISl12_getkeywords, 2, hdr[SJIfiles[sfilegood]], OBSvars, /sji
            IRISl12_getkeywords, 3, hdr[SJIfiles[sfilegood]], OBSvars, /sji
            IRISl12_getkeywords, 4, hdr[SJIfiles[sfilegood]], OBSvars
          endif
        endif
      endif


      pztxdiff = round((OBSvars.PZTxmax-OBSvars.PZTxmin)/OBSvars.cdelt1SJI)+1
      ;print, 'martin requested debug prints'
      ;which, 'iris_isp2solar'
      ;print, pztxdiff
      ;print, OBSvars.PZTxmax
      ;print, OBSvars.PZTxmin
      ;print, OBSvars.cdelt1SJI

      ;print, 'more debug'
      ;print, OBSvars.SJIok_rtype
      ;print, OBSvars.SJIsc_rtype
      ;print, OBSvars.SJIec_rtype
      
      nx=3
      if OBSvars.SJIok_rtype[0] then begin
         
        numcols = OBSvars.SJIec_rtype[0]-OBSvars.SJIsc_rtype[0]+1
        nx = numcols+pztxdiff
        ny = OBSvars.ter-OBSvars.tsr+1
        data = make_array(nx, ny, nsteps,/float,value=!values.f_nan)
        FOVX = nx * OBSvars.cdelt1SJI
        FOVY = ny * OBSvars.cdelt2SJI
      endif else begin
        data = make_array(3,3,nsteps,/float,value=!values.f_nan)
        FOVX = constants->get_missingvalue()
        FOVY = constants->get_missingvalue()
      endelse
      time = make_array(nsteps, /long, value=constants->get_missingvalue())
      pztx = make_array(nsteps, /float, value=OBSvars.PZTxmin)
      pzty = make_array(nsteps, /float, value=OBSvars.PZTymin)
      xcen = dblarr(nsteps)
      ycen = dblarr(nsteps)
      crpix1 = dblarr(nsteps)
      crval1 = dblarr(nsteps)
      crpix2 = dblarr(nsteps)
      crval2 = dblarr(nsteps)
      exptime = dblarr(nsteps)
      slitx = dblarr(nsteps)
      slity = dblarr(nsteps)
      filenameSJI = strarr(nsteps)
      FRMid = strarr(nsteps)
      FDBid = strarr(nsteps)
      CRSid = strarr(nsteps)
      sumsptrlSJI = *(*(rastersSJI[nrFW].rastercrs[nrCRS])).sumspec
      sumspatSJI = *(*(rastersSJI[nrFW].rastercrs[nrCRS])).sumspat
      DataSrcSJI = make_array(nsteps, value=-1.0)
      LUTIDsji = intarr(nsteps)
      obs_vr = dblarr(nsteps)
      ophase = dblarr(nsteps)
      pc1_1 = dblarr(nsteps)
      pc2_1 = dblarr(nsteps)
      pc1_2 = dblarr(nsteps)
      pc2_2 = dblarr(nsteps)
      pc3_1 = dblarr(nsteps)
      pc3_2 = dblarr(nsteps)

      IPRPVER = dblarr(nsteps)
      IPRPPDBV = dblarr(nsteps)
      IPRPDVER = lonarr(nsteps)
      IPRPBVER = lonarr(nsteps)
      IPRPFVER = lonarr(nsteps)
      IPRPGVER = lonarr(nsteps)
      IPRPPVER = lonarr(nsteps)
      IT01PMRF = dblarr(nsteps)       ;       PM Temperature (for pointing) (IMG)
      IT06TELM = dblarr(nsteps)        ;       MidTel Temperature (for pointing) (IMG)
      IT14SPPX = dblarr(nsteps)        ;       Spectrograph +X Temperature (for wavelength) (IMG)
      IT15SPPY = dblarr(nsteps)         ;       Spectrograph +Y Temperature (for wavelength) (IMG)
      IT16SPNX = dblarr(nsteps)       ;       Spectrograph -X Temperature (for wavelength) (IMG)
      IT17SPNY = dblarr(nsteps)        ;       Spectrograph -Y Temperature (for wavelength) (IMG)
      IT18SPPZ = dblarr(nsteps)          ;       Spectrograph +Z Temperature (for wavelength) (IMG)
      IT19SPNZ = dblarr(nsteps)         ;       Spectrograph -Z Temperature (for wavelength) (IMG)
      IPRPDTMP = strarr(nsteps)        ;       Comma-delimited string list of temperatures used by iris_make_dark (IMG)
      IPRPOFFX = dblarr(nsteps)         ;       X (spectral direction) shift in pixels (IMG)
      IPRPOFFY = dblarr(nsteps)         ;       Y (spatial direction) shift in pixels (IMG)
      IPRPOFFF = dblarr(nsteps)         ;       Flag indicating source of X and Y offsets (IMG)


      if OBSvars.SJIok_rtype[0] then begin
        sjifilecounter=0l
        sgroup=-1l
        for step=0,nsteps-1 do begin
          if SJIloginds[step] ge 0 then begin
            SJIfileind = hdrmap[SJIloginds[step]]
            if SJIfileind ge 0 then begin
              ;print,files[SJIfileind]
              t1=systime(1)
              if (sjifilecounter-sgroup*maxl1files eq N_ELEMENTS(hdrSJIall)) || (sgroup eq -1) then begin
                if sjifilecounter+maxl1files gt N_ELEMENTS(sfilegood) then endind=N_ELEMENTS(sfilegood)-1 $
                else endind=sjifilecounter+maxl1files-1
                fileind = sfilegood[sjifilecounter:endind]
                filenamesall = files[SJIfiles[fileind]]
                if l1p5 eq 1 then begin
                  read_iris, files[SJIfiles[fileind]], hdrSJIall, sjiall, /history, /use_shared, /no_shell, /uncomp_delete, _extra=_extra
                endif else begin
                  hdrSJIall = !NULL
                  ind = SJIfiles[fileind]
                  ;divide files for different CRS
                  crsids = hdr[ind[UNIQ(hdr[ind].IICRSID, SORT(hdr[ind].IICRSID))]].IICRSID
                  for icrs=0,N_ELEMENTS(crsids)-1 do begin
                    crscur = where(hdr[ind].IICRSID eq crsids[icrs], count)
                    if count gt 0 then begin
                      iris_prep, files, ind[crscur], hdrtemp, datatemp, /use_shared, /no_shell, /uncomp_delete, _extra=_extra, /run_time, /strict
                      if N_ELEMENTS(hdrSJIall) eq 0 then begin
                        hdrSJIall = make_array(N_ELEMENTS(ind), value=hdrtemp[0])
                        sjiall = make_array((size(datatemp))[1], (size(datatemp))[2], N_ELEMENTS(ind), /float)
                      endif
                      hdrSJIall[crscur] = hdrtemp
                      for i=0,count-1 do sjiall[*,*,crscur[i]] = datatemp[*,*,i]
                    endif
                  endfor
                  hdrSJIall[*].LVL_NUM = 1.51
                endelse
                if N_ELEMENTS(rollangle) gt 0 then begin
                  t=tag_names(hdrSJIall)
                  w=where(t eq 'SAT_ROT', c)
                  if c gt 0 then begin
                    hdrSJIall[*].SAT_ROT=rollangle
                  endif else begin
                    hdrSJIall = add_tag(hdrSJIall, rollangle, 'SAT_ROT')
                  endelse
                endif
                sgroup=sgroup+1
              endif
              sji = sjiall[*,*,sjifilecounter-sgroup*maxl1files]
              hdrSJI = hdrSJIall[sjifilecounter-sgroup*maxl1files]
              filenameSJI[step] = filenamesall[sjifilecounter-sgroup*maxl1files]
              sjifilecounter=sjifilecounter+1
              t2=systime(1)
              timemeas.readfile=timemeas.readfile+(t2-t1)

              ;check for specific corrupted image
              if hdrSJI.cropid eq 0 && hdrSJI.iicrsid ne 1 then begin
                ;corrupted image
                if l1to2log.nbadfiles eq 0 then begin
                  l1to2log.nbadfiles = 1
                  l1to2log.badfiles = ptr_new(filenameSJI[step])
                  l1to2log.badfilesreason = ptr_new('cropid eq 0 && iicrsid ne 1')
                endif else begin
                  l1to2log.nbadfiles = l1to2log.nbadfiles + 1
                  *l1to2log.badfiles = [*l1to2log.badfiles, filenameSJI[step]]
                  *l1to2log.badfilesreason = [*l1to2log.badfilesreason, 'cropid eq 0 && iicrsid ne 1']
                endelse
                SJIfileind=-1
                nmissraster=nmissraster+1
                time[step] = (*(*(rastersSJI[nrFW].rastercrs[nrCRS])).time)[step]
                continue
              endif

              IRISl12_getkeywords, 5, hdrSJI, OBSvars, /sji
              ;image might not be (correctly) flipped, so we check here
              ;first we need to know whether this is FUV or NUV
              ;commented out, because win_flip is incorrect for FUV SJI
              ;it is correct, 1: flipped about the x-axis, 2: flipped about the y-axis
              ;          img_path = gt_tagval(hdrSJI, 'IMG_PATH', missing='')
              ;          ff=strmid(img_path,stregex(img_path,'[1-9][0-9][0-9][0-9]'),4)
              ;          if isnumeric(ff) then ff=uint(ff) else ff=9999
              ;          if ff lt 2000 then begin
              ;            win_flip = gt_tagval(hdrSJI, 'WIN_FLIP', missing=3)
              ;            if win_flip eq 0 then begin
              ;              sji = reverse(sji, 1)
              ;            endif else if win_flip eq 2 then begin
              ;              sji = reverse(sji, 1)
              ;            endif
              ;          endif

              DataSrcSJI[step] = OBSvars.DSRCS
              LUTIDsji[step] = OBSvars.LUTIDsji
              if sumsptrlSJI[step] ne OBSvars.sumspecSJI then box_message,['wrong SUMSPTRL in SJI file',files[SJIfileind]];write errorlog
              if sumspatSJI[step] ne OBSvars.sumspatSJI then box_message,['wrong SUMSPAT in SJI file',files[SJIfileind]];write errorlog
              sumSJI = (sumsptrlSJI[step] ne OBSvars.sumspecmin) || (sumspatSJI[step] ne OBSvars.sumspatmin)

              crpix1[step] = gt_tagval(hdrSJI, 'CRPIX1', missing=0.0)
              crval1[step] = gt_tagval(hdrSJI, 'CRVAL1', missing=0.0)
              cdelt1 = gt_tagval(hdrSJI, 'CDELT1', missing=0.0)
              crpix2[step] = gt_tagval(hdrSJI, 'CRPIX2', missing=0.0)
              crval2[step] = gt_tagval(hdrSJI, 'CRVAL2', missing=0.0)
              cdelt2 = gt_tagval(hdrSJI, 'CDELT2', missing=0.0)
              naxis1 = nx
              naxis2 = OBSvars.ter - OBSvars.tsr + 1
              pc2_1[step] = gt_tagval(hdrSJI, 'PC2_1', missing=0.0)
              pc2_2[step] = gt_tagval(hdrSJI, 'PC2_2', missing=0.0)
              pc1_1[step] = gt_tagval(hdrSJI, 'PC1_1', missing=0.0)
              pc1_2[step] = gt_tagval(hdrSJI, 'PC1_2', missing=0.0)
              pc3_1[step] = gt_tagval(hdrSJI, 'PC3_1', missing=0.0)
              pc3_2[step] = gt_tagval(hdrSJI, 'PC3_2', missing=0.0)
              if sumSJI then begin
                crpix1[step] = crpix1[step] * sumsptrlSJI[step] / OBSvars.sumspecmin
                cdelt1 = cdelt1 / sumsptrlSJI[step] * OBSvars.sumspecmin
                crpix2[step] = crpix2[step] * sumspatSJI[step] / OBSvars.sumspatmin
                cdelt2 = cdelt2 / sumspatSJI[step] * OBSvars.sumspatmin
              endif
              pztx[step]=OBSvars.PZTx
              pzty[step]=OBSvars.PZTy
              shiftx = (pztx[step]-OBSvars.PZTxmin) / OBSvars.cdelt1SJI
              crpix1[step] = crpix1[step] - OBSvars.SJIsc_rtype[0] + shiftx
              crpix2[step] = crpix2[step] - OBSvars.tsr
              xcen[step] = crval1[step] + cdelt1 * (pc1_1[step]*((naxis1+1)/2.0 - crpix1[step]) + pc1_2[step]*((naxis2+1)/2.0 - crpix2[step]))
              ycen[step] = crval2[step] + cdelt2 * (pc2_1[step]*((naxis1+1)/2.0 - crpix1[step]) + pc2_2[step]*((naxis2+1)/2.0 - crpix2[step]))
              crval1[step] = xcen[step]
              crval2[step] = ycen[step]
              slitx[step] = crpix1[step]
              slity[step] = crpix2[step]
              crpix1[step] = (naxis1+1)/2.0
              crpix2[step] = (naxis2+1)/2.0

              IPRPVER[step] = OBSvars.IPRPVER
              IPRPPDBV[step] = OBSvars.IPRPPDBV
              IPRPDVER[step] = OBSvars.IPRPDVER
              IPRPBVER[step] = OBSvars.IPRPBVER
              IPRPFVER[step] = OBSvars.IPRPFVER
              IPRPGVER[step] = OBSvars.IPRPGVER
              IPRPPVER[step] = OBSvars.IPRPPVER
              IT01PMRF[step] = OBSvars.IT01PMRF       ;       PM Temperature (for pointing) (IMG)
              IT06TELM[step] = OBSvars.IT06TELM        ;       MidTel Temperature (for pointing) (IMG)
              IT14SPPX[step] = OBSvars.IT14SPPX        ;       Spectrograph +X Temperature (for wavelength) (IMG)
              IT15SPPY[step] = OBSvars.IT15SPPY         ;       Spectrograph +Y Temperature (for wavelength) (IMG)
              IT16SPNX[step] = OBSvars.IT16SPNX       ;       Spectrograph -X Temperature (for wavelength) (IMG)
              IT17SPNY[step] = OBSvars.IT17SPNY        ;       Spectrograph -Y Temperature (for wavelength) (IMG)
              IT18SPPZ[step] = OBSvars.IT18SPPZ          ;       Spectrograph +Z Temperature (for wavelength) (IMG)
              IT19SPNZ[step] = OBSvars.IT19SPNZ         ;       Spectrograph -Z Temperature (for wavelength) (IMG)
              IPRPDTMP[step] = OBSvars.IPRPDTMP        ;       Comma-delimited string list of temperatures used by iris_make_dark (IMG)
              IPRPOFFX[step] = OBSvars.IPRPOFFX        ;       X (spectral direction) shift in pixels (IMG)
              IPRPOFFY[step] = OBSvars.IPRPOFFY        ;       Y (spatial direction) shift in pixels (IMG)
              IPRPOFFF[step] = OBSvars.IPRPOFFF        ;       Flag indicating source of X and Y offsets (IMG)

              if N_ELEMENTS(history) eq 0 then history = gt_tagval(hdrSJI, 'HISTORY', missing='') $
              else if N_ELEMENTS(history) eq 1 && strcompress(history[0], /remove_all) eq '' then history = gt_tagval(hdrSJI, 'HISTORY', missing='')

              if shiftx lt 0 then begin ;this should not happen
                print,'shiftx negative',shiftx
                shiftx=0
              endif else if shiftx gt pztxdiff then begin ;this should not happen
                print,'shiftx too big',shiftx
                shiftx=pztxdiff
              endif
              shiftxpix = floor(shiftx)
              shiftxsub = shiftx - shiftxpix
              if shiftxsub gt 0.5 then begin
                shiftxsub = shiftxsub - 1
                shiftxpix = shiftxpix + 1
              endif
              if sumSJI then begin
                coords = IRISsim_flipcoords(OBSvars.SJIsc_rtype[0], OBSvars.SJIec_rtype[0], OBSvars.tsr, OBSvars.ter, $
                  sumsptrlSJI[step]/OBSvars.sumspecmin, sumspatSJI[step]/OBSvars.sumspatmin, $
                  /summing_only, /startatzero)
                data[shiftxpix:shiftxpix+numcols-1, *, step] = congrid(sji[coords.tsc:coords.tec, coords.tsr:coords.ter], $
                  OBSvars.SJIec_rtype[0]-OBSvars.SJIsc_rtype[0]+1, $
                  OBSvars.ter[0]-OBSvars.tsr+1)
              endif else data[shiftxpix:shiftxpix+numcols-1, *, step] = sji[OBSvars.SJIsc_rtype[0]:OBSvars.SJIec_rtype[0], OBSvars.tsr:OBSvars.ter]
              if keyword_set(preserveNAN) then begin
                missing = where(data NE data, cmissing)
                if cmissing gt 0 then data[missing]=-200
              endif
              if keyword_set(preserveINF) then begin
                saturated = where(abs(data) eq !values.f_infinity, csaturated)
                if csaturated gt 0 then data[saturated]=16183
              endif
              ;sattot = where(abs(data) eq !values.f_infinity, csattot)
              ;box_message,['before', string(csaturated), string(csattot), string(shiftxsub)]
              pcoeff = [shiftxsub*(-1),0,1,0]
              qcoeff =[0,1,0,0]
              data[*,*,step] = INF_POLY_2D(data[*,*,step],pcoeff,qcoeff,2,cubic=-0.5)
              if keyword_set(preserveNAN) then begin
                if cmissing gt 0 then data[missing]=!values.f_nan
              endif
              if keyword_set(preserveINF) then begin
                if csaturated gt 0 then data[saturated]=!values.f_infinity
              endif
              ;saturated = where(abs(data[*,*,step]) eq !values.f_infinity, csaturated)
              ;sattot = where(abs(data) eq !values.f_infinity, csattot)
              ;box_message,['before', string(csaturated), string(csattot), string(shiftxsub)]

              ;do some statistics
              ts1=systime(1)
              stats1 = iris_cube_statistics(data[*,*,step], missing=constants->get_missingvalue(), /reduced, scaled=scaled)
              if (statistics.vals eq 0) && (stats1.DATAVALS gt 0) then begin
                statistics.mean = stats1.datamean
                statistics.stdev = stats1.DATARMS
                statistics.min = stats1.DATAMIN
                statistics.max = stats1.DATAMAX
                statistics.vals = stats1.DATAVALS
              endif else begin
                if stats1.DATAVALS gt 0 then begin
                  statistics.stdev = irisl12_meanof2stdev(statistics.stdev, stats1.DATARMS, $
                    statistics.mean, stats1.datamean, statistics.vals, stats1.DATAVALS)
                  statistics.mean = (statistics.mean*statistics.vals + stats1.datamean*stats1.DATAVALS) $
                    / (statistics.vals + stats1.DATAVALS)
                  if stats1.DATAMIN lt statistics.min then statistics.min = stats1.DATAMIN
                  if stats1.DATAMAX gt statistics.max then statistics.max = stats1.DATAMAX
                  statistics.vals = statistics.vals + stats1.DATAVALS
                endif
              endelse
              ts2=systime(1)
              timemeas.stats = timemeas.stats+(ts2-ts1)



              exptime[step]=gt_tagval(hdrSJI, 'EXPTIME', missing=0.0)
              obs_vr[step] = OBSvars.obs_vr
              ophase[step] = OBSvars.ophase
              FRMid[step] = strcompress(gt_tagval(hdrSJI, 'ISQFLTID', missing=''), /remove_all)
              FDBid[step] = strcompress(gt_tagval(hdrSJI, 'IIFDBID', missing=''), /remove_all)
              CRSid[step] = strcompress(gt_tagval(hdrSJI, 'IICRSID', missing=''), /remove_all)
              t2 = gt_tagval(hdrSJI, 'DATE_OBS', missing='')
              if t2 ne '' then begin
                t2 = str2utc(t2)
                time[step] = (t2.mjd-tstart.mjd)*86400000L + t2.time-tstart.time
              endif
            endif else begin ;SJIfileind ge 0
              nmissraster=nmissraster+1
              time[step] = (*(*(rastersSJI[nrFW].rastercrs[nrCRS])).time)[step]
            endelse
          endif else SJIfileind=-1 ;SJIloginds[step] ge 0
        endfor ;step=0,nsteps-1
      endif ;window coords ok (OBSvars.SJIok_rtype[0])


      ;calculate min,max and mean exposure duration
      ind=where(exptime gt 0, count)
      if count gt 0 then begin
        OBSvars.exptime = mean(exptime[ind])
        expmin = min(exptime[ind], max=expmax)
        aecact = where((*(*(rastersSJI[nrFW].rastercrs[nrCRS])).exptime/1000.0 - exptime[ind]) $
          gt *(*(rastersSJI[nrFW].rastercrs[nrCRS])).exptime/1000.0*0.08, countaec)
        OBSvars.AECNRAS = countaec
      endif else begin
        expmin=0
        expmax=0
        OBSvars.AECNRAS = 0
      endelse
      OBSvars.expmin = expmin
      OBSvars.expmax = expmax


      ;calculate cadences
      ;as executed
      lost=0
      for i=1,nsteps-1 do begin
        if time[i] ge 0 then begin
          if time[i-1-lost] ge 0 then begin
            lost=0
            if N_ELEMENTS(dtimes) eq 0 then dtimes=time[i]-time[i-1-lost] $
            else dtimes=[dtimes,time[i]-time[i-1-lost]]
          endif
        endif else lost=lost+1
      endfor
      if N_ELEMENTS(dtimes) gt 0 then begin
        cadexav = mean(dtimes)/1000.0
        cadexdv = stddev(dtimes)/1000.0
      endif else begin
        cadexav = 0
        cadexdv = 0
      endelse
      ;as planned
      if nsteps gt 1 then begin
        timestheo = *(*(rastersSJI[nrFW].rastercrs[nrCRS])).time
        for i=1,nsteps-1 do begin
          if i eq 1 then dtimes=timestheo[i]-timestheo[i-1] $
          else dtimes=[dtimes,timestheo[i]-timestheo[i-1]]
        endfor
        cadplav = mean(dtimes)/1000.0
        cadpldv = stddev(dtimes)/1000.0
      endif else begin
        cadplav = 0
        cadpldv = 0
      endelse

      ts1=systime(1)
      ;do rest of statistics for all steps combined
      if statistics.vals gt 0 then begin
        maxelements=100000
        seed=0
        if N_ELEMENTS(data) le maxelements then begin
          stats = iris_cube_statistics(data, missing=constants->get_missingvalue(), scaled=scaled)
        endif else begin
          ind = randomu(seed,maxelements)*N_ELEMENTS(data)
          ind = ind[uniq(ind, sort(ind))]
          stats = iris_cube_statistics(data[ind], missing=constants->get_missingvalue(), scaled=scaled)
        endelse
        statistics.median = stats.datamedn
        statistics.kurtosis = stats.kurtosis
        statistics.skewness = stats.dataskew
        statistics.datap01 = stats.datap01
        statistics.datap10 = stats.datap10
        statistics.datap25 = stats.datap25
        statistics.datap75 = stats.datap75
        statistics.datap90 = stats.datap90
        statistics.datap95 = stats.datap95
        statistics.datap98 = stats.datap98
        statistics.datap99 = stats.datap99
      endif
      ts2=systime(1)
      timemeas.stats = timemeas.stats+(ts2-ts1)



      ;get XCEN and YCEN for all exposures
      ;new way, take middle image
      ind = where(DataSrcSJI gt 0, count)
      if count gt 0 then begin
        mx = (N_ELEMENTS(xcen)-1)/2.0
        ;mx = (max(xcen[ind]) + min(xcen[ind])) / 2.0
        mind = min(abs(ind-mx), ind2)
        ind = ind[ind2]
      endif else ind=0
      xcenall = float(xcen[ind])
      ycenall = float(ycen[ind])
      crpix1all = float(crpix1[ind])
      crval1all = float(crval1[ind])
      crpix2all = float(crpix2[ind])
      crval2all = float(crval2[ind])
      OBSvars.CRPIX3SJI = float(ind+1)
      OBSvars.CRVAL3SJI = time[ind]/1000.0


      ;old way, take first image
      ;      ind = where(xcen ne 0 AND ycen ne 0, count)
      ;      if count gt 0 then ind=ind[0] $
      ;      else ind=0
      ;      xcenall = float(xcen[ind])
      ;      ycenall = float(ycen[ind])
      ;      crpix1all = float(crpix1[ind])
      ;      crval1all = float(crval1[ind])
      ;      crpix2all = float(crpix2[ind])
      ;      crval2all = float(crval2[ind])



      ;scale the data to a signed integer if desired
      missing = where(finite(data) eq 0, cmissing)
      saturated = where(abs(data) eq !values.f_infinity, csaturated)
      if keyword_set(scaled) then begin
        data = (-199) > data < (16382-200)
        data = fix(round((data+200) * 4 - 32768))
        if cmissing gt 0 then data[missing] = -32768
        if csaturated gt 0 then data[saturated] = 32764
        bscales=0.25
        bzeros=7992
      endif else begin
        bscales=1
        bzeros=0
      endelse
      cmissing = cmissing - csaturated


      t1=systime(1)

      ;first region is written into primary block
      mkhdr, mainheader, data, /extend
      ;add some information to the primary header
      sxaddpar, mainheader, 'TELESCOP', OBSvars.Telescop
      sxaddpar, mainheader, 'INSTRUME', 'SJI'
      sxaddpar, mainheader, 'DATA_LEV', OBSvars.DATA_LEV
      sxaddpar, mainheader, 'LVL_NUM', OBSvars.LVL_NUM
      sxaddpar, mainheader, 'VER_RF2', OBSvars.version
      sxaddpar, mainheader, 'DATE_RF2', OBSvars.Date_RF2
      sxaddpar, mainheader, 'DATA_SRC', OBSvars.DATA_SRC
      sxaddpar, mainheader, 'ORIGIN', OBSvars.ORIGIN
      sxaddpar, mainheader, 'BLD_VERS', OBSvars.BLD_VERS
      sxaddpar, mainheader, 'LUTID', OBSvars.LUTID
      sxaddpar, mainheader, 'OBSID', OBSvars.OBSid
      sxaddpar, mainheader, 'OBS_DESC', OBSvars.OBS_Desc
      sxaddpar, mainheader, 'OBSLABEL', OBSvars.OBSLABEL
      sxaddpar, mainheader, 'OBSTITLE', OBSvars.OBSTITLE
      sxaddpar, mainheader, 'DATE_OBS', OBSvars.Date_OBS
      sxaddpar, mainheader, 'DATE_END', OBSvars.Date_End
      sxaddpar, mainheader, 'STARTOBS', OBSvars.OBSstart
      sxaddpar, mainheader, 'ENDOBS', OBSvars.OBSend
      sxaddpar, mainheader, 'OBSREP', OBSvars.OBSrep
      sxaddpar, mainheader, 'CAMERA', 2;;;;;;;;;;;
      sxaddpar, mainheader, 'STATUS', OBSvars.STATUS
      sxaddpar, mainheader, 'BTYPE', OBSvars.btype
      sxaddpar, mainheader, 'BUNIT', OBSvars.bunit
      sxaddpar, mainheader, 'BSCALE', bscales, format="f4.2";, ' True_value = BZERO + BSCALE*Array_value', after='BZERO'
      sxaddpar, mainheader, 'BZERO', bzeros;, ' True_value = BZERO + BSCALE*Array_value', after='BTYPE'
      sxaddpar, mainheader, 'HLZ', OBSvars.HLZ
      sxaddpar, mainheader, 'SAA', OBSvars.SAA
      sxaddpar, mainheader, 'SAT_ROT', OBSvars.SAT_ROT
      sxaddpar, mainheader, 'AECNOBS', OBSvars.AECNOBS
      sxaddpar, mainheader, 'AECNRAS', OBSvars.AECNRAS
      ;sxaddpar, mainheader, 'ACS_ECLP', OBSvars.ACS_ECLP
      ;sxaddpar, mainheader, 'ACS_MODE', OBSvars.ACS_MODE
      ;sxaddpar, mainheader, 'ACS_SAFE', OBSvars.ACS_SAFE
      ;sxaddpar, mainheader, 'ACS_SUNP', OBSvars.ACS_SUNP
      ;sxaddpar, mainheader, 'ASD_REC', OBSvars.ASD_REC
      ;sxaddpar, mainheader, 'DATE', OBSvars.DAT
      sxaddpar, mainheader, 'DSUN_OBS', OBSvars.DSUN_OBS
      ;sxaddpar, mainheader, 'DSUN_REF', OBSvars.DSUN_REF

      sxaddpar, mainheader, 'IAECEVFL', OBSvars.IAECEVFL
      sxaddpar, mainheader, 'IAECFLAG', OBSvars.IAECFLAG
      sxaddpar, mainheader, 'IAECFLFL', OBSvars.IAECFLFL
      sxaddpar, mainheader, 'TR_MODE', OBSvars.TR_MODE

      sxaddpar, mainheader, 'FOVY', FOVY
      sxaddpar, mainheader, 'FOVX', FOVX
      sxaddpar, mainheader, 'XCEN', xcenall
      sxaddpar, mainheader, 'YCEN', ycenall
      sxaddpar, mainheader, 'SUMSPTRL', OBSvars.sumspecmin
      sxaddpar, mainheader, 'SUMSPAT', OBSvars.sumspatmin
      sxaddpar, mainheader, 'EXPTIME', OBSvars.exptime
      sxaddpar, mainheader, 'EXPMIN', OBSvars.expmin
      sxaddpar, mainheader, 'EXPMAX', OBSvars.expmax
      sxaddpar, mainheader, 'DATAMEAN', statistics.mean
      sxaddpar, mainheader, 'DATARMS', statistics.stdev
      sxaddpar, mainheader, 'DATAMEDN', statistics.median
      sxaddpar, mainheader, 'DATAMIN', statistics.min
      sxaddpar, mainheader, 'DATAMAX', statistics.max
      sxaddpar, mainheader, 'DATAVALS', statistics.vals
      sxaddpar, mainheader, 'MISSVALS', cmissing
      sxaddpar, mainheader, 'NSATPIX', csaturated
      sxaddpar, mainheader, 'NSPIKES', 0
      sxaddpar, mainheader, 'TOTVALS', statistics.vals+cmissing+csaturated+0
      sxaddpar, mainheader, 'PERCENTD', float(statistics.vals) / (statistics.vals+cmissing+csaturated+0) *100
      sxaddpar, mainheader, 'DATASKEW', statistics.skewness
      sxaddpar, mainheader, 'DATAKURT', statistics.kurtosis
      sxaddpar, mainheader, 'DATAP01', statistics.datap01
      sxaddpar, mainheader, 'DATAP10', statistics.datap10
      sxaddpar, mainheader, 'DATAP25', statistics.datap25
      sxaddpar, mainheader, 'DATAP75', statistics.datap75
      sxaddpar, mainheader, 'DATAP90', statistics.datap90
      sxaddpar, mainheader, 'DATAP95', statistics.datap95
      sxaddpar, mainheader, 'DATAP98', statistics.datap98
      sxaddpar, mainheader, 'DATAP99', statistics.datap99

      sxaddpar, mainheader, 'NEXP_PRP', float(nsteps)/float((*(rastersSJI[nrFW].rastercrs[nrCRS])).rasterPos)
      sxaddpar, mainheader, 'NEXP', nsteps
      sxaddpar, mainheader, 'NEXPOBS', l1to2log.filesexpected
      sxaddpar, mainheader, 'NRASTERP', (*(rastersSJI[nrFW].rastercrs[nrCRS])).rasterPos
      sxaddpar, mainheader, 'RASTYPDX', nrCRS+1
      sxaddpar, mainheader, 'RASTYPNX', rastersSJI[nrFW].nrasters
      sxaddpar, mainheader, 'RASRPT', 1
      sxaddpar, mainheader, 'RASNRPT', 1
      sxaddpar, mainheader, 'CADPL_AV', cadplav
      sxaddpar, mainheader, 'CADPL_DV', cadpldv
      sxaddpar, mainheader, 'CADEX_AV', cadexav
      sxaddpar, mainheader, 'CADEX_DV', cadexdv
      sxaddpar, mainheader, 'MISSOBS', l1to2log.nmissing
      sxaddpar, mainheader, 'MISSRAS', nmissraster

      sxaddpar, mainheader, 'IPRPVER', IRISl12_mostcommonvalue(IPRPVER)
      sxaddpar, mainheader, 'IPRPPDBV', IRISl12_mostcommonvalue(IPRPPDBV)
      sxaddpar, mainheader, 'IPRPDVER', IRISl12_mostcommonvalue(IPRPDVER)
      sxaddpar, mainheader, 'IPRPBVER', IRISl12_mostcommonvalue(IPRPBVER)

      sxaddpar, mainheader, 'PC1_1', OBSvars.PC1_1
      sxaddpar, mainheader, 'PC1_2', OBSvars.PC1_2
      sxaddpar, mainheader, 'PC2_1', OBSvars.PC2_1
      sxaddpar, mainheader, 'PC2_2', OBSvars.PC2_2
      sxaddpar, mainheader, 'PC3_1', OBSvars.PC3_1
      sxaddpar, mainheader, 'PC3_2', OBSvars.PC3_2

      sxaddpar, mainheader, 'NWIN', 1

      ;add window-specific keywords with counter to mainheader
      number=string(1, format='(I1)')
      sxaddpar, mainheader, 'TDET'+number, OBSvars.SJIdet[0]
      sxaddpar, mainheader, 'TDESC'+number, OBSvars.SJIdesc[0]
      sxaddpar, mainheader, 'TWAVE'+number, OBSvars.SJIwave[0]
      sxaddpar, mainheader, 'TWMIN'+number, OBSvars.SJIwavemin[0]
      sxaddpar, mainheader, 'TWMAX'+number, OBSvars.SJIwavemax[0]
      sxaddpar, mainheader, 'TDMEAN'+number, statistics.mean
      sxaddpar, mainheader, 'TDRMS'+number, statistics.stdev
      sxaddpar, mainheader, 'TDMEDN'+number, statistics.median
      sxaddpar, mainheader, 'TDMIN'+number, statistics.min
      sxaddpar, mainheader, 'TDMAX'+number, statistics.max
      sxaddpar, mainheader, 'TDVALS'+number, statistics.vals
      sxaddpar, mainheader, 'TMISSV'+number, cmissing
      sxaddpar, mainheader, 'TSATPX'+number, csaturated
      sxaddpar, mainheader, 'TSPIKE'+number, 0
      sxaddpar, mainheader, 'TTOTV'+number, statistics.vals+cmissing+csaturated+0
      sxaddpar, mainheader, 'TPCTD'+number, float(statistics.vals) / (statistics.vals+cmissing+csaturated+0) *100
      sxaddpar, mainheader, 'TDSKEW'+number, statistics.skewness
      sxaddpar, mainheader, 'TDKURT'+number, statistics.kurtosis
      sxaddpar, mainheader, 'TDP01_'+number, statistics.datap01
      sxaddpar, mainheader, 'TDP10_'+number, statistics.datap10
      sxaddpar, mainheader, 'TDP25_'+number, statistics.datap25
      sxaddpar, mainheader, 'TDP75_'+number, statistics.datap75
      sxaddpar, mainheader, 'TDP90_'+number, statistics.datap90
      sxaddpar, mainheader, 'TDP95_'+number, statistics.datap95
      sxaddpar, mainheader, 'TDP98_'+number, statistics.datap98
      sxaddpar, mainheader, 'TDP99_'+number, statistics.datap99
      sxaddpar, mainheader, 'TSR'+number, OBSvars.tsr+1
      sxaddpar, mainheader, 'TER'+number, OBSvars.ter+1
      sxaddpar, mainheader, 'TSC'+number, OBSvars.SJIsc_rtype[0]+1
      sxaddpar, mainheader, 'TEC'+number, OBSvars.SJIec_rtype[0]+1
      sxaddpar, mainheader, 'IPRPFV'+number, IRISl12_mostcommonvalue(IPRPFVER)
      sxaddpar, mainheader, 'IPRPGV'+number, IRISl12_mostcommonvalue(IPRPGVER)
      sxaddpar, mainheader, 'IPRPPV'+number, IRISl12_mostcommonvalue(IPRPPVER)

      ;sxaddpar, mainheader, 'CCDTYPE', 'SJI'

      fxaddpar, mainheader, 'CDELT1', OBSvars.CDELT1SJI     ;image scale in the x-direction
      fxaddpar, mainheader, 'CDELT2', OBSvars.CDELT2SJI     ;image scale in the y-direction
      fxaddpar, mainheader, 'CDELT3', cadexav      ;slit width for FUV,NUV


      fxaddpar, mainheader, 'CRPIX1', crpix1all      ;CRPIX1: location of sun/wave center in CCD x direction
      fxaddpar, mainheader, 'CRPIX2', crpix2all      ;CRPIX2: location of sun/wave center in CCD y direction
      fxaddpar, mainheader, 'CRPIX3', OBSvars.CRPIX3SJI      ;"1" for FUV/NUV

      ;fxaddpar, mainheader, 'CRS_TYPE', hdr[sjiind[0]].CRS_TYPE   ;CRS Type

      fxaddpar, mainheader, 'CRVAL1', crval1all     ;SOLARX (SJI), wavelength (FUV&NUV)
      fxaddpar, mainheader, 'CRVAL2', crval2all     ;SOLARY
      fxaddpar, mainheader, 'CRVAL3', OBSvars.CRVAL3SJI     ;SOLARX (FUV/NUV), time (SJI)

      fxaddpar, mainheader, 'CTYPE1', OBSvars.CTYPE1SJI     ;HPLN-TAN (SOLARX); WAVE for FUV/NUV
      fxaddpar, mainheader, 'CTYPE2', OBSvars.CTYPE2SJI     ;HPLT-TAN (SOLARY)
      fxaddpar, mainheader, 'CTYPE3', 'Time'     ;HPLN-TAN (SOLARX) for FUV/NUV

      fxaddpar, mainheader, 'CUNIT1', OBSvars.CUNIT1SJI     ;arcsec for SJI, Angstrom for second FUV CCD
      fxaddpar, mainheader, 'CUNIT2', OBSvars.CUNIT2SJI
      fxaddpar, mainheader, 'CUNIT3', 'seconds'

      sxaddpar, mainheader, 'KEYWDDOC', OBSvars.KEYWDDOC
      for i=0,N_ELEMENTS(history)-1 do if strcompress(history[i],/remove_all) ne '' then $
        sxaddpar, mainheader, 'HISTORY', history[i]
      sxaddpar, mainheader, 'HISTORY', 'level2  Version '+OBSvars.version



      file = filename + '_SJI_' + (*(rastersSJI[nrFW].rastercrs[nrCRS])).FWdesc + fns('_t###.fits', nrCRS)
      writefits, file, data, mainheader

      ;write extension with auxiliary data (time, PZTX, PZTY, Exposure duration, etc.)
      auxdata = dblarr(31, nsteps)
      auxdata[ 0, *] = time/1000d
      auxdata[ 1, *] = pztx
      auxdata[ 2, *] = pzty
      auxdata[ 3, *] = exptime
      auxdata[ 4, *] = slitx
      auxdata[ 5, *] = slity
      auxdata[ 6, *] = sumsptrlSJI
      auxdata[ 7, *] = sumspatSJI
      auxdata[ 8, *] = DataSrcSJI
      auxdata[ 9, *] = LUTIDsji
      auxdata[10, *] = xcen
      auxdata[11, *] = ycen
      auxdata[12, *] = obs_vr
      auxdata[13, *] = ophase
      auxdata[14, *] = pc1_1
      auxdata[15, *] = pc1_2
      auxdata[16, *] = pc2_1
      auxdata[17, *] = pc2_2
      auxdata[18, *] = pc3_1
      auxdata[19, *] = pc3_2
      auxdata[20, *] = IT01PMRF
      auxdata[21, *] = IT06TELM
      auxdata[22, *] = IT14SPPX
      auxdata[23, *] = IT15SPPY
      auxdata[24, *] = IT16SPNX
      auxdata[25, *] = IT17SPNY
      auxdata[26, *] = IT18SPPZ
      auxdata[27, *] = IT19SPNZ
      auxdata[28, *] = IPRPOFFX
      auxdata[29, *] = IPRPOFFY
      auxdata[30, *] = IPRPOFFF
      
      
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
      sxaddpar, header, 'IT01PSJI', 20, 'SJI PM Temperature (for pointing) (rowindex)'
      sxaddpar, header, 'IT06TSJI', 21, 'SJI MidTel Temperature (for pointing) (rowindex)'
      sxaddpar, header, 'IT14SSJI', 22, 'SJI Spectrograph +X Temperature (for wavelength) (rowindex)'
      sxaddpar, header, 'IT15SSJI', 23, 'SJI Spectrograph +Y Temperature (for wavelength) (rowindex)'
      sxaddpar, header, 'IT16SSJI', 24, 'SJI Spectrograph -X Temperature (for wavelength) (rowindex)'
      sxaddpar, header, 'IT17SSJI', 25, 'SJI Spectrograph -Y Temperature (for wavelength) (rowindex)'
      sxaddpar, header, 'IT18SSJI', 26, 'SJI Spectrograph +Z Temperature (for wavelength) (rowindex)'
      sxaddpar, header, 'IT19SSJI', 27, 'SJI Spectrograph -Z Temperature (for wavelength) (rowindex)'
      sxaddpar, header, 'POFFXSJI', 28, 'SJI X (spectral direction) shift in pixels (rowindex)'
      sxaddpar, header, 'POFFYSJI', 29, 'SJI Y (spatial direction) shift in pixels (rowindex)'
      sxaddpar, header, 'POFFFSJI', 30, 'SJI Flag indicating source of X and Y offsets (rowindex)'
      writefits, file, auxdata, header, /append





      ;now let's add another extension with a few string vectors
      lFRMid = max(strlen(FRMid))
      lFDBid = max(strlen(FDBid))
      lCRSid = max(strlen(CRSid))
      lfilenameSJI = max(strlen(filenameSJI))
      lIPRPDTMP = max(strlen(IPRPDTMP))
      ltot = lFRMid + lFDBid + lCRSid + lfilenameSJI + lIPRPDTMP

      if ltot gt 0 then begin
        ftcreate, ltot, nsteps, header, auxdata2

        for step=0,nsteps-1 do begin
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
          start=start+lfilenameSJI
          if strlen(IPRPDTMP[step]) gt 0 then $
            auxdata2[start:start+strlen(IPRPDTMP[step])-1, step] = byte(IPRPDTMP[step])
        endfor

      endif else ftcreate, 1, nsteps, header, auxdata2

      sxaddpar, header, 'TFIELDS', 5
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
      ix=ix+lfilenameSJI
      sxaddpar, header, 'TBCOL5', ix+1
      sxaddpar, header, 'TFORM5', 'A'+strcompress(string(lIPRPDTMP),/remove_all)
      sxaddpar, header, 'TTYPE5', 'SJI Comma-delimited string list of temperatures used by iris_make_dark'
      sxaddpar, header, 'PDTMPSJI', ix, 'SJI string list of temperatures of each exposure (rowindex)'
      sxaddpar, header, 'LPDTMSJI', lIPRPDTMP, 'SJI Length of string list (rows)'

      writefits, file, auxdata2, header, /append

      t2=systime(1)
      timemeas.writefile=timemeas.writefile+(t2-t1)

    endfor;nrCRS
  endfor;nrFW

  obj_destroy, constants

END
