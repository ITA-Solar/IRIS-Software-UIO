PRO IRISl12_saveSpectralData, hdr, files, hdrmap, rasters, filename, OBSvars, l1to2log, _extra=_extra $
    , timemeas=timemeas, scaled=scaled, l1p5=l1p5, maxl1files=maxl1files, rollangle=rollangle
  ; $Id: irisl12_savespectraldata.pro,v 1.119 2018/03/23 10:06:33 mawiesma Exp $  ;
    
  constants = obj_new('IRISsim_constants')
  
  tstart = str2utc(OBSvars.OBSstart)
  
  
  for rastertypenr=0,N_ELEMENTS(rasters)-1 do begin ;loop over all rastertypes
    print, 'Processing rastertype ' + strtrim(string(rastertypenr+1),2) + ' of ' + strtrim(string(N_ELEMENTS(rasters)),2)
    
    ;initialize some data, which will be the same for all rasterrepetitions within a rastertype
    OBSvars.sumspecmin = min((*(rasters[rastertypenr]).sumspecNUV)[*])
    OBSvars.sumspecminFUV = min((*(rasters[rastertypenr]).sumspecFUV)[*])
    ;OBSvars.sumspatmin=0
    FUVloginds = (*(rasters[rastertypenr]).FUVlogind)[*]
    countffile = 0
    if max(FUVloginds) ge 0 then begin
      flogindgood = where(FUVloginds ge 0, countflog)
      if countflog gt 0 then begin
        FUVloginds = FUVloginds[flogindgood]
        OBSvars.sumspecminFUV = min((*(rasters[rastertypenr]).sumspecFUV)[flogindgood])
        FUVfiles = hdrmap[FUVloginds]
        ffilegood = where(FUVfiles ge 0, countffile)
        if countffile gt 0 then begin
          FUVfiles = FUVfiles[ffilegood]
        ;IRISl12_getkeywords, 2, hdr[FUVfiles], OBSvars, /fuv
        endif
      endif
    endif
    
    NUVloginds = (*(rasters[rastertypenr]).NUVlogind)[*]
    countnfile = 0
    if max(NUVloginds) ge 0 then begin
      nlogindgood = where(NUVloginds ge 0, countnlog)
      if countnlog gt 0 then begin
        NUVloginds = NUVloginds[nlogindgood]
        OBSvars.sumspecmin = min((*(rasters[rastertypenr]).sumspecNUV)[nlogindgood])
        NUVfiles = hdrmap[NUVloginds]
        nfilegood = where(NUVfiles ge 0, countnfile)
        if countnfile gt 0 then begin
          NUVfiles = NUVfiles[nfilegood]
        ;IRISl12_getkeywords, 2, hdr[NUVfiles], OBSvars, /nuv
        endif
      endif
    endif
    
    OBSvars.nregFUV = 0
    ;OBSvars.FUVsr_rtype[*] = 0
    ;OBSvars.FUVer_rtype[*] = 0
    OBSvars.FUVsc_rtype[*] = 0
    OBSvars.FUVec_rtype[*] = 0
    OBSvars.FUVok_rtype[*] = 0
    OBSvars.FUVdet[*] = 'FUV'
    OBSvars.crval1FUV[*] = 0
    OBSvars.FUVwavemin[*] = 0
    OBSvars.FUVwavemax[*] = 0
    OBSvars.FUVdesc[*] = ''
    OBSvars.FUVwave[*] = 0
    if countffile gt 0 then begin
      IRISl12_getkeywords, 3, hdr[FUVfiles], OBSvars, /fuv
    endif
    
    OBSvars.nregNUV = 0
    ;OBSvars.NUVsr_rtype[*] = 0
    ;OBSvars.NUVer_rtype[*] = 0
    OBSvars.NUVsc_rtype[*] = 0
    OBSvars.NUVec_rtype[*] = 0
    OBSvars.NUVok_rtype[*] = 0
    OBSvars.NUVdet[*] = 'NUV'
    OBSvars.crval1NUV[*] = 0
    OBSvars.NUVwavemin[*] = 0
    OBSvars.NUVwavemax[*] = 0
    OBSvars.NUVdesc[*] = ''
    OBSvars.NUVwave[*] = 0
    if countnfile gt 0 then begin
      IRISl12_getkeywords, 3, hdr[NUVfiles], OBSvars, /nuv
    endif
    
    
    ;only do the raster, when there actually was a raster
    if OBSvars.nregFUV + OBSvars.nregNUV gt 0 then begin
    
      ;first loop to get the cadences, only if it is not a sit-and-stare
      if ~rasters[rastertypenr].sitandstare then begin
        cadex=fltarr(rasters[rastertypenr].nRasters)
        for rasternr=0,rasters[rastertypenr].nRasters-1 do begin ;loop over all rasters within a rastertype
        
          ;initialize variables which will be the same for the whole rasterrepetition
          ;different for each rastertype and -repetition
          OBSvars.Date_OBS=''
          OBSvars.Date_End=''
          OBSvars.SAT_ROT = 0
          OBSvars.crval2FUV = 0
          OBSvars.crval3FUV = 0
          OBSvars.crval2NUV = OBSvars.crval2FUV
          OBSvars.crval3NUV = OBSvars.crval3FUV
          FUVloginds = (*(rasters[rastertypenr]).FUVlogind)[*, rasternr]
          if max(FUVloginds) ge 0 then begin
            flogindgood = where(FUVloginds ge 0, countflog)
            if countflog gt 0 then begin
              FUVloginds = FUVloginds[flogindgood]
              FUVfiles = hdrmap[FUVloginds]
              ffilegood = where(FUVfiles ge 0, countffile)
              if countffile gt 0 then begin
                filesind = FUVfiles[ffilegood]
              endif else filesind = !NULL
            endif
          endif
          
          NUVloginds = (*(rasters[rastertypenr]).NUVlogind)[*, rasternr]
          if max(NUVloginds) ge 0 then begin
            nlogindgood = where(NUVloginds ge 0, countnlog)
            if countnlog gt 0 then begin
              NUVloginds = NUVloginds[nlogindgood]
              NUVfiles = hdrmap[NUVloginds]
              nfilegood = where(NUVfiles ge 0, countnfile)
              if countnfile gt 0 then begin
                if N_ELEMENTS(filesind) gt 0 then filesind = [filesind, NUVfiles[nfilegood]] $
                else filesind =  NUVfiles[nfilegood]
              endif
            endif
          endif
          if N_ELEMENTS(filesind) gt 0 then $
            IRISl12_getkeywords, 4, hdr[filesind], OBSvars
            
            
          tstartcad = str2utc(OBSvars.DATE_OBS)
          tendcad = str2utc(OBSvars.DATE_END)
          cadex[rasternr] = ((tendcad.mjd-tstartcad.mjd)*86400000L + tendcad.time-tstartcad.time)
          if rasters[rastertypenr].nSteps gt 1 then $
            cadex[rasternr] = cadex[rasternr] * float(rasters[rastertypenr].nSteps) / float(rasters[rastertypenr].nSteps-1)
            
        endfor
      endif ;not a sit-and-stare
      
      
      ;real loop
      for rasternr=0,rasters[rastertypenr].nRasters-1 do begin ;loop over all rasters within a rastertype
        print, 'Processing rasterrepetition ' + strtrim(string(rasternr+1),2) + ' of ' + strtrim(string(rasters[rastertypenr].nRasters),2)
        print, 'Number of steps: ' + strtrim(string(rasters[rastertypenr].nSteps),2)
      
        ;initialize variables which will be the same for the whole rasterrepetition
        ;different for each rastertype and -repetition
        OBSvars.Date_OBS=''
        OBSvars.Date_End=''
        OBSvars.SAT_ROT = 0
        OBSvars.crval2FUV = 0
        OBSvars.crval3FUV = 0
        OBSvars.crval2NUV = OBSvars.crval2FUV
        OBSvars.crval3NUV = OBSvars.crval3FUV
        FUVloginds = (*(rasters[rastertypenr]).FUVlogind)[*, rasternr]
        if max(FUVloginds) ge 0 then begin
          flogindgood = where(FUVloginds ge 0, countflog)
          if countflog gt 0 then begin
            FUVloginds = FUVloginds[flogindgood]
            FUVfiles = hdrmap[FUVloginds]
            ffilegood = where(FUVfiles ge 0, countffile)
            if countffile gt 0 then begin
              filesind = FUVfiles[ffilegood]
            endif else filesind = !NULL
          endif
        endif
        
        NUVloginds = (*(rasters[rastertypenr]).NUVlogind)[*, rasternr]
        if max(NUVloginds) ge 0 then begin
          nlogindgood = where(NUVloginds ge 0, countnlog)
          if countnlog gt 0 then begin
            NUVloginds = NUVloginds[nlogindgood]
            NUVfiles = hdrmap[NUVloginds]
            nfilegood = where(NUVfiles ge 0, countnfile)
            if countnfile gt 0 then begin
              if N_ELEMENTS(filesind) gt 0 then filesind = [filesind, NUVfiles[nfilegood]] $
              else filesind =  NUVfiles[nfilegood]
            endif
          endif
        endif
        if N_ELEMENTS(filesind) gt 0 then $
          IRISl12_getkeywords, 4, hdr[filesind], OBSvars
          
        nmissraster=0 ;number of missing files in this raster
        
        ;each raster will be saved into one level-2 file
        data = ptrarr(OBSvars.nregFUV + OBSvars.nregNUV)
        nymax=0
        for region=0,OBSvars.nregFUV-1 do begin ;loop over all FUV regions
          if OBSvars.FUVok_rtype[region] then begin
            nx = OBSvars.FUVec_rtype[region]-OBSvars.FUVsc_rtype[region]+1
            ny = OBSvars.ter-OBSvars.tsr+1
            data[region] = ptr_new(make_array(nx, ny, rasters[rastertypenr].nSteps,/float,value=!values.f_nan))
            if ny gt nymax then nymax=ny
          endif else data[region] = ptr_new(make_array(3, 3, rasters[rastertypenr].nSteps,/float,value=!values.f_nan))
        endfor
        for region=OBSvars.nregFUV,OBSvars.nregFUV+OBSvars.nregNUV-1 do begin ;loop over all NUV regions
          if OBSvars.NUVok_rtype[region-OBSvars.nregFUV] then begin
            nx = OBSvars.NUVec_rtype[region-OBSvars.nregFUV]-OBSvars.NUVsc_rtype[region-OBSvars.nregFUV]+1
            ny = OBSvars.ter-OBSvars.tsr+1
            data[region] = ptr_new(make_array(nx, ny, rasters[rastertypenr].nSteps,/float,value=!values.f_nan))
            if ny gt nymax then nymax=ny
          endif else data[region] = ptr_new(make_array(3, 3, rasters[rastertypenr].nSteps,/float,value=!values.f_nan))
        endfor
        
        ;calculate basic step size
        ;theoretical values, real values are calculated further below if there are any
        if rasters[rastertypenr].nSteps gt 1 then begin
          stepsizes = dblarr(rasters[rastertypenr].nSteps-1)
          for i=1,rasters[rastertypenr].nSteps-1 do begin
            stepsizes[i-1] = (*(rasters[rastertypenr]).PZTxy)[0,i] - (*(rasters[rastertypenr]).PZTxy)[0,i-1]
            endfor
        endif else stepsizes=0
        basicStepSizeAv = mean(stepsizes)
        basicStepSizeDv = stddev(stepsizes)

        ;calculate FOVX/Y and X/YCEN
        ;FOVX: theoretical FOVX, calculated from xml-files (planned values)
        ;real value is calculated further down
        ;FOVy: real values
        minp = min((*(rasters[rastertypenr]).PZTxy)[0,*], max=maxp)
        ny = nymax
        FOVX = maxp - minp
        if FOVX lt OBSvars.cdelt3FUV then FOVX = OBSvars.cdelt3FUV
        nx = FOVX / OBSvars.cdelt3FUV
        FOVY = ny * OBSvars.cdelt2FUV
        
        
        ;get values for arrays (auxialiary extensions)
        FRMid = strarr(rasters[rastertypenr].nSteps)
        time = make_array(rasters[rastertypenr].nSteps, /long, value=constants->get_missingvalue())
        
        expdurFUV = fltarr(rasters[rastertypenr].nSteps)
        filenameFUV = strarr(rasters[rastertypenr].nSteps)
        FDBidFUV = strarr(rasters[rastertypenr].nSteps)
        CRSidFUV = strarr(rasters[rastertypenr].nSteps)
        sumsptrlFUV = (*(rasters[rastertypenr]).sumspecFUV)[*, rasternr]
        sumspatFUV = (*(rasters[rastertypenr]).sumspatFUV)[*, rasternr]
        DataSrcFUV = make_array(rasters[rastertypenr].nSteps, value=-1.0)
        LUTIDfuv = intarr(rasters[rastertypenr].nSteps)
        
        expdurNUV = fltarr(rasters[rastertypenr].nSteps)
        filenameNUV = strarr(rasters[rastertypenr].nSteps)
        FDBidNUV = strarr(rasters[rastertypenr].nSteps)
        CRSidNUV = strarr(rasters[rastertypenr].nSteps)
        sumsptrlNUV = (*(rasters[rastertypenr]).sumspecNUV)[*, rasternr]
        sumspatNUV = (*(rasters[rastertypenr]).sumspatNUV)[*, rasternr]
        DataSrcNUV = make_array(rasters[rastertypenr].nSteps, value=-1.0)
        LUTIDnuv = intarr(rasters[rastertypenr].nSteps)
        
        pztx = dblarr(rasters[rastertypenr].nSteps)
        pzty = dblarr(rasters[rastertypenr].nSteps)
        xcen = dblarr(rasters[rastertypenr].nSteps)
        ycen = dblarr(rasters[rastertypenr].nSteps)
        crpix2 = dblarr(rasters[rastertypenr].nSteps)
        crpix3 = dblarr(rasters[rastertypenr].nSteps)
        crval2 = dblarr(rasters[rastertypenr].nSteps)
        crval3 = dblarr(rasters[rastertypenr].nSteps)
        obs_vr = dblarr(rasters[rastertypenr].nSteps)
        ophase = dblarr(rasters[rastertypenr].nSteps)
        pc1_1 = dblarr(rasters[rastertypenr].nSteps)
        pc1_2 = dblarr(rasters[rastertypenr].nSteps)
        pc2_1 = dblarr(rasters[rastertypenr].nSteps)
        pc2_2 = dblarr(rasters[rastertypenr].nSteps)
        pc3_1 = dblarr(rasters[rastertypenr].nSteps)
        pc3_2 = dblarr(rasters[rastertypenr].nSteps)
        pc3_3 = dblarr(rasters[rastertypenr].nSteps)
        pc2_3 = dblarr(rasters[rastertypenr].nSteps)
        
        IPRPVER = dblarr(rasters[rastertypenr].nSteps)
        IPRPPDBV = dblarr(rasters[rastertypenr].nSteps)
        IPRPDVER = lonarr(rasters[rastertypenr].nSteps)
        IPRPBVER = lonarr(rasters[rastertypenr].nSteps)
        IPRPFVER = lonarr(rasters[rastertypenr].nSteps)
        IPRPGVER = lonarr(rasters[rastertypenr].nSteps)
        IPRPPVER = lonarr(rasters[rastertypenr].nSteps)
        IPRPFVERNUV = lonarr(rasters[rastertypenr].nSteps)
        IPRPGVERNUV = lonarr(rasters[rastertypenr].nSteps)
        IPRPPVERNUV = lonarr(rasters[rastertypenr].nSteps)

        IT01PMRF = dblarr(rasters[rastertypenr].nSteps)       ;       PM Temperature (for pointing) (IMG)
        IT06TELM = dblarr(rasters[rastertypenr].nSteps)        ;       MidTel Temperature (for pointing) (IMG)
        IT14SPPX = dblarr(rasters[rastertypenr].nSteps)        ;       Spectrograph +X Temperature (for wavelength) (IMG)
        IT15SPPY = dblarr(rasters[rastertypenr].nSteps)         ;       Spectrograph +Y Temperature (for wavelength) (IMG)
        IT16SPNX = dblarr(rasters[rastertypenr].nSteps)       ;       Spectrograph -X Temperature (for wavelength) (IMG)
        IT17SPNY = dblarr(rasters[rastertypenr].nSteps)        ;       Spectrograph -Y Temperature (for wavelength) (IMG)
        IT18SPPZ = dblarr(rasters[rastertypenr].nSteps)          ;       Spectrograph +Z Temperature (for wavelength) (IMG)
        IT19SPNZ = dblarr(rasters[rastertypenr].nSteps)         ;       Spectrograph -Z Temperature (for wavelength) (IMG)
        IPRPDTMP = strarr(rasters[rastertypenr].nSteps)        ;       Comma-delimited string list of temperatures used by iris_make_dark (IMG)
        IPRPOFFX = dblarr(rasters[rastertypenr].nSteps)         ;       X (spectral direction) shift in pixels (IMG)
        IPRPOFFY = dblarr(rasters[rastertypenr].nSteps)         ;       Y (spatial direction) shift in pixels (IMG)
        IPRPOFFF = dblarr(rasters[rastertypenr].nSteps)         ;       Flag indicating source of X and Y offsets (IMG)

        IT01PMRFNUV = dblarr(rasters[rastertypenr].nSteps)       ;       PM Temperature (for pointing) (IMG)
        IT06TELMNUV = dblarr(rasters[rastertypenr].nSteps)        ;       MidTel Temperature (for pointing) (IMG)
        IT14SPPXNUV = dblarr(rasters[rastertypenr].nSteps)        ;       Spectrograph +X Temperature (for wavelength) (IMG)
        IT15SPPYNUV = dblarr(rasters[rastertypenr].nSteps)         ;       Spectrograph +Y Temperature (for wavelength) (IMG)
        IT16SPNXNUV = dblarr(rasters[rastertypenr].nSteps)       ;       Spectrograph -X Temperature (for wavelength) (IMG)
        IT17SPNYNUV = dblarr(rasters[rastertypenr].nSteps)        ;       Spectrograph -Y Temperature (for wavelength) (IMG)
        IT18SPPZNUV = dblarr(rasters[rastertypenr].nSteps)          ;       Spectrograph +Z Temperature (for wavelength) (IMG)
        IT19SPNZNUV = dblarr(rasters[rastertypenr].nSteps)         ;       Spectrograph -Z Temperature (for wavelength) (IMG)
        IPRPDTMPNUV = strarr(rasters[rastertypenr].nSteps)        ;       Comma-delimited string list of temperatures used by iris_make_dark (IMG)
        IPRPOFFXNUV = dblarr(rasters[rastertypenr].nSteps)         ;       X (spectral direction) shift in pixels (IMG)
        IPRPOFFYNUV = dblarr(rasters[rastertypenr].nSteps)         ;       Y (spatial direction) shift in pixels (IMG)
        IPRPOFFFNUV = dblarr(rasters[rastertypenr].nSteps)         ;       Flag indicating source of X and Y offsets (IMG)

        date_obs='' ;start of rasterrepetition
        date_end='' ;end of rasterrepetition
        
        dpztx = !NULL
        
        statisticstemp={mean:0.0, $
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
        statistics = make_array(OBSvars.nregFUV+OBSvars.nregNUV+1, value=statisticstemp)
        
        fuvfilecounter=0
        fgroup=-1
        nuvfilecounter=0
        ngroup=-1
        for step=0,rasters[rastertypenr].nSteps-1 do begin ;loop over all steps within a raster
          FUVlogind = (*(rasters[rastertypenr]).FUVlogind)[step, rasternr]
          if FUVlogind ge 0 then begin
            FUVfileind = hdrmap[FUVlogind]
            if FUVfileind ge 0 then begin
              ;print, files[FUVfileind]
              t1=systime(1)
              if (fuvfilecounter-fgroup*maxl1files eq N_ELEMENTS(hdrFUVall)) || (fgroup eq -1) then begin
                if fuvfilecounter+maxl1files gt N_ELEMENTS(ffilegood) then endind=N_ELEMENTS(ffilegood)-1 $
                else endind=fuvfilecounter+maxl1files-1
                fileind = ffilegood[fuvfilecounter:endind]
                filenamefall = files[FUVfiles[fileind]]
                if l1p5 eq 1 then begin
                  read_iris, files[FUVfiles[fileind]], hdrFUVall, fuvall, /history, /use_shared, /no_shell, /uncomp_delete, _extra=_extra
                endif else begin
                  hdrFUVall = !NULL
                  ind = FUVfiles[fileind]
                  ;divide files for different CRS
                  crsid = hdr[ind[UNIQ(hdr[ind].IICRSID, SORT(hdr[ind].IICRSID))]].IICRSID
                  for icrs=0,N_ELEMENTS(crsid)-1 do begin
                    crscur = where(hdr[ind].IICRSID eq crsid[icrs], count)
                    if count gt 0 then begin
                      iris_prep, files, ind[crscur], hdrtemp, datatemp, /use_shared, /no_shell, /uncomp_delete, _extra=_extra, /run_time, /strict
                      if N_ELEMENTS(hdrFUVall) eq 0 then begin
                        hdrFUVall = make_array(N_ELEMENTS(ind), value=hdrtemp[0])
                        fuvall = make_array((size(datatemp))[1], (size(datatemp))[2], N_ELEMENTS(ind), /float)
                      endif
                      hdrFUVall[crscur] = hdrtemp
                      for i=0,count-1 do fuvall[*,*,crscur[i]] = datatemp[*,*,i]
                    endif
                  endfor 
                  hdrFUVall[*].LVL_NUM = 1.51
                endelse
                if N_ELEMENTS(rollangle) gt 0 then begin
                  t=tag_names(hdrFUVall)
                  w=where(t eq 'SAT_ROT', c)
                  if c gt 0 then begin
                    hdrFUVall[*].SAT_ROT=rollangle
                  endif else begin
                    hdrFUVall = add_tag(hdrFUVall, rollangle, 'SAT_ROT')
                  endelse
                endif
                fgroup=fgroup+1
              endif
              fuv = fuvall[*,*,fuvfilecounter-fgroup*maxl1files]
              hdrFUV = hdrFUVall[fuvfilecounter-fgroup*maxl1files]
              filenameFUV[step] = filenamefall[fuvfilecounter-fgroup*maxl1files]
              fuvfilecounter=fuvfilecounter+1
              t2=systime(1)
              timemeas.readfile=timemeas.readfile+(t2-t1)
              
              ;check for specific corrupted image
              if hdrFUV.cropid eq 0 && hdrFUV.iicrsid ne 1 then begin
                ;corrupted image
                if l1to2log.nbadfiles eq 0 then begin
                  l1to2log.nbadfiles = 1
                  l1to2log.badfiles = ptr_new(filenameFUV[step])
                  l1to2log.badfilesreason = ptr_new('cropid eq 0 && iicrsid ne 1')
                endif else begin
                  l1to2log.nbadfiles = l1to2log.nbadfiles + 1
                  *l1to2log.badfiles = [*l1to2log.badfiles, filenameFUV[step]]
                  *l1to2log.badfilesreason = [*l1to2log.badfilesreason, 'cropid eq 0 && iicrsid ne 1']
                endelse
                FUVfileind=-1
                nmissraster=nmissraster+1
              endif
              
              IRISl12_getkeywords, 5, hdrFUV, OBSvars, /fuv
              
            ;image might not be (fully) flipped, so we check here
            ;assume it is correctly flipped
            ;              win_flip = gt_tagval(hdrFUV, 'WIN_FLIP', missing=3)
            ;              if win_flip eq 0 then begin
            ;                fuv = reverse(fuv, 2)
            ;              endif else if win_flip eq 2 then begin
            ;                fuv = reverse(fuv, 2)
            ;              endif
            endif else nmissraster=nmissraster+1
          endif else FUVfileind=-1
          
          NUVlogind = (*(rasters[rastertypenr]).NUVlogind)[step, rasternr]
          if NUVlogind ge 0 then begin
            NUVfileind = hdrmap[NUVlogind]
            if NUVfileind ge 0 then begin
              ;print, files[NUVfileind]
              t1=systime(1)
              if (nuvfilecounter-ngroup*maxl1files eq N_ELEMENTS(hdrNUVall)) || (ngroup eq -1) then begin
                if nuvfilecounter+maxl1files gt N_ELEMENTS(nfilegood) then endind=N_ELEMENTS(nfilegood)-1 $
                else endind=nuvfilecounter+maxl1files-1
                fileind = nfilegood[nuvfilecounter:endind]
                filenamenall = files[NUVfiles[fileind]]
                if l1p5 eq 1 then begin
                  read_iris, files[NUVfiles[fileind]], hdrNUVall, nuvall, /history, /use_shared, /no_shell, /uncomp_delete, _extra=_extra
                endif else begin
                  hdrNUVall = !NULL
                  ind = NUVfiles[fileind]
                  ;divide files for different CRS
                  crsid = hdr[ind[UNIQ(hdr[ind].IICRSID, SORT(hdr[ind].IICRSID))]].IICRSID
                  for icrs=0,N_ELEMENTS(crsid)-1 do begin
                    crscur = where(hdr[ind].IICRSID eq crsid[icrs], count)
                    if count gt 0 then begin
                      iris_prep, files, ind[crscur], hdrtemp, datatemp, /use_shared, /no_shell, /uncomp_delete, _extra=_extra, /run_time, /strict
                      if N_ELEMENTS(hdrNUVall) eq 0 then begin
                        hdrNUVall = make_array(N_ELEMENTS(ind), value=hdrtemp[0])
                        nuvall = make_array((size(datatemp))[1], (size(datatemp))[2], N_ELEMENTS(ind), /float)
                      endif
                      hdrNUVall[crscur] = hdrtemp
                      for i=0,count-1 do nuvall[*,*,crscur[i]] = datatemp[*,*,i]
                    endif
                  endfor 
                  hdrNUVall[*].LVL_NUM = 1.51
                endelse
                if N_ELEMENTS(rollangle) gt 0 then begin
                  t=tag_names(hdrNUVall)
                  w=where(t eq 'SAT_ROT', c)
                  if c gt 0 then begin
                    hdrNUVall[*].SAT_ROT=rollangle
                  endif else begin
                    hdrNUVall = add_tag(hdrNUVall, rollangle, 'SAT_ROT')
                  endelse
                endif
                ngroup=ngroup+1
              endif
              nuv = nuvall[*,*,nuvfilecounter-ngroup*maxl1files]
              hdrNUV = hdrNUVall[nuvfilecounter-ngroup*maxl1files]
              filenameNUV[step] = filenamenall[nuvfilecounter-ngroup*maxl1files]
              nuvfilecounter=nuvfilecounter+1
              t2=systime(1)
              timemeas.readfile=timemeas.readfile+(t2-t1)
              
              ;check for specific corrupted image
              if hdrNUV.cropid eq 0 && hdrNUV.iicrsid ne 1 then begin
                ;corrupted image
                if l1to2log.nbadfiles eq 0 then begin
                  l1to2log.nbadfiles = 1
                  l1to2log.badfiles = ptr_new(filenameNUV[step])
                  l1to2log.badfilesreason = ptr_new('cropid eq 0 && iicrsid ne 1')
                endif else begin
                  l1to2log.nbadfiles = l1to2log.nbadfiles + 1
                  *l1to2log.badfiles = [*l1to2log.badfiles, filenameNUV[step]]
                  *l1to2log.badfilesreason = [*l1to2log.badfilesreason, 'cropid eq 0 && iicrsid ne 1']
                endelse
                NUVfileind=-1
                nmissraster=nmissraster+1
              endif

              IRISl12_getkeywords, 5, hdrNUV, OBSvars, /nuv
            ;image might not be (fully) flipped, so we check here
            ;assume it is correctly flipped
            ;              win_flip = gt_tagval(hdrNUV, 'WIN_FLIP', missing=3)
            ;              if win_flip eq 0 then begin
            ;                nuv = reverse(nuv, 1)
            ;              endif else if win_flip eq 1 then begin
            ;                nuv = reverse(nuv, 1)
            ;              endif
            endif else nmissraster=nmissraster+1
          endif else NUVfileind=-1
          
          
          ;get values for arrays (auxialiary extensions)
          if FUVfileind ge 0 then begin
            FRMid[step] = strcompress(gt_tagval(hdrFUV, 'ISQFLTID', missing=''), /remove_all)
            t2 = gt_tagval(hdrFUV, 'DATE_OBS', missing='')
            if t2 ne '' then begin
              t2 = str2utc(t2)
              time[step] = (t2.mjd-tstart.mjd)*86400000L + t2.time-tstart.time
            endif
          endif else if NUVfileind ge 0 then begin
            FRMid[step] = strcompress(gt_tagval(hdrNUV, 'ISQFLTID', missing=''), /remove_all)
            t2 = gt_tagval(hdrNUV, 'DATE_OBS', missing='')
            if t2 ne '' then begin
              t2 = str2utc(t2)
              time[step] = (t2.mjd-tstart.mjd)*86400000L + t2.time-tstart.time
            endif
          endif else begin
            time[step] = (*(rasters[rastertypenr]).time)[step, rasternr]
          endelse
          
          pztx[step] = OBSvars.PZTx
          pzty[step] = OBSvars.PZTy
          
          if FUVfileind ge 0 then begin
            expdurFUV[step] = gt_tagval(hdrFUV, 'EXPTIME', missing=0.0)
            FDBidFUV[step] = strcompress(gt_tagval(hdrFUV, 'IIFDBID', missing=''), /remove_all)
            CRSidFUV[step] = strcompress(gt_tagval(hdrFUV, 'IICRSID', missing=''), /remove_all)
            DataSrcFUV[step] = OBSvars.DSRCF
            LUTIDfuv[step] = OBSvars.LUTIDfuv
            obs_vr[step] = OBSvars.obs_vr
            ophase[step] = OBSvars.ophase
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
            IPRPOFFX[step] = OBSvars.IPRPOFFX         ;       X (spectral direction) shift in pixels (IMG)
            IPRPOFFY[step] = OBSvars.IPRPOFFY          ;       Y (spatial direction) shift in pixels (IMG)
            IPRPOFFF[step] = OBSvars.IPRPOFFF          ;       Flag indicating source of X and Y offsets (IMG)
            if sumsptrlFUV[step] ne OBSvars.sumspecFUV then box_message,['wrong SUMSPTRL in FUV file',files[FUVfileind]];write errorlog
            if sumspatFUV[step] ne OBSvars.sumspatFUV then box_message,['wrong SUMSPAT in FUV file',files[FUVfileind]];write errorlog
            sumFUV = (sumsptrlFUV[step] ne OBSvars.sumspecFUV) || (sumspatFUV[step] ne OBSvars.sumspatmin)
            crpix1 = gt_tagval(hdrFUV, 'CRPIX1', missing=0.0)
            crpix2[step] = gt_tagval(hdrFUV, 'CRPIX2', missing=0.0)
            crval2[step] = gt_tagval(hdrFUV, 'CRVAL2', missing=0.0)
            cdelt2 = gt_tagval(hdrFUV, 'CDELT2', missing=0.0)
            crval3[step] = gt_tagval(hdrFUV, 'CRVAL3', missing=0.0)
            cdelt3 = gt_tagval(hdrFUV, 'CDELT3', missing=0.0)
            naxis1 = constants->get_PixCCDx()*2 / OBSvars.sumspecminFUV
            naxis2 = OBSvars.ter - OBSvars.tsr + 1
            pc1_1[step] = gt_tagval(hdrFUV, 'PC1_1', missing=0.0)
            pc1_2[step] = gt_tagval(hdrFUV, 'PC1_2', missing=0.0)
            pc2_1[step] = gt_tagval(hdrFUV, 'PC2_1', missing=0.0)
            pc2_2[step] = gt_tagval(hdrFUV, 'PC2_2', missing=0.0)
            pc3_1[step] = gt_tagval(hdrFUV, 'PC3_1', missing=0.0)
            pc3_2[step] = gt_tagval(hdrFUV, 'PC3_2', missing=0.0)
            if sumFUV then begin
              crpix1 = crpix1 * sumsptrlFUV[step] / OBSvars.sumspecminFUV
              crpix2[step] = crpix2[step] * sumspatFUV[step] / OBSvars.sumspatmin
              cdelt2 = cdelt2 / sumspatFUV[step] * OBSvars.sumspatmin
            endif
            crpix2[step] = crpix2[step] - OBSvars.tsr
            xcen[step] = crval3[step] + cdelt2 * pc3_2[step]*((naxis2+1)/2.0 - crpix2[step])
            ycen[step] = crval2[step] + cdelt2 * pc2_2[step]*((naxis2+1)/2.0 - crpix2[step])
            crval3[step] = xcen[step]
            crval2[step] = ycen[step]
            crpix3[step] = step+1.0
            crpix2[step] = (naxis2+1)/2.0
            
            if N_ELEMENTS(history) eq 0 then history = gt_tagval(hdrFUV, 'HISTORY', missing='') $
            else if N_ELEMENTS(history) eq 1 && strcompress(history[0], /remove_all) eq '' then history = gt_tagval(hdrFUV, 'HISTORY', missing='')
          endif
          
          if NUVfileind ge 0 then begin
            expdurNUV[step] = gt_tagval(hdrNUV, 'EXPTIME', missing=0.0)
            FDBidNUV[step] = strcompress(gt_tagval(hdrNUV, 'IIFDBID', missing=''), /remove_all)
            CRSidNUV[step] = strcompress(gt_tagval(hdrNUV, 'IICRSID', missing=''), /remove_all)
            DataSrcNUV[step] = OBSvars.DSRCN
            LUTIDnuv[step] = OBSvars.LUTIDnuv
            obs_vr[step] = OBSvars.obs_vr
            ophase[step] = OBSvars.ophase
            IPRPVER[step] = OBSvars.IPRPVER
            IPRPPDBV[step] = OBSvars.IPRPPDBV
            IPRPDVER[step] = OBSvars.IPRPDVER
            IPRPBVER[step] = OBSvars.IPRPBVER
            IPRPFVERNUV[step] = OBSvars.IPRPFVERNUV
            IPRPGVERNUV[step] = OBSvars.IPRPGVERNUV
            IPRPPVERNUV[step] = OBSvars.IPRPPVERNUV
            IT01PMRFNUV[step] = OBSvars.IT01PMRFNUV       ;       PM Temperature (for pointing) (IMG)
            IT06TELMNUV[step] = OBSvars.IT06TELMNUV        ;       MidTel Temperature (for pointing) (IMG)
            IT14SPPXNUV[step] = OBSvars.IT14SPPXNUV        ;       Spectrograph +X Temperature (for wavelength) (IMG)
            IT15SPPYNUV[step] = OBSvars.IT15SPPYNUV         ;       Spectrograph +Y Temperature (for wavelength) (IMG)
            IT16SPNXNUV[step] = OBSvars.IT16SPNXNUV       ;       Spectrograph -X Temperature (for wavelength) (IMG)
            IT17SPNYNUV[step] = OBSvars.IT17SPNYNUV        ;       Spectrograph -Y Temperature (for wavelength) (IMG)
            IT18SPPZNUV[step] = OBSvars.IT18SPPZNUV          ;       Spectrograph +Z Temperature (for wavelength) (IMG)
            IT19SPNZNUV[step] = OBSvars.IT19SPNZNUV         ;       Spectrograph -Z Temperature (for wavelength) (IMG)
            IPRPDTMPNUV[step] = OBSvars.IPRPDTMPNUV        ;       Comma-delimited string list of temperatures used by iris_make_dark (IMG)
            IPRPOFFXNUV[step] = OBSvars.IPRPOFFXNUV         ;       X (spectral direction) shift in pixels (IMG)
            IPRPOFFYNUV[step] = OBSvars.IPRPOFFYNUV          ;       Y (spatial direction) shift in pixels (IMG)
            IPRPOFFFNUV[step] = OBSvars.IPRPOFFFNUV          ;       Flag indicating source of X and Y offsets (IMG)
            if sumsptrlNUV[step] ne OBSvars.sumspecNUV then box_message,['wrong SUMSPTRL in NUV file',files[NUVfileind]];write errorlog
            if sumspatNUV[step] ne OBSvars.sumspatNUV then box_message,['wrong SUMSPAT in NUV file',files[NUVfileind]];write errorlog
            sumNUV = (sumsptrlNUV[step] ne OBSvars.sumspecmin) || (sumspatNUV[step] ne OBSvars.sumspatmin)
            if (xcen[step] eq 0) && (ycen[step] eq 0) then begin
              crpix1 = gt_tagval(hdrNUV, 'CRPIX1', missing=0.0)
              crpix2[step] = gt_tagval(hdrNUV, 'CRPIX2', missing=0.0)
              crval2[step] = gt_tagval(hdrNUV, 'CRVAL2', missing=0.0)
              cdelt2 = gt_tagval(hdrNUV, 'CDELT2', missing=0.0)
              crval3[step] = gt_tagval(hdrNUV, 'CRVAL3', missing=0.0)
              cdelt3 = gt_tagval(hdrNUV, 'CDELT3', missing=0.0)
              naxis1 = constants->get_PixCCDx() / OBSvars.sumspecmin
              naxis2 = OBSvars.ter - OBSvars.tsr + 1
              pc1_1[step] = gt_tagval(hdrNUV, 'PC1_1', missing=0.0)
              pc1_2[step] = gt_tagval(hdrNUV, 'PC1_2', missing=0.0)
              pc2_1[step] = gt_tagval(hdrNUV, 'PC2_1', missing=0.0)
              pc2_2[step] = gt_tagval(hdrNUV, 'PC2_2', missing=0.0)
              pc3_1[step] = gt_tagval(hdrNUV, 'PC3_1', missing=0.0)
              pc3_2[step] = gt_tagval(hdrNUV, 'PC3_2', missing=0.0)
              if sumNUV then begin
                crpix1 = crpix1 * sumsptrlNUV[step] / OBSvars.sumspecmin
                crpix2[step] = crpix2[step] * sumspatNUV[step] / OBSvars.sumspatmin
                cdelt2 = cdelt2 / sumspatNUV[step] * OBSvars.sumspatmin
              endif
              crpix2[step] = crpix2[step] - OBSvars.tsr
              xcen[step] = crval3[step] + cdelt2 * pc3_2[step]*((naxis2+1)/2.0 - crpix2[step])
              ycen[step] = crval2[step] + cdelt2 * pc2_2[step]*((naxis2+1)/2.0 - crpix2[step])
              crval3[step] = xcen[step]
              crval2[step] = ycen[step]
              crpix3[step] = step+1.0
              crpix2[step] = (naxis2+1)/2.0
            endif
            
            if N_ELEMENTS(history) eq 0 then history = gt_tagval(hdrNUV, 'HISTORY', missing='') $
            else if N_ELEMENTS(history) eq 1 && strcompress(history[0], /remove_all) eq '' then history = gt_tagval(hdrNUV, 'HISTORY', missing='')
          endif

          pc3_3[step] = pc2_2[step]
          pc2_3[step] = -pc3_2[step]
          
          ;got values for arrays
          
          
          ;calculate step size
          if step gt 0 then begin
            if (DataSrcFUV[step] gt 0 || DataSrcNUV[step] gt 0) && $
              (DataSrcFUV[step-1] gt 0 || DataSrcNUV[step-1] gt 0) then begin
              if N_ELEMENTS(dpztx) eq 0 then dpztx=pztx[step]-pztx[step-1] $
              else dpztx=[dpztx,pztx[step]-pztx[step-1]]
            endif
          endif
          
          
          for region=0,OBSvars.nregFUV-1 do begin ;loop over all FUV regions
            if OBSvars.FUVok_rtype[region] then begin
              if FUVfileind ge 0 then begin
                if sumFUV then begin
                  coords = IRISsim_flipcoords(OBSvars.FUVsc_rtype[region], OBSvars.FUVec_rtype[region], OBSvars.tsr, OBSvars.ter, $
                    sumsptrlFUV[step]/OBSvars.sumspecFUV, sumspatFUV[step]/OBSvars.sumspatmin, $
                    /summing_only, /startatzero)
                  (*data[region])[*,*,step] = congrid(fuv[coords.tsc:coords.tec, coords.tsr:coords.ter], $
                    OBSvars.FUVec_rtype[region]-OBSvars.FUVsc_rtype[region]+1, $
                    OBSvars.ter-OBSvars.tsr+1)
                endif else (*data[region])[*,*,step] = fuv[OBSvars.FUVsc_rtype[region]:OBSvars.FUVec_rtype[region], $
                  OBSvars.tsr:OBSvars.ter]
                ts1=systime(1)
                stats1 = iris_cube_statistics((*data[region])[*,*,step], missing=constants->get_missingvalue(), /reduced, scaled=scaled)
                if (statistics[region+1].vals eq 0) && (stats1.DATAVALS gt 0) then begin
                  statistics[region+1].mean = stats1.datamean
                  statistics[region+1].stdev = stats1.DATARMS
                  statistics[region+1].min = stats1.DATAMIN
                  statistics[region+1].max = stats1.DATAMAX
                  statistics[region+1].vals = stats1.DATAVALS
                endif else begin
                  if stats1.DATAVALS gt 0 then begin
                    statistics[region+1].stdev = irisl12_meanof2stdev(statistics[region+1].stdev, stats1.DATARMS, $
                      statistics[region+1].mean, stats1.datamean, statistics[region+1].vals, stats1.DATAVALS)
                    statistics[region+1].mean = (statistics[region+1].mean*statistics[region+1].vals + stats1.datamean*stats1.DATAVALS) $
                      / (statistics[region+1].vals + stats1.DATAVALS)
                    if stats1.DATAMIN lt statistics[region+1].min then statistics[region+1].min = stats1.DATAMIN
                    if stats1.DATAMAX gt statistics[region+1].max then statistics[region+1].max = stats1.DATAMAX
                    statistics[region+1].vals = statistics[region+1].vals + stats1.DATAVALS
                  endif
                endelse
                ts2=systime(1)
                timemeas.stats = timemeas.stats+(ts2-ts1)
              endif
            endif
          endfor ;region=0,rasters[rastertypenr].FUVnRegions-1
          
          
          for region=0,OBSvars.nregNUV-1 do begin ;loop over all NUV regions
            regionNUV = region + OBSvars.nregFUV
            if OBSvars.NUVok_rtype[region] then begin
              if NUVfileind ge 0 then begin
                if sumNUV then  begin
                  coords = IRISsim_flipcoords(OBSvars.NUVsc_rtype[regionNUV], OBSvars.NUVec_rtype[regionNUV], OBSvars.tsr, OBSvars.ter, $
                    sumsptrlNUV[step]/OBSvars.sumspecmin, sumspatNUV[step]/OBSvars.sumspatmin, $
                    /summing_only, /startatzero)
                  (*data[regionNUV])[*,*,step] = congrid(nuv[coords.tsc:coords.tec, coords.tsr:coords.ter], $
                    OBSvars.NUVec_rtype[region]-OBSvars.NUVsc_rtype[region]+1, $
                    OBSvars.ter-OBSvars.tsr+1)
                endif else (*data[regionNUV])[*,*,step] = nuv[OBSvars.NUVsc_rtype[region]:OBSvars.NUVec_rtype[region], $
                  OBSvars.tsr:OBSvars.ter]
                ts1=systime(1)
                stats1 = iris_cube_statistics((*data[regionNUV])[*,*,step], missing=constants->get_missingvalue(), /reduced, scaled=scaled)
                if (statistics[regionNUV+1].vals eq 0) && (stats1.DATAVALS gt 0) then begin
                  statistics[regionNUV+1].mean = stats1.datamean
                  statistics[regionNUV+1].stdev = stats1.DATARMS
                  statistics[regionNUV+1].min = stats1.DATAMIN
                  statistics[regionNUV+1].max = stats1.DATAMAX
                  statistics[regionNUV+1].vals = stats1.DATAVALS
                endif else begin
                  if stats1.DATAVALS gt 0 then begin
                    statistics[regionNUV+1].stdev = irisl12_meanof2stdev(statistics[regionNUV+1].stdev, stats1.DATARMS, $
                      statistics[regionNUV+1].mean, stats1.datamean, statistics[regionNUV+1].vals, stats1.DATAVALS)
                    statistics[regionNUV+1].mean = (statistics[regionNUV+1].mean*statistics[regionNUV+1].vals + stats1.datamean*stats1.DATAVALS) $
                      / (statistics[regionNUV+1].vals + stats1.DATAVALS)
                    if stats1.DATAMIN lt statistics[regionNUV+1].min then statistics[regionNUV+1].min = stats1.DATAMIN
                    if stats1.DATAMAX gt statistics[regionNUV+1].max then statistics[regionNUV+1].max = stats1.DATAMAX
                    statistics[regionNUV+1].vals = statistics[regionNUV+1].vals + stats1.DATAVALS
                  endif
                endelse
                ts2=systime(1)
                timemeas.stats = timemeas.stats+(ts2-ts1)
              endif
            endif
          endfor ;region=0,rasters[rastertypenr].NUVnRegions-1
        endfor ;step=0,rasters[rastertypenr].nSteps-1
        
        
        ;calculate min,max and mean exposure duration
        ;calculate number of images with active AEC
        ind=where(expdurFUV gt 0, count)
        if count gt 0 then begin
          exptemp=expdurFUV[ind]
          aecact = where(((*(rasters[rastertypenr]).exptimeFUV)[*, rasternr]/1000.0 - exptemp) $
            gt (*(rasters[rastertypenr]).exptimeFUV)[*, rasternr]/1000.0*0.08, countaec)
          OBSvars.AECNRAS = countaec
        endif else OBSvars.AECNRAS = 0
        
        ind=where(expdurNUV gt 0, count)
        if count gt 0 then begin
          if N_ELEMENTS(exptemp) gt 0 then exptemp=[exptemp,expdurNUV[ind]] $
          else exptemp=expdurNUV[ind]
          aecact = where(((*(rasters[rastertypenr]).exptimeNUV)[*, rasternr]/1000.0 - expdurNUV[ind]) $
            gt (*(rasters[rastertypenr]).exptimeNUV)[*, rasternr]/1000.0*0.08, countaec)
          OBSvars.AECNRAS = OBSvars.AECNRAS + countaec
        endif
        if N_ELEMENTS(exptemp) gt 0 then begin
          OBSvars.exptime = mean(exptemp)
          expmin = min(exptemp, max=expmax)
        endif else begin
          expmin=0
          expmax=0
        endelse
        OBSvars.expmin = expmin
        OBSvars.expmax = expmax
        
        
        
        ;calculate basic step time
        for i=1,rasters[rastertypenr].nSteps-1 do begin
          if time[i] ge 0 && time[i-1] ge 0 then begin
            if N_ELEMENTS(dtimes) eq 0 then dtimes=time[i]-time[i-1] $
            else dtimes=[dtimes, time[i]-time[i-1]]
          endif
        endfor
        if N_ELEMENTS(dtimes) gt 0 then begin
          BasicStepTimeAv = mean(dtimes)/1000.0
          BasicStepTimeDv = stddev(dtimes)/1000.0
        endif else begin
          BasicStepTimeAv = 0
          BasicStepTimeDv = 0
        endelse
        
        ;calculate basic step size from real values, if there are any
        if N_ELEMENTS(dpztx) eq 1 then begin
          basicStepSizeAv = dpztx
          basicStepSizeDv = 0
        endif else if N_ELEMENTS(dpztx) gt 1 then begin
          basicStepSizeAv = mean(dpztx)
          basicStepSizeDv = stddev(dpztx)
        endif
        
        ;calculate real FOVX, from real data, only if it is not a sit-and-stare
        if ~rasters[rastertypenr].sitandstare then FOVX = (rasters[rastertypenr].nSteps-1) * basicStepSizeAv

        ; we need to adjust pc3_2 and pc2_3 for different pixelsizes (only for non-sit-and-stare)
        if ~rasters[rastertypenr].sitandstare then begin
          fact = OBSvars.cdelt2FUV / basicStepSizeAv
          pc3_2 = pc3_2 * fact
          pc2_3 = pc2_3 / fact
          OBSvars.PC3_2 = OBSvars.PC3_2 * fact
          OBSvars.PC2_3 = OBSvars.PC2_3 / fact
        endif
        
        ;calculate cadences
        ;for sit and stare (like SJI)
        if rasters[rastertypenr].sitandstare then begin
          ;as executed
          lost=0
          for i=1,rasters[rastertypenr].nSteps-1 do begin
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
          if rasters[rastertypenr].nSteps gt 1 then begin
            timestheo = *(rasters[rastertypenr]).time
            for i=1,rasters[rastertypenr].nSteps-1 do begin
              if i eq 1 then dtimes=timestheo[i]-timestheo[i-1] $
              else dtimes=[dtimes,timestheo[i]-timestheo[i-1]]
            endfor
            cadplav = mean(dtimes)/1000.0
            cadpldv = stddev(dtimes)/1000.0
          endif else begin
            cadplav = 0
            cadpldv = 0
          endelse
        endif else begin;rasters[rastertypenr].sitandstare
          cadexav = mean(cadex)/1000.0
          if N_ELEMENTS(cadex) gt 1 then cadexdv = stddev(cadex)/1000.0 $
          else cadexdv = 0
          cadplav = rasters[rastertypenr].cadpl_av
          cadpldv = rasters[rastertypenr].cadpl_dv
        endelse
        
        
        ;do statistics of all windows combined
        ts1=systime(1)
        if total(statistics[1:*].vals) gt 0 then begin
          maxelements=100000
          seed=0
          for region=1,N_ELEMENTS(statistics)-1 do begin
            if (statistics[0].vals eq 0) && (statistics[region].vals gt 0) then begin
              statistics[0].stdev = statistics[region].stdev
              statistics[0].mean = statistics[region].mean
              statistics[0].vals = statistics[region].vals
            endif else begin
              if statistics[region].vals gt 0 then begin
                statistics[0].stdev = irisl12_meanof2stdev(statistics[0].stdev, statistics[region].stdev, $
                  statistics[0].mean, statistics[region].mean, statistics[0].vals, statistics[region].vals)
                statistics[0].mean = (statistics[0].mean*statistics[0].vals + statistics[region].mean*statistics[region].vals) $
                  / (statistics[0].vals + statistics[region].vals)
                statistics[0].vals = statistics[0].vals + statistics[region].vals
              endif
            endelse
            
            ;do rest of statistics for each window
            if statistics[region].vals gt 0 then begin
              if N_ELEMENTS(*data[region-1]) le maxelements then begin
                stats = iris_cube_statistics(*data[region-1], missing=constants->get_missingvalue(), scaled=scaled)
                if N_ELEMENTS(dtemp) eq 0 then dtemp=(*data[region-1])[*] $
                else dtemp = [dtemp, (*data[region-1])[*]]
              endif else begin
                ind = randomu(seed,maxelements)*N_ELEMENTS(*data[region-1])
                ind = ind[uniq(ind, sort(ind))]
                stats = iris_cube_statistics((*data[region-1])[ind], missing=constants->get_missingvalue(), scaled=scaled)
                if N_ELEMENTS(dtemp) eq 0 then dtemp=(*data[region-1])[ind] $
                else dtemp = [dtemp, (*data[region-1])[ind]]
              endelse
              statistics[region].median = stats.datamedn
              statistics[region].kurtosis = stats.kurtosis
              statistics[region].skewness = stats.dataskew
              statistics[region].datap01 = stats.datap01
              statistics[region].datap10 = stats.datap10
              statistics[region].datap25 = stats.datap25
              statistics[region].datap75 = stats.datap75
              statistics[region].datap90 = stats.datap90
              statistics[region].datap95 = stats.datap95
              statistics[region].datap98 = stats.datap98
              statistics[region].datap99 = stats.datap99
            endif
          endfor
          ind = where(statistics[1:*].vals gt 0, count)
          if count gt 0 then begin
            statistics[0].min = min(statistics[ind+1].min)
            statistics[0].max = max(statistics[ind+1].max)
          endif
          
          ;do rest of statistics for all windows combined
          if statistics[0].vals gt 0 then begin
            stats = iris_cube_statistics(dtemp, missing=constants->get_missingvalue(), scaled=scaled)
            statistics[0].median = stats.datamedn
            statistics[0].kurtosis = stats.kurtosis
            statistics[0].skewness = stats.dataskew
            statistics[0].datap01 = stats.datap01
            statistics[0].datap10 = stats.datap10
            statistics[0].datap25 = stats.datap25
            statistics[0].datap75 = stats.datap75
            statistics[0].datap90 = stats.datap90
            statistics[0].datap95 = stats.datap95
            statistics[0].datap98 = stats.datap98
            statistics[0].datap99 = stats.datap99
          endif
        endif
        ts2=systime(1)
        timemeas.stats = timemeas.stats+(ts2-ts1)
        dtemp=0
        
        
        
        ;get XCEN and YCEN for all exposures
        ind = where(DataSrcFUV gt 0 OR DataSrcNUV gt 0, count)
        if count gt 0 then begin
          mx = (N_ELEMENTS(xcen)-1)/2.0
          ;mx = (max(xcen[ind]) + min(xcen[ind])) / 2.0
          mind = min(abs(ind-mx), ind2)
          ind = ind[ind2]
        endif else ind=0
        xcenall = float(xcen[ind])
        ycenall = float(ycen[ind])
        crpix2all = float(crpix2[ind])
        crval2all = float(crval2[ind])
        crval3all = float(crval3[ind])
        crpix3all = float(crpix3[ind])
        
        
        
        ;scale the data to a signed integer if desired
        cmissingall = ulonarr(N_ELEMENTS(data))
        csaturatedall = ulonarr(N_ELEMENTS(data))
        for i=0,N_ELEMENTS(data)-1 do begin
          missing = where(finite(*data[i]) eq 0, cmissing)
          cmissingall[i] = cmissing
          saturated = where(abs(*data[i]) eq !values.f_infinity, csaturated)
          csaturatedall[i] = csaturated
          if keyword_set(scaled) then begin
            *data[i] = (-199) > *data[i] < (16382-200)
            *data[i] = fix(round((*data[i]+200) * 4 - 32768))
            if cmissing gt 0 then (*data[i])[missing] = -32768
            if csaturated gt 0 then (*data[i])[saturated] = 32764
          endif
        endfor
        if keyword_set(scaled) then begin
          bscales=0.25
          bzeros=7992
        endif else begin
          bscales=1
          bzeros=0
        endelse
        cmissingall = cmissingall - csaturatedall
        
        
        t1=systime(1)
        
        
        file = filename + '_raster_' + fns('t###',rastertypenr) + fns('_r#####.fits',rasternr)
        
        
        ;create main header
        mkhdr, mainheader, 0, /extend
        ;add some information to the primary header
        sxaddpar, mainheader, 'TELESCOP', OBSvars.Telescop
        sxaddpar, mainheader, 'INSTRUME', 'SPEC'
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
        sxaddpar, mainheader, 'CAMERA', 1;;;;;;;;;;;
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
        sxaddpar, mainheader, 'SUMSPTRL', 0
        sxaddpar, mainheader, 'SUMSPTRN', OBSvars.sumspecmin
        sxaddpar, mainheader, 'SUMSPTRF', OBSvars.sumspecminFUV
        sxaddpar, mainheader, 'SUMSPAT', OBSvars.sumspatmin
        sxaddpar, mainheader, 'EXPTIME', OBSvars.exptime
        sxaddpar, mainheader, 'EXPMIN', OBSvars.expmin
        sxaddpar, mainheader, 'EXPMAX', OBSvars.expmax
        sxaddpar, mainheader, 'DATAMEAN', statistics[0].mean
        sxaddpar, mainheader, 'DATARMS', statistics[0].stdev
        sxaddpar, mainheader, 'DATAMEDN', statistics[0].median
        sxaddpar, mainheader, 'DATAMIN', statistics[0].min
        sxaddpar, mainheader, 'DATAMAX', statistics[0].max
        sxaddpar, mainheader, 'DATAVALS', statistics[0].vals
        sxaddpar, mainheader, 'MISSVALS', total(cmissingall,/preserve_type)
        sxaddpar, mainheader, 'NSATPIX', total(csaturatedall,/preserve_type)
        sxaddpar, mainheader, 'NSPIKES', 0
        sxaddpar, mainheader, 'TOTVALS', statistics[0].vals+total(cmissingall,/preserve_type)+total(csaturatedall,/preserve_type)+0
        sxaddpar, mainheader, 'PERCENTD', float(statistics[0].vals) / (statistics[0].vals+total(cmissingall)+total(csaturatedall)+0) *100
        sxaddpar, mainheader, 'DATASKEW', statistics[0].skewness
        sxaddpar, mainheader, 'DATAKURT', statistics[0].kurtosis
        sxaddpar, mainheader, 'DATAP01', statistics[0].datap01
        sxaddpar, mainheader, 'DATAP10', statistics[0].datap10
        sxaddpar, mainheader, 'DATAP25', statistics[0].datap25
        sxaddpar, mainheader, 'DATAP75', statistics[0].datap75
        sxaddpar, mainheader, 'DATAP90', statistics[0].datap90
        sxaddpar, mainheader, 'DATAP95', statistics[0].datap95
        sxaddpar, mainheader, 'DATAP98', statistics[0].datap98
        sxaddpar, mainheader, 'DATAP99', statistics[0].datap99
        
        if rasters[rastertypenr].sitandstare then nexp_prp=rasters[rastertypenr].nSteps $
        else nexp_prp=1
        sxaddpar, mainheader, 'NEXP_PRP', nexp_prp
        sxaddpar, mainheader, 'NEXP', rasters[rastertypenr].nSteps
        sxaddpar, mainheader, 'NEXPOBS', l1to2log.filesexpected
        sxaddpar, mainheader, 'NRASTERP', rasters[rastertypenr].rasterPos
        sxaddpar, mainheader, 'RASTYPDX', rastertypenr+1
        sxaddpar, mainheader, 'RASTYPNX', N_ELEMENTS(rasters)
        sxaddpar, mainheader, 'RASRPT', rasternr+1
        sxaddpar, mainheader, 'RASNRPT', rasters[rastertypenr].nRasters
        sxaddpar, mainheader, 'STEPS_AV', basicStepSizeAv
        sxaddpar, mainheader, 'STEPS_DV', basicStepSizeDv
        sxaddpar, mainheader, 'STEPT_AV', BasicStepTimeAv
        sxaddpar, mainheader, 'STEPT_DV', BasicStepTimeDv
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

        sxaddpar, mainheader, 'NWIN', OBSvars.nregFUV + OBSvars.nregNUV
        
        ;add window-specific keywords with counter to mainheader
        ;FUV
        for region=0,OBSvars.nregFUV-1 do begin ;loop over all FUV regions
          number=string(region+1, format='(I1)')
          sxaddpar, mainheader, 'TDET'+number, OBSvars.FUVdet[region]
          sxaddpar, mainheader, 'TDESC'+number, OBSvars.FUVdesc[region]
          sxaddpar, mainheader, 'TWAVE'+number, OBSvars.FUVwave[region]
          sxaddpar, mainheader, 'TWMIN'+number, OBSvars.FUVwavemin[region]
          sxaddpar, mainheader, 'TWMAX'+number, OBSvars.FUVwavemax[region]
          sxaddpar, mainheader, 'TDMEAN'+number, statistics[region+1].mean
          sxaddpar, mainheader, 'TDRMS'+number, statistics[region+1].stdev
          sxaddpar, mainheader, 'TDMEDN'+number, statistics[region+1].median
          sxaddpar, mainheader, 'TDMIN'+number, statistics[region+1].min
          sxaddpar, mainheader, 'TDMAX'+number, statistics[region+1].max
          sxaddpar, mainheader, 'TDVALS'+number, statistics[region+1].vals
          sxaddpar, mainheader, 'TMISSV'+number, cmissingall[region]
          sxaddpar, mainheader, 'TSATPX'+number, csaturatedall[region]
          sxaddpar, mainheader, 'TSPIKE'+number, 0
          sxaddpar, mainheader, 'TTOTV'+number, statistics[region+1].vals+cmissingall[region]+csaturatedall[region]+0
          sxaddpar, mainheader, 'TPCTD'+number, float(statistics[region+1].vals) / (statistics[region+1].vals+cmissingall[region]+csaturatedall[region]+0) *100
          sxaddpar, mainheader, 'TDSKEW'+number, statistics[region+1].skewness
          sxaddpar, mainheader, 'TDKURT'+number, statistics[region+1].kurtosis
          sxaddpar, mainheader, 'TDP01_'+number, statistics[region+1].datap01
          sxaddpar, mainheader, 'TDP10_'+number, statistics[region+1].datap10
          sxaddpar, mainheader, 'TDP25_'+number, statistics[region+1].datap25
          sxaddpar, mainheader, 'TDP75_'+number, statistics[region+1].datap75
          sxaddpar, mainheader, 'TDP90_'+number, statistics[region+1].datap90
          sxaddpar, mainheader, 'TDP95_'+number, statistics[region+1].datap95
          sxaddpar, mainheader, 'TDP98_'+number, statistics[region+1].datap98
          sxaddpar, mainheader, 'TDP99_'+number, statistics[region+1].datap99
          sxaddpar, mainheader, 'TSR'+number, OBSvars.tsr+1
          sxaddpar, mainheader, 'TER'+number, OBSvars.ter+1
          sxaddpar, mainheader, 'TSC'+number, OBSvars.FUVsc_rtype[region]+1
          sxaddpar, mainheader, 'TEC'+number, OBSvars.FUVec_rtype[region]+1
          sxaddpar, mainheader, 'IPRPFV'+number, IRISl12_mostcommonvalue(IPRPFVER)
          sxaddpar, mainheader, 'IPRPGV'+number, IRISl12_mostcommonvalue(IPRPGVER)
          sxaddpar, mainheader, 'IPRPPV'+number, IRISl12_mostcommonvalue(IPRPPVER)
        endfor ;region=0,rasters[rastertypenr].FUVnRegions-1
        
        ;NUV
        for region=0,OBSvars.nregNUV-1 do begin ;loop over all NUV regions
          regionNUV=region+OBSvars.nregFUV
          number=strcompress(string(regionNUV+1, format='(I2)'), /remove_all)
          sxaddpar, mainheader, 'TDET'+number, OBSvars.NUVdet[region]
          sxaddpar, mainheader, 'TDESC'+number, OBSvars.NUVdesc[region]
          sxaddpar, mainheader, 'TWAVE'+number, OBSvars.NUVwave[region]
          sxaddpar, mainheader, 'TWMIN'+number, OBSvars.NUVwavemin[region]
          sxaddpar, mainheader, 'TWMAX'+number, OBSvars.NUVwavemax[region]
          sxaddpar, mainheader, 'TDMEAN'+number, statistics[regionNUV+1].mean
          sxaddpar, mainheader, 'TDRMS'+number, statistics[regionNUV+1].stdev
          sxaddpar, mainheader, 'TDMEDN'+number, statistics[regionNUV+1].median
          sxaddpar, mainheader, 'TDMIN'+number, statistics[regionNUV+1].min
          sxaddpar, mainheader, 'TDMAX'+number, statistics[regionNUV+1].max
          sxaddpar, mainheader, 'TDVALS'+number, statistics[regionNUV+1].vals
          sxaddpar, mainheader, 'TMISSV'+number, cmissingall[regionNUV]
          sxaddpar, mainheader, 'TSATPX'+number, csaturatedall[regionNUV]
          sxaddpar, mainheader, 'TSPIKE'+number, 0
          sxaddpar, mainheader, 'TTOTV'+number, statistics[regionNUV+1].vals+cmissingall[regionNUV]+csaturatedall[regionNUV]+0
          sxaddpar, mainheader, 'TPCTD'+number, float(statistics[regionNUV+1].vals) / (statistics[regionNUV+1].vals+cmissingall[regionNUV]+csaturatedall[regionNUV]+0) *100
          sxaddpar, mainheader, 'TDSKEW'+number, statistics[regionNUV+1].skewness
          sxaddpar, mainheader, 'TDKURT'+number, statistics[regionNUV+1].kurtosis
          sxaddpar, mainheader, 'TDP01_'+number, statistics[regionNUV+1].datap01
          sxaddpar, mainheader, 'TDP10_'+number, statistics[regionNUV+1].datap10
          sxaddpar, mainheader, 'TDP25_'+number, statistics[regionNUV+1].datap25
          sxaddpar, mainheader, 'TDP75_'+number, statistics[regionNUV+1].datap75
          sxaddpar, mainheader, 'TDP90_'+number, statistics[regionNUV+1].datap90
          sxaddpar, mainheader, 'TDP95_'+number, statistics[regionNUV+1].datap95
          sxaddpar, mainheader, 'TDP98_'+number, statistics[regionNUV+1].datap98
          sxaddpar, mainheader, 'TDP99_'+number, statistics[regionNUV+1].datap99
          sxaddpar, mainheader, 'TSR'+number, OBSvars.tsr+1
          sxaddpar, mainheader, 'TER'+number, OBSvars.ter+1
          sxaddpar, mainheader, 'TSC'+number, OBSvars.NUVsc_rtype[region]+1
          sxaddpar, mainheader, 'TEC'+number, OBSvars.NUVec_rtype[region]+1
          sxaddpar, mainheader, 'IPRPFV'+number, IRISl12_mostcommonvalue(IPRPFVERNUV)
          sxaddpar, mainheader, 'IPRPGV'+number, IRISl12_mostcommonvalue(IPRPGVERNUV)
          sxaddpar, mainheader, 'IPRPPV'+number, IRISl12_mostcommonvalue(IPRPPVERNUV)
        endfor ;region=0,rasters[rastertypenr].NUVnRegions-1
        
        sxaddpar, mainheader, 'KEYWDDOC', OBSvars.KEYWDDOC
        for i=0,N_ELEMENTS(history)-1 do if strcompress(history[i],/remove_all) ne '' then $
          sxaddpar, mainheader, 'HISTORY', history[i]
        sxaddpar, mainheader, 'HISTORY', 'level2  Version '+OBSvars.version
        writefits, file, 0, mainheader
        
        ;;;END of main data block
        
        
        
        ;minpzt = min(*(rasters[rastertypenr]).PZTxy, dimension=2)
        
        
        ;write extension with region data
        ;FUV
        for region=0,OBSvars.nregFUV-1 do begin ;loop over all FUV regions
          ;if OBSvars.FUVsr[region] ge 2072 then cdelt1FUV=OBSvars.cdelt1FUV2 else cdelt1FUV=OBSvars.cdelt1FUV
          mkhdr, header, *data[region], /image
          sxaddpar, header, 'BSCALE', bscales, format="f4.2";, ' True_value = BZERO + BSCALE*Array_value', after='BZERO'
          sxaddpar, header, 'BZERO', bzeros;, ' True_value = BZERO + BSCALE*Array_value', after='BTYPE'
          sxaddpar, header, 'CDELT1', OBSvars.cdelt1FUV[region]     ;image scale in the x-direction
          sxaddpar, header, 'CDELT2', OBSvars.cdelt2FUV     ;image scale in the y-direction
          sxaddpar, header, 'CDELT3', basicStepSizeAv
          sxaddpar, header, 'CRPIX1', OBSvars.crpix1FUV      ;CRPIX1: location of sun/wave center in CCD x direction
          sxaddpar, header, 'CRPIX2', crpix2all      ;CRPIX2: location of sun/wave center in CCD y direction
          sxaddpar, header, 'CRPIX3', crpix3all      ;"1" for FUV/NUV
          sxaddpar, header, 'CRVAL1', OBSvars.CRVAL1FUV[region] ;OBSvars.FUVwavemin[region]     ;SOLARX (SJI), wavelength (FUV&NUV)
          sxaddpar, header, 'CRVAL2', crval2all ;minpzt[1]     ;SOLARY
          sxaddpar, header, 'CRVAL3', crval3all ;minpzt[0]     ;SOLARX (FUV/NUV)
          sxaddpar, header, 'CTYPE1', OBSvars.ctype1FUV     ;HPLN-TAN (SOLARX); WAVE for FUV/NUV
          sxaddpar, header, 'CTYPE2', OBSvars.ctype2FUV     ;HPLT-TAN (SOLARY)
          sxaddpar, header, 'CTYPE3', OBSvars.ctype3FUV     ;HPLN-TAN (SOLARX) for FUV/NUV
          sxaddpar, header, 'CUNIT1', OBSvars.cunit1FUV     ;arcsec for SJI, Angstrom for second FUV CCD
          sxaddpar, header, 'CUNIT2', OBSvars.cunit2FUV
          sxaddpar, header, 'CUNIT3', OBSvars.cunit3FUV
          sxaddpar, header, 'PC1_1', OBSvars.PC1_1
          sxaddpar, header, 'PC1_2', OBSvars.PC1_2
          sxaddpar, header, 'PC2_1', OBSvars.PC2_1
          sxaddpar, header, 'PC2_2', OBSvars.PC2_2
          sxaddpar, header, 'PC3_1', OBSvars.PC3_1
          sxaddpar, header, 'PC3_2', OBSvars.PC3_2
          sxaddpar, header, 'PC3_3', OBSvars.PC3_3
          sxaddpar, header, 'PC2_3', OBSvars.PC2_3
                    
          writefits, file, *data[region], header, /append
        endfor ;region=0,rasters[rastertypenr].FUVnRegions-1
        
        ;NUV
        for region=0,OBSvars.nregNUV-1 do begin ;loop over all NUV regions
          mkhdr, header, *data[region+OBSvars.nregFUV], /image
          sxaddpar, header, 'BSCALE', bscales, format="f4.2";, ' True_value = BZERO + BSCALE*Array_value', after='BZERO'
          sxaddpar, header, 'BZERO', bzeros;, ' True_value = BZERO + BSCALE*Array_value', after='BTYPE'
          sxaddpar, header, 'CDELT1', OBSvars.cdelt1NUV     ;image scale in the x-direction
          sxaddpar, header, 'CDELT2', OBSvars.cdelt2NUV     ;image scale in the y-direction
          sxaddpar, header, 'CDELT3', basicStepSizeAv
          sxaddpar, header, 'CRPIX1', OBSvars.crpix1NUV      ;CRPIX1: location of sun/wave center in CCD x direction
          sxaddpar, header, 'CRPIX2', crpix2all      ;CRPIX2: location of sun/wave center in CCD y direction
          sxaddpar, header, 'CRPIX3', crpix3all     ;"1" for FUV/NUV
          sxaddpar, header, 'CRVAL1', OBSvars.CRVAL1NUV[region] ;OBSvars.NUVwavemin[region]    ;SOLARX (SJI), wavelength (FUV&NUV)
          sxaddpar, header, 'CRVAL2', crval2all ;minpzt[1]     ;SOLARY
          sxaddpar, header, 'CRVAL3', crval3all ;minpzt[0]     ;SOLARX (FUV/NUV)
          sxaddpar, header, 'CTYPE1', OBSvars.ctype1NUV     ;HPLN-TAN (SOLARX); WAVE for FUV/NUV
          sxaddpar, header, 'CTYPE2', OBSvars.ctype2NUV     ;HPLT-TAN (SOLARY)
          sxaddpar, header, 'CTYPE3', OBSvars.ctype3NUV     ;HPLN-TAN (SOLARX) for FUV/NUV
          sxaddpar, header, 'CUNIT1', OBSvars.cunit1NUV     ;arcsec for SJI, Angstrom for second FUV CCD
          sxaddpar, header, 'CUNIT2', OBSvars.cunit2NUV
          sxaddpar, header, 'CUNIT3', OBSvars.cunit3NUV
          sxaddpar, header, 'PC1_1', OBSvars.PC1_1
          sxaddpar, header, 'PC1_2', OBSvars.PC1_2
          sxaddpar, header, 'PC2_1', OBSvars.PC2_1
          sxaddpar, header, 'PC2_2', OBSvars.PC2_2
          sxaddpar, header, 'PC3_1', OBSvars.PC3_1
          sxaddpar, header, 'PC3_2', OBSvars.PC3_2
          sxaddpar, header, 'PC3_3', OBSvars.PC3_3
          sxaddpar, header, 'PC2_3', OBSvars.PC2_3
          
          writefits, file, *data[region+OBSvars.nregFUV], header, /append
        endfor ;region=0,rasters[rastertypenr].NUVnRegions-1
        
        ;END of window extensions
        
        
        
        
        
        ;now let's add another extension with a few vectors
        auxdata = dblarr(47, rasters[rastertypenr].nSteps)
        auxdata[ 0, *] = time/1000d
        auxdata[ 1, *] = pztx
        auxdata[ 2, *] = pzty
        auxdata[ 3, *] = expdurFUV
        auxdata[ 4, *] = expdurNUV
        auxdata[ 5, *] = sumsptrlFUV
        auxdata[ 6, *] = sumspatFUV
        auxdata[ 7, *] = sumsptrlNUV
        auxdata[ 8, *] = sumspatNUV
        auxdata[ 9, *] = DataSrcFUV
        auxdata[10, *] = DataSrcNUV
        auxdata[11, *] = LUTIDfuv
        auxdata[12, *] = LUTIDnuv
        auxdata[13, *] = xcen
        auxdata[14, *] = ycen
        auxdata[15, *] = obs_vr
        auxdata[16, *] = ophase
        auxdata[17, *] = pc1_1
        auxdata[18, *] = pc1_2
        auxdata[19, *] = pc2_1
        auxdata[20, *] = pc2_2
        auxdata[21, *] = pc3_1
        auxdata[22, *] = pc3_2
        auxdata[23, *] = pc3_3
        auxdata[24, *] = pc2_3
        auxdata[25, *] = IT01PMRF
        auxdata[26, *] = IT06TELM
        auxdata[27, *] = IT14SPPX
        auxdata[28, *] = IT15SPPY
        auxdata[29, *] = IT16SPNX
        auxdata[30, *] = IT17SPNY
        auxdata[31, *] = IT18SPPZ
        auxdata[32, *] = IT19SPNZ
        auxdata[33, *] = IPRPOFFX
        auxdata[34, *] = IPRPOFFY
        auxdata[35, *] = IPRPOFFF
        auxdata[36, *] = IT01PMRFNUV
        auxdata[37, *] = IT06TELMNUV
        auxdata[38, *] = IT14SPPXNUV
        auxdata[39, *] = IT15SPPYNUV
        auxdata[40, *] = IT16SPNXNUV
        auxdata[41, *] = IT17SPNYNUV
        auxdata[42, *] = IT18SPPZNUV
        auxdata[43, *] = IT19SPNZNUV
        auxdata[44, *] = IPRPOFFXNUV
        auxdata[45, *] = IPRPOFFYNUV
        auxdata[46, *] = IPRPOFFFNUV
        mkhdr, header, auxdata, /image
        sxaddpar, header, 'TIME', 0, 'time of each exposure in s after start of OBS (rowindex)'
        sxaddpar, header, 'PZTX', 1, 'PZTX of each exposure in arcsec (rowindex)'
        sxaddpar, header, 'PZTY', 2, 'PZTY of each exposure in arcsec (rowindex)'
        sxaddpar, header, 'EXPTIMEF', 3, 'FUV Exposure duration of each exposure in s (rowindex)'
        sxaddpar, header, 'EXPTIMEN', 4, 'NUV Exposure duration of each exposure in s (rowindex)'
        sxaddpar, header, 'SUMSPTRF', 5, 'FUV spectral summing (rowindex)'
        sxaddpar, header, 'SUMSPATF', 6, 'FUV spatial summing (rowindex)'
        sxaddpar, header, 'SUMSPTRN', 7, 'NUV spectral summing (rowindex)'
        sxaddpar, header, 'SUMSPATN', 8, 'NUV spatial summing (rowindex)'
        sxaddpar, header, 'DSRCFIX', 9, 'FUV data source level (rowindex)'
        sxaddpar, header, 'DSRCNIX', 10, 'NUV data source level (rowindex)'
        sxaddpar, header, 'LUTIDF', 11, 'FUV LUT ID (rowindex)'
        sxaddpar, header, 'LUTIDN', 12, 'NUV LUT ID (rowindex)'
        sxaddpar, header, 'XCENIX', 13, 'XCEN (rowindex)'
        sxaddpar, header, 'YCENIX', 14, 'YCEN (rowindex)'
        sxaddpar, header, 'OBS_VRIX', 15, 'Speed of observer in radial direction (rowindex)'
        sxaddpar, header, 'OPHASEIX', 16, 'Orbital phase (rowindex)'
        sxaddpar, header, 'PC1_1IX', 17, 'PC1_1 (rowindex)'
        sxaddpar, header, 'PC1_2IX', 18, 'PC1_2 (rowindex)'
        sxaddpar, header, 'PC2_1IX', 19, 'PC2_1 (rowindex)'
        sxaddpar, header, 'PC2_2IX', 20, 'PC2_2 (rowindex)'
        sxaddpar, header, 'PC3_1IX', 21, 'PC3_1 (rowindex)'
        sxaddpar, header, 'PC3_2IX', 22, 'PC3_2 (rowindex)'
        sxaddpar, header, 'PC3_3IX', 23, 'PC3_3 (rowindex)'
        sxaddpar, header, 'PC2_3IX', 24, 'PC2_3 (rowindex)'
        sxaddpar, header, 'IT01PFUV', 25, 'FUV PM Temperature (for pointing) (rowindex)'
        sxaddpar, header, 'IT06TFUV', 26, 'FUV MidTel Temperature (for pointing) (rowindex)'
        sxaddpar, header, 'IT14SFUV', 27, 'FUV Spectrograph +X Temperature (for wavelength) (rowindex)'
        sxaddpar, header, 'IT15SFUV', 28, 'FUV Spectrograph +Y Temperature (for wavelength) (rowindex)'
        sxaddpar, header, 'IT16SFUV', 29, 'FUV Spectrograph -X Temperature (for wavelength) (rowindex)'
        sxaddpar, header, 'IT17SFUV', 30, 'FUV Spectrograph -Y Temperature (for wavelength) (rowindex)'
        sxaddpar, header, 'IT18SFUV', 31, 'FUV Spectrograph +Z Temperature (for wavelength) (rowindex)'
        sxaddpar, header, 'IT19SFUV', 32, 'FUV Spectrograph -Z Temperature (for wavelength) (rowindex)'
        sxaddpar, header, 'POFFXFUV', 33, 'FUV X (spectral direction) shift in pixels (rowindex)'
        sxaddpar, header, 'POFFYFUV', 34, 'FUV Y (spatial direction) shift in pixels (rowindex)'
        sxaddpar, header, 'POFFFFUV', 35, 'FUV Flag indicating source of X and Y offsets (rowindex)'
        sxaddpar, header, 'IT01PNUV', 36, 'NUV PM Temperature (for pointing) (rowindex)'
        sxaddpar, header, 'IT06TNUV', 37, 'NUV MidTel Temperature (for pointing) (rowindex)'
        sxaddpar, header, 'IT14SNUV', 38, 'NUV Spectrograph +X Temperature (for wavelength) (rowindex)'
        sxaddpar, header, 'IT15SNUV', 39, 'NUV Spectrograph +Y Temperature (for wavelength) (rowindex)'
        sxaddpar, header, 'IT16SNUV', 40, 'NUV Spectrograph -X Temperature (for wavelength) (rowindex)'
        sxaddpar, header, 'IT17SNUV', 41, 'NUV Spectrograph -Y Temperature (for wavelength) (rowindex)'
        sxaddpar, header, 'IT18SNUV', 42, 'NUV Spectrograph +Z Temperature (for wavelength) (rowindex)'
        sxaddpar, header, 'IT19SNUV', 43, 'NUV Spectrograph -Z Temperature (for wavelength) (rowindex)'
        sxaddpar, header, 'POFFXNUV', 44, 'NUV X (spectral direction) shift in pixels (rowindex)'
        sxaddpar, header, 'POFFYNUV', 45, 'NUV Y (spatial direction) shift in pixels (rowindex)'
        sxaddpar, header, 'POFFFNUV', 46, 'NUV Flag indicating source of X and Y offsets (rowindex)'
        writefits, file, auxdata, header, /append
        
        ;END of auxiliary data extensions
        
        
        
        
        
        ;now let's add another extension with a few string vectors
        lFRMid = max(strlen(FRMid))
        lFDBidFUV = max(strlen(FDBidFUV))
        lCRSidFUV = max(strlen(CRSidFUV))
        lFDBidNUV = max(strlen(FDBidNUV))
        lCRSidNUV = max(strlen(CRSidNUV))
        lfilenameFUV = max(strlen(filenameFUV))
        lfilenameNUV = max(strlen(filenameNUV))
        lIPRPDTMP = max(strlen(IPRPDTMP))
        lIPRPDTMPNUV = max(strlen(IPRPDTMPNUV))
        ltot = lFRMid + lFDBidFUV + lCRSidFUV + lFDBidNUV + lCRSidNUV + lfilenameFUV + lfilenameNUV + $
          lIPRPDTMP + lIPRPDTMPNUV
        
        if ltot gt 0 then begin
        
          ftcreate, ltot, rasters[rastertypenr].nSteps, header, auxdata2
          
          for step=0,rasters[rastertypenr].nSteps-1 do begin
            start=0
            if strlen(FRMid[step]) gt 0 then $
              auxdata2[start:start+strlen(FRMid[step])-1, step] = byte(FRMid[step])
            start=start+lFRMid
            if strlen(FDBidFUV[step]) gt 0 then $
              auxdata2[start:start+strlen(FDBidFUV[step])-1, step] = byte(FDBidFUV[step])
            start=start+lFDBidFUV
            if strlen(CRSidFUV[step]) gt 0 then $
              auxdata2[start:start+strlen(CRSidFUV[step])-1, step] = byte(CRSidFUV[step])
            start=start+lCRSidFUV
            if strlen(FDBidNUV[step]) gt 0 then $
              auxdata2[start:start+strlen(FDBidNUV[step])-1, step] = byte(FDBidNUV[step])
            start=start+lFDBidNUV
            if strlen(CRSidNUV[step]) gt 0 then $
              auxdata2[start:start+strlen(CRSidNUV[step])-1, step] = byte(CRSidNUV[step])
            start=start+lCRSidNUV
            if strlen(filenameFUV[step]) gt 0 then $
              auxdata2[start:start+strlen(filenameFUV[step])-1, step] = byte(filenameFUV[step])
            start=start+lfilenameFUV
            if strlen(filenameNUV[step]) gt 0 then $
              auxdata2[start:start+strlen(filenameNUV[step])-1, step] = byte(filenameNUV[step])
            start=start+lfilenameNUV
            if strlen(IPRPDTMP[step]) gt 0 then $
              auxdata2[start:start+strlen(IPRPDTMP[step])-1, step] = byte(IPRPDTMP[step])
            start=start+lIPRPDTMP
            if strlen(IPRPDTMPNUV[step]) gt 0 then $
              auxdata2[start:start+strlen(IPRPDTMPNUV[step])-1, step] = byte(IPRPDTMPNUV[step])
          endfor
          
        endif else ftcreate, 1, rasters[rastertypenr].nSteps, header, auxdata2
        
        sxaddpar, header, 'TFIELDS', 7
        ix=0
        sxaddpar, header, 'TBCOL1', ix+1
        sxaddpar, header, 'TFORM1', 'A'+strcompress(string(lFRMid),/remove_all)
        sxaddpar, header, 'TTYPE1', 'FRMID'
        sxaddpar, header, 'FRMID', ix, 'FRM ID of each exposure (rowindex)'
        sxaddpar, header, 'LFRMID', lFRMid, 'Length of FRM ID (rows)'
        ix=ix+lFRMid
        sxaddpar, header, 'TBCOL2', ix+1
        sxaddpar, header, 'TFORM2', 'A'+strcompress(string(lFDBidFUV),/remove_all)
        sxaddpar, header, 'TTYPE2', 'FUVFDBID'
        sxaddpar, header, 'FDBIDF', ix, 'FUV FDB ID of each exposure (rowindex)'
        sxaddpar, header, 'LFDBIDF', lFDBidFUV, 'Length of FUV FDB ID (rows)'
        ix=ix+lFDBidFUV
        sxaddpar, header, 'TBCOL3', ix+1
        sxaddpar, header, 'TFORM3', 'A'+strcompress(string(lCRSidFUV),/remove_all)
        sxaddpar, header, 'TTYPE3', 'FUVCRSID'
        sxaddpar, header, 'CRSIDF', ix, 'FUV CRS ID of each exposure (rowindex)'
        sxaddpar, header, 'LCRSIDF', lCRSidFUV, 'Length of FUV CRS ID (rows)'
        ix=ix+lCRSidFUV
        sxaddpar, header, 'TBCOL4', ix+1
        sxaddpar, header, 'TFORM4', 'A'+strcompress(string(lFDBidNUV),/remove_all)
        sxaddpar, header, 'TTYPE4', 'NUVFDBID'
        sxaddpar, header, 'FDBIDN', ix, 'NUV FDB ID of each exposure (rowindex)'
        sxaddpar, header, 'LFDBIDN', lFDBidNUV, 'Length of NUV FDB ID (rows)'
        ix=ix+lFDBidNUV
        sxaddpar, header, 'TBCOL5', ix+1
        sxaddpar, header, 'TFORM5', 'A'+strcompress(string(lCRSidNUV),/remove_all)
        sxaddpar, header, 'TTYPE5', 'NUVCRSID'
        sxaddpar, header, 'CRSIDN', ix, 'NUV CRS ID of each exposure (rowindex)'
        sxaddpar, header, 'LCRSIDN', lCRSidNUV, 'Length of NUV CRS ID (rows)'
        ix=ix+lCRSidNUV
        sxaddpar, header, 'TBCOL6', ix+1
        sxaddpar, header, 'TFORM6', 'A'+strcompress(string(lfilenameFUV),/remove_all)
        sxaddpar, header, 'TTYPE6', 'FUVfilename'
        sxaddpar, header, 'FILEF', ix, 'FUV Filename of each exposure (rowindex)'
        sxaddpar, header, 'LFILEF', lfilenameFUV, 'Length of FUV Filename (rows)'
        ix=ix+lfilenameFUV
        sxaddpar, header, 'TBCOL7', ix+1
        sxaddpar, header, 'TFORM7', 'A'+strcompress(string(lfilenameNUV),/remove_all)
        sxaddpar, header, 'TTYPE7', 'NUVfilename'
        sxaddpar, header, 'FILEN', ix, 'NUV Filename of each exposure (rowindex)'
        sxaddpar, header, 'LFILEN', lfilenameNUV, 'Length of NUV Filename (rows)'
        ix=ix+lfilenameNUV
        sxaddpar, header, 'TBCOL8', ix+1
        sxaddpar, header, 'TFORM8', 'A'+strcompress(string(lIPRPDTMP),/remove_all)
        sxaddpar, header, 'TTYPE8', 'FUV Comma-delimited string list of temperatures used by iris_make_dark'
        sxaddpar, header, 'PDTMPFUV', ix, 'FUV string list of temperatures of each exposure (rowindex)'
        sxaddpar, header, 'LPDTMFUV', lIPRPDTMP, 'FUV Length of string list (rows)'
        ix=ix+lIPRPDTMP
        sxaddpar, header, 'TBCOL9', ix+1
        sxaddpar, header, 'TFORM9', 'A'+strcompress(string(lIPRPDTMPNUV),/remove_all)
        sxaddpar, header, 'TTYPE9', 'NUV Comma-delimited string list of temperatures used by iris_make_dark'
        sxaddpar, header, 'PDTMPNUV', ix, 'NUV string list of temperatures of each exposure (rowindex)'
        sxaddpar, header, 'LPDTMNUV', lIPRPDTMPNUV, 'NUV Length of string list (rows)'
        
        writefits, file, auxdata2, header, /append
        
        t2=systime(1)
        timemeas.writefile=timemeas.writefile+(t2-t1)
        
      endfor ;rasternr=0,rasters[rastertypenr].nRasters-1
    endif ;if rasters[rastertypenr].FUVnRegions + rasters[rastertypenr].NUVnRegions gt 0
  endfor ;rastertypenr=0,N_ELEMENTS(rasters)-1
  
  obj_destroy, constants
END
