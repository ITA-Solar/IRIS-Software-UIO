PRO IRISl12_getkeywords, level, hdr, OBSvars, fuv=fuv, nuv=nuv, sji=sji, $
  hdrmap=hdrmap, OBSid=OBSid, simlog=simlog, level2dir=level2dir, simendtime=simendtime, maxdeviation=maxdeviation, $
  finalstatus=finalstatus
  ; $Id: irisl12_getkeywords.pro,v 1.76 2018/04/26 13:12:57 mawiesma Exp $  ;

  constants = obj_new('IRISsim_constants')

  case level of

    ;starting values of one execution of one OBS
    0: begin

      ;define structure for all OBS-variables
      OBSvars = {version:'', $
        Date_RF2:'', $
        OBSid:'', $
        OBSrep:0L, $
        OBSstart:'', $
        OBSstartfile:'', $
        OBSend:'', $
        OBS_Desc:'', $
        Telescop:'', $
        DATA_LEV:2.0, $
        LVL_NUM:2.0, $
        DATA_SRC:0.0, $
        Origin:'', $
        Bld_vers:'', $
        DSRCF:0.0, $
        DSRCN:0.0, $
        DSRCS:0.0, $
        OBSLABEL:'', $
        OBSTITLE:'', $
        STATUS:'Quicklook', $
        Date_OBS:'', $
        Date_End:'', $
        LUTID:0.0, $
        LUTIDfuv:0, $
        LUTIDnuv:0, $
        LUTIDsji:0, $
        HLZ:0, $
        SAA:'', $
        SAT_ROT:0.0, $
        ACS_ECLP:0L, $
        ACS_MODE:0L, $
        ACS_SAFE:0L, $
        ACS_SUNP:0L, $
        ASD_REC:'', $
        DAT:'', $
        DSUN_OBS:0.0, $
        DSUN_REF:0.0, $
        TR_MODE:'', $
        OBS_VR:0.0, $
        OPHASE:0.0, $
        cdelt1FUV_l1:0.0d, $
        cdelt1FUV2_l1:0.0d, $
        cdelt1FUV:dblarr(8), $
        cdelt2FUV:0.0, $
        cdelt3FUV:0.0, $
        crpix1FUV_l1:0.0, $
        crpix1FUV2_l1:0.0, $
        crpix2FUV_l1:0.0, $
        crpix3FUV_l1:0.0, $
        crpix1FUV:0.0, $
        crpix2FUV:0.0, $
        crpix3FUV:0.0, $
        crval1FUV_l1:0.0d, $
        crval1FUV2_l1:0.0d, $
        crval1FUV:dblarr(8), $
        crval2FUV:0.0, $
        crval3FUV:0.0, $
        ctype1FUV:'', $
        ctype2FUV:'', $
        ctype3FUV:'', $
        cunit1FUV:'', $
        cunit2FUV:'', $
        cunit3FUV:'', $
        cdelt1NUV:0.0d, $
        cdelt2NUV:0.0, $
        cdelt3NUV:0.0, $
        crpix1NUV_l1:0.0, $
        crpix2NUV_l1:0.0, $
        crpix3NUV_l1:0.0, $
        crpix1NUV:0.0, $
        crpix2NUV:0.0, $
        crpix3NUV:0.0, $
        crval1NUV_l1:0.0d, $
        crval1NUV:dblarr(8), $
        crval2NUV:0.0, $
        crval3NUV:0.0, $
        ctype1NUV:'', $
        ctype2NUV:'', $
        ctype3NUV:'', $
        cunit1NUV:'', $
        cunit2NUV:'', $
        cunit3NUV:'', $
        cdelt1SJI:0.0, $
        cdelt2SJI:0.0, $
        cdelt3SJI:0.0, $
        crpix1SJI_l1:0.0, $
        crpix2SJI_l1:0.0, $
        crpix3SJI_l1:0.0, $
        crpix1SJI:0.0, $
        crpix2SJI:0.0, $
        crpix3SJI:0.0, $
        crval1SJI:0.0, $
        crval2SJI:0.0, $
        crval3SJI:0.0, $
        ctype1SJI:'', $
        ctype2SJI:'', $
        ctype3SJI:'', $
        cunit1SJI:'', $
        cunit2SJI:'', $
        cunit3SJI:'', $
        roll:0.0, $
        btype:'Intensity', $
        bunit:'', $
        nregFUV:0, $
        maxdev:30, $ ;maximum deviation of coords from median value (=maximum drift)
        tsr:0, $
        ter:0, $
        FUVsc_rtype:intarr(8), $
        FUVec_rtype:intarr(8), $
        FUVdesc:strarr(8), $
        FUVwave:dblarr(8), $
        FUVwavemin:dblarr(8), $
        FUVwavemax:dblarr(8), $
        FUVdet:strarr(8), $
        FUVok_rtype:intarr(8), $
        FUVok:intarr(8), $
        nregNUV:0, $
        NUVsc_rtype:intarr(8), $
        NUVec_rtype:intarr(8), $
        NUVdesc:strarr(8), $
        NUVwave:dblarr(8), $
        NUVwavemin:dblarr(8), $
        NUVwavemax:dblarr(8), $
        NUVdet:strarr(8), $
        NUVok_rtype:intarr(8), $
        NUVok:intarr(8), $
        nregSJI:0, $
        SJIsc_rtype:intarr(8), $
        SJIec_rtype:intarr(8), $
        SJIdesc:strarr(8), $
        SJIwave:dblarr(8), $
        SJIwavemin:dblarr(8), $
        SJIwavemax:dblarr(8), $
        SJIdet:strarr(8), $
        SJIok_rtype:intarr(8), $
        SJIok:intarr(8), $
        sumspecminFUV:0, $
        sumspecmin:0, $ ;this is now sumspecmin for NUV and SJI
        sumspatmin:0, $
        sumspecFUV:0, $
        sumspatFUV:0, $
        sumspecNUV:0, $
        sumspatNUV:0, $
        sumspecSJI:0, $
        sumspatSJI:0, $
        exptime:0.0, $
        expmin:0.0, $
        expmax:0.0, $
        PZTx:0d, $
        PZTy:0d, $
        PZTxmin:-9999d, $
        PZTymin:0d, $
        PZTxmax:0d, $
        PZTymax:0d, $
        IAECEVFL:'', $
        IAECFLAG:'', $
        IAECFLFL:'', $
        AECNOBS:0L, $
        AECNRAS:0L, $
        PC1_1:0d, $
        PC1_2:0d, $
        PC2_1:0d, $
        PC2_2:0d, $
        PC3_1:0d, $
        PC3_2:0d, $
        PC3_3:0d, $
        PC2_3:0d, $
        keywddoc:'', $
        IPRPVER:0.0, $            ;       Version of iris_prep (OBS)
        IPRPPDBV:0.0, $            ;       Version of iris_mk_pointdb (OBS)
        IPRPDVER:0L, $            ;       Version of iris_make_dark (OBS)
        IPRPBVER:0L, $            ;       Version of background subtraction (OBS)
        IPRPFVER:0L, $            ;       Recnum of flatfield (IMG_PATH)
        IPRPGVER:0L, $            ;       Version of geometry correction (IMG_PATH)
        IPRPPVER:0L, $       ;       Version of bad-pixel mask (not used) (IMG_PATH)
        IT01PMRF:0.0, $            ;       PM Temperature (for pointing) (IMG)
        IT06TELM:0.0, $            ;       MidTel Temperature (for pointing) (IMG)
        IT14SPPX:0.0, $            ;       Spectrograph +X Temperature (for wavelength) (IMG)
        IT15SPPY:0.0, $            ;       Spectrograph +Y Temperature (for wavelength) (IMG)
        IT16SPNX:0.0, $            ;       Spectrograph -X Temperature (for wavelength) (IMG)
        IT17SPNY:0.0, $            ;       Spectrograph -Y Temperature (for wavelength) (IMG)
        IT18SPPZ:0.0, $            ;       Spectrograph +Z Temperature (for wavelength) (IMG)
        IT19SPNZ:0.0, $            ;       Spectrograph -Z Temperature (for wavelength) (IMG)
        IPRPDTMP:'', $            ;       Comma-delimited string list of temperatures used by iris_make_dark (IMG)
        IPRPFVERNUV:0L, $            ;       Recnum of flatfield (IMG_PATH)
        IPRPGVERNUV:0L, $            ;       Version of geometry correction (IMG_PATH)
        IPRPPVERNUV:0L, $       ;       Version of bad-pixel mask (not used) (IMG_PATH)
        IT01PMRFNUV:0.0, $            ;       PM Temperature (for pointing) (IMG)
        IT06TELMNUV:0.0, $            ;       MidTel Temperature (for pointing) (IMG)
        IT14SPPXNUV:0.0, $            ;       Spectrograph +X Temperature (for wavelength) (IMG)
        IT15SPPYNUV:0.0, $            ;       Spectrograph +Y Temperature (for wavelength) (IMG)
        IT16SPNXNUV:0.0, $            ;       Spectrograph -X Temperature (for wavelength) (IMG)
        IT17SPNYNUV:0.0, $            ;       Spectrograph -Y Temperature (for wavelength) (IMG)
        IT18SPPZNUV:0.0, $            ;       Spectrograph +Z Temperature (for wavelength) (IMG)
        IT19SPNZNUV:0.0, $            ;       Spectrograph -Z Temperature (for wavelength) (IMG)
        IPRPDTMPNUV:'', $            ;       Comma-delimited string list of temperatures used by iris_make_dark (IMG)
        IPRPOFFX:0.0, $            ;       X (spectral direction) shift in pixels
        IPRPOFFY:0.0, $            ;       Y (spatial direction) shift in pixels
        IPRPOFFF:0L, $                    ;       Flag indicating source of X and Y offsets
        IPRPOFFXNUV:0.0, $            ;       X (spectral direction) shift in pixels
        IPRPOFFYNUV:0.0, $            ;       Y (spatial direction) shift in pixels
        IPRPOFFFNUV:0L $                   ;       Flag indicating source of X and Y offsets
      }

      ;check if most essential keywords are present
      if required_tags(hdr[0], /ISQOLTID, /IIOLNRPT, /INSTRUME, /IIOLRPT, /ISQOLTDX, /IIFLRPT, /ISQFLTDX) then begin
        ;OBS ID
        if keyword_set(OBSid) then OBSvars.OBSid = OBSid $
        else OBSvars.OBSid = IRISl12_mostcommonvalue(strcompress(hdr.ISQOLTID, /remove_all))
        if OBSvars.OBSid eq '0' then begin
          idtemp = strcompress(hdr.ISQOLTID, /remove_all)
          ind = where(idtemp ne '0', count)
          if count gt 0 then OBSvars.OBSid = IRISl12_mostcommonvalue(idtemp[ind])
        endif

        ;OBS repetitions
        hind = where(hdr.ISQOLTID eq OBSvars.OBSid, count)
        if count gt 0 then $
          OBSvars.OBSrep = IRISl12_mostcommonvalue(hdr[hind].IIOLNRPT)

        if keyword_set(finalstatus) then OBSvars.status = 'Final'

      endif

    end ;case 0





    ;-------------------------------------------------------------
    ;global values of OBS
    1: begin

      ;start time of OBS
      ;check if the output folder contains a valid date, if yes, use that one
      datefileok=0
      if keyword_set(level2dir) then begin
        temp = extract_fids(file_basename(level2dir), fidfound=fidfound)
        if fidfound then begin
          OBSvars.OBSstartfile = file2time(file_basename(level2dir))
          datefileok=1
        endif
      endif

      if required_tags(hdr[0], /DATE_OBS) then begin
        ;go forwards until we find the first valid date, this is the start date
        dateok=0
        i=0
        while ~dateok do begin
          if hdrmap[i] ge 0 then begin
            temp = hdr[hdrmap[i]].DATE_OBS
            if valid_time(temp) then begin
              OBSvars.OBSstart = temp
              dateok=1
            endif else i=i+1
          endif else i=i+1
          if i eq N_ELEMENTS(hdrmap) then dateok=1
        endwhile
        if valid_time(OBSvars.OBSstart) && (i ge 0) && (i lt N_ELEMENTS(hdrmap)) then begin
          trealstart = str2utc(OBSvars.OBSstart)
          trealstart.time = trealstart.time - simlog[i].simtime
          while trealstart.time lt 0 do begin
            trealstart.time = trealstart.time + 86400000L
            trealstart.mjd = trealstart.mjd - 1
          endwhile
          OBSvars.OBSstart = utc2str(trealstart)
        endif

        ;end time of OBS
        ;go backwards until we find the first valid date, this is the end date
        dateok=0
        i=N_ELEMENTS(hdrmap)-1
        while ~dateok do begin
          if hdrmap[i] ge 0 then begin
            temp = hdr[hdrmap[i]].DATE_OBS
            if valid_time(temp) then begin
              OBSvars.OBSend = temp
              dateok=1
            endif else i=i-1
          endif else i=i-1
          if i eq -1 then dateok=1
        endwhile
        if valid_time(OBSvars.OBSend) && (i ge 0) && (i lt N_ELEMENTS(hdrmap)) then begin
          trealend = str2utc(OBSvars.OBSend)
          trealend.time = trealend.time + (simendtime - simlog[i].simtime)
          while trealend.time ge 86400000L do begin
            trealend.time = trealend.time - 86400000L
            trealend.mjd = trealend.mjd + 1
          endwhile
          OBSvars.OBSend = utc2str(trealend)
        endif
      endif

      if ~datefileok then OBSvars.OBSstartfile = OBSvars.OBSstart






      hdrorig = hdr
      hdrgood = where(hdrmap ge 0, count)
      if count gt 0 then begin
        hdr = hdr[hdrmap[hdrgood]]

        if required_tags(hdr[0], /TELESCOP) then $
          OBSvars.Telescop = IRISl12_mostcommonvalue(hdr.TELESCOP) $
        else OBSvars.Telescop = 'IRIS'

        if required_tags(hdr[0], /OBSLABEL) then $
          OBSvars.OBSLABEL = IRISl12_mostcommonvalue(hdr.OBSLABEL)

        if required_tags(hdr[0], /OBSTITLE) then $
          OBSvars.OBSTITLE = IRISl12_mostcommonvalue(hdr.OBSTITLE)

        if required_tags(hdr[0], /ORIGIN) then $
          OBSvars.ORIGIN = IRISl12_mostcommonvalue(hdr.ORIGIN)

        if required_tags(hdr[0], /BLD_VERS) then $
          OBSvars.BLD_VERS = IRISl12_mostcommonvalue(hdr.BLD_VERS)

        if required_tags(hdr[0], /HLZ) then $
          OBSvars.HLZ = IRISl12_mostcommonvalue(hdr.HLZ)

        if required_tags(hdr[0], /SAA) then $
          OBSvars.SAA = IRISl12_mostcommonvalue(hdr.SAA)

        ;        if required_tags(hdr[0], /ACS_ECLP) then $
        ;          OBSvars.ACS_ECLP = IRISl12_mostcommonvalue(hdr.ACS_ECLP)
        ;
        ;        if required_tags(hdr[0], /ACS_MODE) then $
        ;          OBSvars.ACS_MODE = IRISl12_mostcommonvalue(hdr.ACS_MODE)
        ;
        ;        if required_tags(hdr[0], /ACS_SAFE) then $
        ;          OBSvars.ACS_SAFE = IRISl12_mostcommonvalue(hdr.ACS_SAFE)
        ;
        ;        if required_tags(hdr[0], /ACS_SUNP) then $
        ;          OBSvars.ACS_SUNP = IRISl12_mostcommonvalue(hdr.ACS_SUNP)
        ;
        ;        if required_tags(hdr[0], /ASD_REC) then $
        ;          OBSvars.ASD_REC = IRISl12_mostcommonvalue(hdr.ASD_REC)
        ;
        ;        if required_tags(hdr[0], /DATE) then $
        ;          OBSvars.DAT = IRISl12_mostcommonvalue(hdr.DATE)

        if required_tags(hdr[0], /DSUN_OBS) then $
          OBSvars.DSUN_OBS = IRISl12_mostcommonvalue(hdr.DSUN_OBS)

        ;        if required_tags(hdr[0], /DSUN_REF) then $
        ;          OBSvars.DSUN_REF = IRISl12_mostcommonvalue(hdr.DSUN_REF)

        if required_tags(hdr[0], /KEYWDDOC) then $
          OBSvars.KEYWDDOC = IRISl12_mostcommonvalue(hdr.KEYWDDOC)

;these are global values, but iris_prep doesn't populate them with the keyword_only option
;therefore are those also in case 5
;        if required_tags(hdr[0], /IPRPVER) then $            ;       Version of iris_prep
;          OBSvars.IPRPVER = IRISl12_mostcommonvalue(hdr.IPRPVER)
;
;        if required_tags(hdr[0], /IPRPPDBV) then $            ;       Version of iris_mk_pointdb
;          OBSvars.IPRPPDBV = IRISl12_mostcommonvalue(hdr.IPRPPDBV)
;
;        if required_tags(hdr[0], /IPRPDVER) then $            ;       Version of iris_make_dark
;          OBSvars.IPRPDVER = IRISl12_mostcommonvalue(hdr.IPRPDVER)
;
;        if required_tags(hdr[0], /IPRPBVER) then $            ;       Version of background subtraction
;          OBSvars.IPRPBVER = IRISl12_mostcommonvalue(hdr.IPRPBVER, /nonzero)


        fuvind = where(hdr.INSTRUME eq 'FUV', countFUV)
        nuvind = where(hdr.INSTRUME eq 'NUV', countNUV)
        sjiind = where(hdr.INSTRUME eq 'SJI', countSJI)


        ;we need to check whether iris_isp2wcs has been run with the 'level1point5' keyword
        if required_tags(hdr[0], /PC2_1, /PC3_1) then begin
          if countFUV gt 0 then hdrtemp=hdr[fuvind] $
          else if countNUV gt 0 then hdrtemp=hdr[nuvind]
          if countFUV gt 0 || countNUV gt 0 then begin
            pc2_1 = mean(hdrtemp.PC2_1)
            pc3_1 = mean(hdrtemp.PC3_1)
            if abs(pc2_1) gt 1e-9 && abs(pc3_1) gt 1e-9 then begin
              iris_isp2wcs, hdrorig, /level1point5
              iris_isp2wcs, hdr, /level1point5
            endif
          endif
        endif


        ;CDELTx
        if required_tags(hdr[0], /CDELT1) then begin
          if countFUV gt 0 then OBSvars.cdelt1FUV_l1 = IRISl12_mostcommonvalue(hdr[fuvind].CDELT1)
          if countNUV gt 0 then OBSvars.cdelt1NUV = IRISl12_mostcommonvalue(hdr[nuvind].CDELT1)
          if countSJI gt 0 then OBSvars.cdelt1SJI = IRISl12_mostcommonvalue(hdr[sjiind].CDELT1)
        endif
        if required_tags(hdr[0], /CDELT1A) then $
          if countFUV gt 0 then OBSvars.cdelt1FUV2_l1 = IRISl12_mostcommonvalue(hdr[fuvind].CDELT1A)
        if required_tags(hdr[0], /CDELT2) then begin
          if countFUV gt 0 then OBSvars.cdelt2FUV = IRISl12_mostcommonvalue(hdr[fuvind].CDELT2)
          if countNUV gt 0 then OBSvars.cdelt2NUV = IRISl12_mostcommonvalue(hdr[nuvind].CDELT2)
          if countSJI gt 0 then OBSvars.cdelt2SJI = IRISl12_mostcommonvalue(hdr[sjiind].CDELT2)
        endif
        if required_tags(hdr[0], /CDELT3) then begin
          if countFUV gt 0 then OBSvars.cdelt3FUV = IRISl12_mostcommonvalue(hdr[fuvind].CDELT3)
          if countNUV gt 0 then OBSvars.cdelt3NUV = IRISl12_mostcommonvalue(hdr[nuvind].CDELT3)
          if countSJI gt 0 then OBSvars.cdelt3SJI = IRISl12_mostcommonvalue(hdr[sjiind].CDELT3)
        endif

        ;CRPIXx
        if required_tags(hdr[0], /CRPIX1) then begin
          if countFUV gt 0 then OBSvars.crpix1FUV_l1 = IRISl12_mostcommonvalue(hdr[fuvind].CRPIX1)
          if countNUV gt 0 then OBSvars.crpix1NUV_l1 = IRISl12_mostcommonvalue(hdr[nuvind].CRPIX1)
          if countSJI gt 0 then OBSvars.crpix1SJI_l1 = IRISl12_mostcommonvalue(hdr[sjiind].CRPIX1)
        endif
        if required_tags(hdr[0], /CRPIX1A) then $
          if countFUV gt 0 then OBSvars.crpix1FUV2_l1 = IRISl12_mostcommonvalue(hdr[fuvind].CRPIX1A)
        if required_tags(hdr[0], /CRPIX2) then begin
          if countFUV gt 0 then OBSvars.crpix2FUV_l1 = IRISl12_mostcommonvalue(hdr[fuvind].CRPIX2)
          if countNUV gt 0 then OBSvars.crpix2NUV_l1 = IRISl12_mostcommonvalue(hdr[nuvind].CRPIX2)
          if countSJI gt 0 then OBSvars.crpix2SJI_l1 = IRISl12_mostcommonvalue(hdr[sjiind].CRPIX2)
        endif
        if required_tags(hdr[0], /CRPIX3) then begin
          if countFUV gt 0 then OBSvars.crpix3FUV_l1 = IRISl12_mostcommonvalue(hdr[fuvind].CRPIX3)
          if countNUV gt 0 then OBSvars.crpix3NUV_l1 = IRISl12_mostcommonvalue(hdr[nuvind].CRPIX3)
          if countSJI gt 0 then OBSvars.crpix3SJI_l1 = IRISl12_mostcommonvalue(hdr[sjiind].CRPIX3)
        endif

        OBSvars.crpix1FUV = 1
        OBSvars.crpix1NUV = 1
        OBSvars.crpix1SJI = 1
        ;        OBSvars.crpix2FUV = 1
        ;        OBSvars.crpix2NUV = 1
        ;        OBSvars.crpix2SJI = 1
        ;        OBSvars.crpix3FUV = 1
        ;        OBSvars.crpix3NUV = 1
        ;        OBSvars.crpix3SJI = 1

        ;CRVALx
        if required_tags(hdr[0], /CRVAL1) then begin
          if countFUV gt 0 then OBSvars.crval1FUV_l1 = IRISl12_mostcommonvalue(hdr[fuvind].CRVAL1)
          if countNUV gt 0 then OBSvars.crval1NUV_l1 = IRISl12_mostcommonvalue(hdr[nuvind].CRVAL1)
          ;OBSvars.crval1SJI = IRISl12_mostcommonvalue(hdr[sjiind].CRVAL1)
        endif
        if required_tags(hdr[0], /CRVAL1A) then $
          if countFUV gt 0 then OBSvars.crval1FUV2_l1 = IRISl12_mostcommonvalue(hdr[fuvind].CRVAL1A)
        ;        if required_tags(hdr[0], /CRVAL2) then begin
        ;          OBSvars.crval2FUV = IRISl12_mostcommonvalue(hdr[fuvind].CRVAL2)
        ;          OBSvars.crval2NUV = IRISl12_mostcommonvalue(hdr[nuvind].CRVAL2)
        ;          OBSvars.crval2SJI = IRISl12_mostcommonvalue(hdr[sjiind].CRVAL2)
        ;        endif
        ;        if required_tags(hdr[0], /CRVAL3) then begin
        ;          OBSvars.crval3FUV = IRISl12_mostcommonvalue(hdr[fuvind].CRVAL3)
        ;          OBSvars.crval3NUV = IRISl12_mostcommonvalue(hdr[nuvind].CRVAL3)
        ;          OBSvars.crval3SJI = IRISl12_mostcommonvalue(hdr[sjiind].CRVAL3)
        ;        endif


        ;CTYPEx
        if required_tags(hdr[0], /CTYPE1) then begin
          if countFUV gt 0 then OBSvars.ctype1FUV = IRISl12_mostcommonvalue(hdr[fuvind].CTYPE1)
          if countNUV gt 0 then OBSvars.ctype1NUV = IRISl12_mostcommonvalue(hdr[nuvind].CTYPE1)
          if countSJI gt 0 then OBSvars.ctype1SJI = IRISl12_mostcommonvalue(hdr[sjiind].CTYPE1)
        endif
        if required_tags(hdr[0], /CTYPE2) then begin
          if countFUV gt 0 then OBSvars.ctype2FUV = IRISl12_mostcommonvalue(hdr[fuvind].CTYPE2)
          if countNUV gt 0 then OBSvars.ctype2NUV = IRISl12_mostcommonvalue(hdr[nuvind].CTYPE2)
          if countSJI gt 0 then OBSvars.ctype2SJI = IRISl12_mostcommonvalue(hdr[sjiind].CTYPE2)
        endif
        if required_tags(hdr[0], /CTYPE3) then begin
          if countFUV gt 0 then OBSvars.ctype3FUV = IRISl12_mostcommonvalue(hdr[fuvind].CTYPE3)
          if countNUV gt 0 then OBSvars.ctype3NUV = IRISl12_mostcommonvalue(hdr[nuvind].CTYPE3)
          if countSJI gt 0 then OBSvars.ctype3SJI = IRISl12_mostcommonvalue(hdr[sjiind].CTYPE3)
        endif

        ;CUNITx
        if required_tags(hdr[0], /CUNIT1) then begin
          if countFUV gt 0 then OBSvars.cunit1FUV = IRISl12_mostcommonvalue(hdr[fuvind].CUNIT1)
          if countNUV gt 0 then OBSvars.cunit1NUV = IRISl12_mostcommonvalue(hdr[nuvind].CUNIT1)
          if countSJI gt 0 then OBSvars.cunit1SJI = IRISl12_mostcommonvalue(hdr[sjiind].CUNIT1)
        endif
        if required_tags(hdr[0], /CUNIT2) then begin
          if countFUV gt 0 then OBSvars.cunit2FUV = IRISl12_mostcommonvalue(hdr[fuvind].CUNIT2)
          if countNUV gt 0 then OBSvars.cunit2NUV = IRISl12_mostcommonvalue(hdr[nuvind].CUNIT2)
          if countSJI gt 0 then OBSvars.cunit2SJI = IRISl12_mostcommonvalue(hdr[sjiind].CUNIT2)
        endif
        if required_tags(hdr[0], /CUNIT3) then begin
          if countFUV gt 0 then OBSvars.cunit3FUV = IRISl12_mostcommonvalue(hdr[fuvind].CUNIT3)
          if countNUV gt 0 then OBSvars.cunit3NUV = IRISl12_mostcommonvalue(hdr[nuvind].CUNIT3)
          if countSJI gt 0 then OBSvars.cunit3SJI = IRISl12_mostcommonvalue(hdr[sjiind].CUNIT3)
        endif

        if required_tags(hdr[0], /BTYPE) then $
          OBSvars.btype = IRISl12_mostcommonvalue(hdr.BTYPE)
        if required_tags(hdr[0], /BUNIT) then $
          OBSvars.bunit = IRISl12_mostcommonvalue(hdr.BUNIT)

        if countSJI gt 0 then  begin
          pzta = gt_tagval(hdr[sjiind], 'ISQPZTA', missing=0) - $
            gt_tagval(hdr[sjiind], 'IRTPZTA', missing=0) - $
            gt_tagval(hdr[sjiind], 'IWBPZTA', missing=0)
          pztb = gt_tagval(hdr[sjiind], 'ISQPZTB', missing=0) - $
            gt_tagval(hdr[sjiind], 'IRTPZTB', missing=0) - $
            gt_tagval(hdr[sjiind], 'IWBPZTB', missing=0)
          pztc = gt_tagval(hdr[sjiind], 'ISQPZTC', missing=0) - $
            gt_tagval(hdr[sjiind], 'IRTPZTC', missing=0) - $
            gt_tagval(hdr[sjiind], 'IWBPZTC', missing=0)
          goodind=where((pzta gt -5000) AND (pzta lt 5000) AND $
            (pztb gt -5000) AND (pztb lt 5000) AND $
            (pztc gt -5000) AND (pztc lt 5000), count)
          if count gt 0 then begin
            ;pztvxmin = [0,0,0]
            ;pztvymin = [0,0,0]
            db=iris_mk_pointdb()
            for i=0,count-1 do begin
              ;if (pzta[goodind[i]] ne 0) || (pztb[goodind[i]] ne 0) || (pztc[goodind[i]] ne 0) then begin
                ;xy = IRIS_pzt2xy(pzta[goodind[i]], pztb[goodind[i]], pztc[goodind[i]])
                xy = iris_isp2solar(hdr[sjiind[goodind[i]]], db=db, roll_angle=hdr[sjiind[goodind[i]]].sat_rot, /pzt2xy)
                xy = xy * hdr[sjiind[goodind[i]]].cdelt2
                if OBSvars.PZTxmin lt -9998 then begin
                  OBSvars.PZTxmin = xy[0]
                  OBSvars.PZTymin = xy[1]
                  OBSvars.PZTxmax = xy[0]
                  OBSvars.PZTymax = xy[1]
                  ;pztvxmin = [pzta[goodind[i]], pztb[goodind[i]], pztc[goodind[i]]]
                  ;pztvymin = [pzta[goodind[i]], pztb[goodind[i]], pztc[goodind[i]]]
                endif else begin
                  if xy[0] lt OBSvars.PZTxmin then begin
                    OBSvars.PZTxmin=xy[0]
                    ;pztvxmin = [pzta[goodind[i]], pztb[goodind[i]], pztc[goodind[i]]]
                  endif
                  if xy[1] lt OBSvars.PZTymin then begin
                    OBSvars.PZTymin=xy[1]
                    ;pztvymin = [pzta[goodind[i]], pztb[goodind[i]], pztc[goodind[i]]]
                  endif
                  if xy[0] gt OBSvars.PZTxmax then OBSvars.PZTxmax=xy[0]
                  if xy[1] gt OBSvars.PZTymax then OBSvars.PZTymax=xy[1]
                endelse
              ;endif ;if all pzt values are zero then ignore 
              ;obsolete now, invalid values are set to -9999
            endfor
            ;these calculations of crpix, and crval is obsolet, now done in case 4
            ;            minxind = where((pzta eq pztvxmin[0]) AND (pztb eq pztvxmin[1]) AND (pztc eq pztvxmin[2]), countx)
            ;            if countx gt 0 then begin
            ;              crpix1 = IRISl12_mostcommonvalue(hdr[sjiind[minxind]].CRPIX1)
            ;              x = IRISl12_mostcommonvalue(hdr[sjiind[minxind]].TSC1)
            ;              OBSvars.crval1SJI = IRISl12_mostcommonvalue(hdr[sjiind[minxind]].CRVAL1) + (x-crpix1)*OBSvars.cdelt1SJI
            ;            endif
            ;            minyind = where((pzta eq pztvymin[0]) AND (pztb eq pztvymin[1]) AND (pztc eq pztvymin[2]), county)
            ;            if county gt 0 then begin
            ;              crpix2 = IRISl12_mostcommonvalue(hdr[sjiind[minxind]].CRPIX2)
            ;              y = IRISl12_mostcommonvalue(hdr[sjiind[minxind]].TSR1)
            ;              OBSvars.crval2SJI = IRISl12_mostcommonvalue(hdr[sjiind[minyind]].CRVAL2) + (y-crpix2)*OBSvars.cdelt2SJI
            ;            endif
          endif else begin
            OBSvars.PZTxmin = 0
            OBSvars.PZTymin = 0
            OBSvars.PZTxmax = 0
            OBSvars.PZTymax = 0
          endelse
        endif


        if keyword_set(maxdeviation) then OBSvars.maxdev=maxdeviation

        ;get minimum summing in y-direction
        OBSvars.sumspatmin = min(simlog.sumspat)
        sumspatgtminind = where(simlog[hdrgood].sumspat gt OBSvars.sumspatmin, cgtmin)
        ;if cgtmin gt 0 then sumspatgtmin = simlog[hdrgood[sumspatgtminind]].sumspat


        ;get y-range of all windows
        tnames=TAG_NAMES(hdr[0])
        for w=0,7 do begin
          termax=0
          tsrmin=0
          number=string(w+1, format='(I1)')
          tind=where(strcmp(tnames,'TER'+number) eq 1, count)
          if count gt 0 then begin
            ter = hdr.(tind[0])-1
            wind=where(ter gt 0, wcount)
            ;if there is a window ter must be greater than 0
            if wcount gt 0 then begin
              tind=where(strcmp(tnames,'TSR'+number) eq 1, count)
              if count gt 0 then begin
                tsr = hdr.(tind[0])-1

                ;correct coordinates for different summing
                for igtmin=0,cgtmin-1 do begin
                  coords = IRISsim_flipcoords(0,0, tsr[sumspatgtminind[igtmin]], ter[sumspatgtminind[igtmin]], $
                    1, simlog[hdrgood[sumspatgtminind[igtmin]]].sumspat/OBSvars.sumspatmin, $
                    /backwards, /summing_only, /startatzero)
                  tsr[sumspatgtminind[igtmin]] = coords.tsr
                  ter[sumspatgtminind[igtmin]] = coords.ter
                endfor

                ;let's create one array with all spatial variations
                if N_ELEMENTS(terAll) eq 0 then begin
                  terAll = ter[wind]
                  tsrAll = tsr[wind]
                endif else begin
                  terAll = [terAll, ter[wind]]
                  tsrAll = [tsrAll, tsr[wind]]
                endelse
              endif ;there is TSRn
            endif ;there is real TERn
          endif ;there is TERn
        endfor ;loop over all regions

        if N_ELEMENTS(terAll) gt 0 then begin
          if N_ELEMENTS(tsrAll) eq 1 then tsrmed=tsrAll[0] $
          else tsrmed = median(tsrAll)
          tsrmin = min(tsrAll)
          if tsrmin lt tsrmed-OBSvars.maxdev then begin
            wminind = where(tsrAll ge tsrmed-OBSvars.maxdev)
            tsrmin = min(tsrAll[wminind])
          endif
          if N_ELEMENTS(terAll) eq 1 then termed=terAll[0] $
          else termed = median(terAll)
          termax = max(terAll)
          if termax gt termed+OBSvars.maxdev then begin
            wmaxind = where(terAll le termed+OBSvars.maxdev)
            termax = max(terAll[wmaxind])
          endif
        endif else begin
          box_message, 'strange, no values for y-coordinates'
          tsrmin=0
          termax=0
        endelse

        ;check if these are legal values
        if tsrmin lt 0 then begin
          box_message, 'TSR is below zero, setting it to zero'
          tsrmin=0
        endif else if tsrmin ge constants->get_PixCCDy()-1 then begin
          box_message, 'TSR is too big, setting it to zero'
          tsrmin=0
        endif
        if termax eq tsrmin then begin
          box_message, 'TER is the same as TSR'
        endif else if termax gt constants->get_PixCCDy()-1 then begin
          box_message, 'TER is too big, setting it to maximum value'
          termax=constants->get_PixCCDy()-1
        endif

        OBSvars.tsr = tsrmin
        OBSvars.ter = termax




        ;calculate number of images with active AEC
        exptime = gt_tagval(hdr, 'EXPTIME', missing=0.0)
        goodexp = where(exptime gt 0, count)
        if count gt 0 then begin
          aecact = where((simlog[hdrgood[goodexp]].exptime/1000.0 - exptime[goodexp]) $
            gt simlog[hdrgood[goodexp]].exptime/1000.0*0.08, countaec)
          OBSvars.AECNOBS = countaec
        endif

      endif ;no good indices, shouldn't happen
      hdr = hdrorig

    end ;case 1 ;global values of OBS






    ;-------------------------------------------------------------
    ;window coordinates in x-direction for SJI
    2: begin

      if keyword_set(sji) then begin
        sumspecs = gt_tagval(hdr, 'SUMSPTRL', missing=1)
        sumspats = gt_tagval(hdr, 'SUMSPAT', missing=1)
        hdrind = where((sumspecs eq OBSvars.sumspecmin) AND (sumspats eq OBSvars.sumspatmin), count)
        if count eq 0 then begin
          sumspectemp = min(sumspecs)
          sumspattemp = min(sumspats)
          hdrind = where((sumspecs eq sumspectemp) AND (sumspats eq sumspattemp), count)
        endif

        tnames=TAG_NAMES(hdr[0])
        number=string(0+1, format='(I1)')
        tind=where(strcmp(tnames,'TSC'+number) eq 1, count)
        if count gt 0 then begin
          sc = hdr[hdrind].(tind[0])-1
          if N_ELEMENTS(sc) eq 1 then scmed=sc[0] $
          else scmed = median(sc)
          scmin = min(sc)
          if scmin lt scmed-OBSvars.maxdev then begin
            minind=where((scmed-sc) le OBSvars.maxdev, countmin)
            if countmin gt 0 then begin
              scmin = min(sc[minind])
            endif
          endif
          OBSvars.SJIsc_rtype[0] = scmin
        endif

        tind=where(strcmp(tnames,'TEC'+number) eq 1, count)
        if count gt 0 then begin
          ec = hdr[hdrind].(tind[0])-1
          if N_ELEMENTS(ec) eq 1 then ecmed=ec[0] $
          else ecmed = median(ec)
          ecmax = max(ec)
          if ecmax gt ecmed+OBSvars.maxdev then begin
            maxind=where((ec-ecmed) le OBSvars.maxdev, countmax)
            if countmax gt 0 then begin
              ecmax = max(ec[maxind])
            endif
          endif
          OBSvars.SJIec_rtype[0] = ecmax
        endif

        ;check if we have to convert the given coordinates to the lowest summing (in case FUV and NUV have different summing)
        if n_elements(sumspectemp) gt 0 then begin
          coords = IRISsim_flipcoords(OBSvars.SJIsc_rtype[0], OBSvars.SJIec_rtype[0], 0, 0, $
            sumspectemp/OBSvars.sumspecmin, 1, $
            /backwards,/summing_only,/startatzero)
          OBSvars.SJIsc_rtype[0] = coords.tsc
          OBSvars.SJIec_rtype[0] = coords.tec
        endif

        OBSvars.cdelt1SJI = IRISl12_mostcommonvalue(hdr[hdrind].CDELT1)
        OBSvars.cdelt2SJI = IRISl12_mostcommonvalue(hdr[hdrind].CDELT2)
        if N_ELEMENTS(sumspectemp) gt 0 then begin
          OBSvars.cdelt1SJI = OBSvars.cdelt1SJI / sumspectemp * OBSvars.sumspecmin
          OBSvars.cdelt2SJI = OBSvars.cdelt2SJI / sumspattemp * OBSvars.sumspatmin
        endif

      endif;keyword_set(sji)

    end ;case 2 ;window coordinates in x-direction for SJI





    ;-------------------------------------------------------------
    ;window values for one rastertype/SJI CRS
    3: begin

      ;get the hdr indeces with the lowest summing
      nregions = IRISl12_mostcommonvalue(gt_tagval(hdr, 'CRS_NREG', missing=1))
      sumspecs = gt_tagval(hdr, 'SUMSPTRL', missing=1)
      sumspats = gt_tagval(hdr, 'SUMSPAT', missing=1)
      if keyword_set(fuv) then $
        hdrind = where((sumspecs eq OBSvars.sumspecminFUV) AND (sumspats eq OBSvars.sumspatmin), count) $
      else $
        hdrind = where((sumspecs eq OBSvars.sumspecmin) AND (sumspats eq OBSvars.sumspatmin), count)
      if count eq 0 then begin
        sumspectemp = min(sumspecs)
        sumspattemp = min(sumspats)
        hdrind = where((sumspecs eq sumspectemp) AND (sumspats eq sumspattemp), count)
      endif

      if keyword_set(fuv) then OBSvars.nregFUV = nregions
      if keyword_set(nuv) then OBSvars.nregNUV = nregions
      if keyword_set(sji) then OBSvars.nregSJI = nregions

      ;get window coordinates which are used for this rastertype
      tnames=TAG_NAMES(hdr[0])
      for w=0,nregions-1 do begin
        number=string(w+1, format='(I1)')

        ;;extend to include all summing cases (as with tsr, ter)
        ;;;;;;;;;;;;;;;;;;;;;
        tind=where(strcmp(tnames,'TSC'+number) eq 1, count)
        if count gt 0 then begin
          if keyword_set(fuv) then begin
            sc = hdr[hdrind].(tind[0])-1
            if N_ELEMENTS(sc) eq 1 then scmed=sc[0] $
            else scmed = median(sc)
            scmin = min(sc)
            if scmin lt scmed-OBSvars.maxdev then begin
              minind=where((scmed-sc) le OBSvars.maxdev, countmin)
              if countmin gt 0 then begin
                scmin = min(sc[minind])
              endif
            endif
            OBSvars.FUVsc_rtype[w] = scmin
          endif else if keyword_set(nuv) then begin
            sc = hdr[hdrind].(tind[0])-1
            if N_ELEMENTS(sc) eq 1 then scmed=sc[0] $
            else scmed = median(sc)
            scmin = min(sc)
            if scmin lt scmed-OBSvars.maxdev then begin
              minind=where((scmed-sc) le OBSvars.maxdev, countmin)
              if countmin gt 0 then begin
                scmin = min(sc[minind])
              endif
            endif
            OBSvars.NUVsc_rtype[w] = scmin
          endif ;else if keyword_set(sji) then begin
          ;            sc = hdr[hdrind].(tind[0])-1
          ;            if N_ELEMENTS(sc) eq 1 then scmed=sc[0] $
          ;            else scmed = median(sc)
          ;            scmin = min(sc)
          ;            if scmin lt scmed-OBSvars.maxdev then begin
          ;              minind=where((scmed-sc) le OBSvars.maxdev, countmin)
          ;              if countmin gt 0 then begin
          ;                scmin = min(sc[minind])
          ;              endif
          ;            endif
          ;            OBSvars.SJIsc_rtype[w] = scmin
          ;          endif
        endif

        tind=where(strcmp(tnames,'TEC'+number) eq 1, count)
        if count gt 0 then begin
          if keyword_set(fuv) then begin
            ec = hdr[hdrind].(tind[0])-1
            if N_ELEMENTS(ec) eq 1 then ecmed=ec[0] $
            else ecmed = median(ec)
            ecmax = max(ec)
            if ecmax gt ecmed+OBSvars.maxdev then begin
              maxind=where((ec-ecmed) le OBSvars.maxdev, countmax)
              if countmax gt 0 then begin
                ecmax = max(ec[maxind])
              endif
            endif
            OBSvars.FUVec_rtype[w] = ecmax
          endif else if keyword_set(nuv) then begin
            ec = hdr[hdrind].(tind[0])-1
            if N_ELEMENTS(ec) eq 1 then ecmed=ec[0] $
            else ecmed = median(ec)
            ecmax = max(ec)
            if ecmax gt ecmed+OBSvars.maxdev then begin
              maxind=where((ec-ecmed) le OBSvars.maxdev, countmax)
              if countmax gt 0 then begin
                ecmax = max(ec[maxind])
              endif
            endif
            OBSvars.NUVec_rtype[w] = ecmax
          endif ;else if keyword_set(sji) then begin
          ;            ec = hdr[hdrind].(tind[0])-1
          ;            if N_ELEMENTS(ec) eq 1 then ecmed=ec[0] $
          ;            else ecmed = median(ec)
          ;            ecmax = max(ec)
          ;            if ecmax gt ecmed+OBSvars.maxdev then begin
          ;              maxind=where((ec-ecmed) le OBSvars.maxdev, countmax)
          ;              if countmax gt 0 then begin
          ;                ecmax = max(ec[maxind])
          ;              endif
          ;            endif
          ;            OBSvars.SJIec_rtype[w] = ecmax
          ;          endif
        endif

        ;check if we have to convert the given coordinates to the lowest summing (in case FUV and NUV have different summing)
        if n_elements(sumspectemp) gt 0 then begin
          if keyword_set(fuv) then begin
            coords = IRISsim_flipcoords(OBSvars.FUVsc_rtype[w], OBSvars.FUVec_rtype[w], 0, 0, $
              sumspectemp/OBSvars.sumspecminFUV, 1, $
              /backwards,/summing_only,/startatzero)
            ;OBSvars.FUVsr_rtype[w] = coords.tsr
            ;OBSvars.FUVer_rtype[w] = coords.ter
            OBSvars.FUVsc_rtype[w] = coords.tsc
            OBSvars.FUVec_rtype[w] = coords.tec
          endif
          if keyword_set(nuv) then begin
            coords = IRISsim_flipcoords(OBSvars.NUVsc_rtype[w], OBSvars.NUVec_rtype[w], 0, 0, $
              sumspectemp/OBSvars.sumspecmin, 1, $
              /backwards,/summing_only,/startatzero)
            ;OBSvars.NUVsr_rtype[w] = coords.tsr
            ;OBSvars.NUVer_rtype[w] = coords.ter
            OBSvars.NUVsc_rtype[w] = coords.tsc
            OBSvars.NUVec_rtype[w] = coords.tec
          endif
          ;          if keyword_set(sji) then begin
          ;            coords = IRISsim_flipcoords(OBSvars.SJIsc_rtype[w], OBSvars.SJIec_rtype[w], 0, 0, $
          ;              sumspectemp/OBSvars.sumspecmin, 1, $
          ;              /backwards,/summing_only,/startatzero)
          ;            ;OBSvars.SJIsr_rtype[w] = coords.tsr
          ;            ;OBSvars.SJIer_rtype[w] = coords.ter
          ;            OBSvars.SJIsc_rtype[w] = coords.tsc
          ;            OBSvars.SJIec_rtype[w] = coords.tec
          ;          endif
        endif


        if keyword_set(fuv) then begin
          coords = IRISsim_flipcoords(OBSvars.FUVsc_rtype[w], OBSvars.FUVec_rtype[w], OBSvars.tsr, OBSvars.ter, $
            OBSvars.sumspecminFUV, OBSvars.sumspatmin, $
            /backwards,/summing_only,/startatzero)
          sr = coords.tsr
          er = coords.ter
          sc = coords.tsc
          ec = coords.tec
          if (sc ge 0) && (sc lt 2*constants->get_PixCCDx()) && $
            (ec gt 0) && (ec lt 2*constants->get_PixCCDx()) && $
            (ec gt sc) && $
            (sr ge 0) && (sr lt constants->get_PixCCDy()) && $
            (er gt 0) && (er lt constants->get_PixCCDy()) && $
            (er gt sr) then begin
            OBSvars.FUVok_rtype[w] = 1
            if sc ge constants->get_PixCCDx() then begin
              OBSvars.FUVdet[w] = 'FUV2'
              OBSvars.cdelt1FUV[w] = OBSvars.cdelt1FUV2_l1
              crpix1 = OBSvars.crpix1FUV2_l1
              if N_ELEMENTS(sumspectemp) gt 0 then begin
                OBSvars.cdelt1FUV[w] = OBSvars.cdelt1FUV[w] / sumspectemp * OBSvars.sumspecminFUV
                crpix1 = crpix1 * sumspectemp / OBSvars.sumspecminFUV
              endif
              OBSvars.crval1FUV[w] = OBSvars.crval1FUV2_l1 + (OBSvars.FUVsc_rtype[w]+1-crpix1) * OBSvars.cdelt1FUV[w]
            endif else begin
              OBSvars.FUVdet[w] = 'FUV1'
              OBSvars.cdelt1FUV[w] = OBSvars.cdelt1FUV_l1
              crpix1 = OBSvars.crpix1FUV_l1
              if N_ELEMENTS(sumspectemp) gt 0 then begin
                OBSvars.cdelt1FUV[w] = OBSvars.cdelt1FUV[w] / sumspectemp * OBSvars.sumspecminFUV
                crpix1 = crpix1 * sumspectemp / OBSvars.sumspecminFUV
              endif
              OBSvars.crval1FUV[w] = OBSvars.crval1FUV_l1 + (OBSvars.FUVsc_rtype[w]+1-crpix1) * OBSvars.cdelt1FUV[w]
            endelse
            OBSvars.FUVwavemin[w] = OBSvars.crval1FUV[w]
            OBSvars.FUVwavemax[w] = OBSvars.crval1FUV[w] + (OBSvars.FUVec_rtype[w]-OBSvars.FUVsc_rtype[w]) * OBSvars.cdelt1FUV[w]
            OBSvars.FUVdesc[w] = constants->get_WavelineDescription(OBSvars.FUVwavemin[w], OBSvars.FUVwavemax[w], wave=wave)
            OBSvars.FUVwave[w] = wave
          endif
        endif

        if keyword_set(nuv) then begin
          coords = IRISsim_flipcoords(OBSvars.NUVsc_rtype[w], OBSvars.NUVec_rtype[w], OBSvars.tsr, OBSvars.ter, $
            OBSvars.sumspecmin, OBSvars.sumspatmin, $
            /backwards,/summing_only,/startatzero)
          sr = coords.tsr
          er = coords.ter
          sc = coords.tsc
          ec = coords.tec
          if (sc ge 0) && (sc lt 2*constants->get_PixCCDx()) && $
            (ec gt 0) && (ec lt 2*constants->get_PixCCDx()) && $
            (ec gt sc) && $
            (sr ge 0) && (sr lt constants->get_PixCCDy()) && $
            (er gt 0) && (er lt constants->get_PixCCDy()) && $
            (er gt sr) then begin
            OBSvars.NUVok_rtype[w] = 1
            OBSvars.NUVdet[w] = 'NUV'
            crpix1 = OBSvars.crpix1NUV_l1
            if N_ELEMENTS(sumspectemp) gt 0 then begin
              if w eq 0 then $
                OBSvars.cdelt1NUV = OBSvars.cdelt1NUV / sumspectemp * OBSvars.sumspecmin
              crpix1 = crpix1 * sumspectemp / OBSvars.sumspecmin
            endif
            OBSvars.crval1NUV[w] = OBSvars.crval1NUV_l1 + (OBSvars.NUVsc_rtype[w]+1-crpix1) * OBSvars.cdelt1NUV
            OBSvars.NUVwavemin[w] = OBSvars.crval1NUV[w]
            OBSvars.NUVwavemax[w] = OBSvars.crval1NUV[w] + (OBSvars.NUVec_rtype[w]-OBSvars.NUVsc_rtype[w]) * OBSvars.cdelt1NUV
            OBSvars.NUVdesc[w] = constants->get_WavelineDescription(OBSvars.NUVwavemin[w], OBSvars.NUVwavemax[w], wave=wave)
            OBSvars.NUVwave[w] = wave
          endif
        endif

        if keyword_set(sji) then begin
          coords = IRISsim_flipcoords(OBSvars.SJIsc_rtype[w], OBSvars.SJIec_rtype[w], OBSvars.tsr, OBSvars.ter, $
            OBSvars.sumspecmin, OBSvars.sumspatmin, $
            /backwards,/summing_only,/startatzero)
          sr = coords.tsr
          er = coords.ter
          sc = coords.tsc
          ec = coords.tec
          if (sc ge 0) && (sc lt 2*constants->get_PixCCDxSJI()) && $
            (ec gt 0) && (ec lt 2*constants->get_PixCCDxSJI()) && $
            (ec gt sc) && $
            (sr ge 0) && (sr lt constants->get_PixCCDy()) && $
            (er gt 0) && (er lt constants->get_PixCCDy()) && $
            (er gt sr) then begin
            OBSvars.SJIok_rtype[w] = 1
            OBSvars.SJIdet[w] = 'SJI'
            OBSvars.SJIdesc[w] = IRISl12_mostcommonvalue(hdr.IMG_PATH)
            OBSvars.SJIwave[w] = constants->get_SJIwaverange(OBSvars.SJIdesc[w], wavemin=wavemin, wavemax=wavemax)
            OBSvars.SJIwavemin = wavemin
            OBSvars.SJIwavemax = wavemax
          endif
        endif
      endfor


      if keyword_set(fuv) then begin
        OBSvars.cdelt2FUV = IRISl12_mostcommonvalue(hdr[hdrind].CDELT2)
        OBSvars.cdelt3FUV = IRISl12_mostcommonvalue(hdr[hdrind].CDELT3)
        if N_ELEMENTS(sumspattemp) gt 0 then begin
          OBSvars.cdelt2FUV = OBSvars.cdelt2FUV / sumspattemp * OBSvars.sumspatmin
          ;OBSvars.cdelt3FUV = OBSvars.cdelt3FUV / sumspattemp * OBSvars.sumspatmin
        endif
      endif

      if keyword_set(nuv) then begin
        OBSvars.cdelt2NUV = IRISl12_mostcommonvalue(hdr[hdrind].CDELT2)
        OBSvars.cdelt3NUV = IRISl12_mostcommonvalue(hdr[hdrind].CDELT3)
        if N_ELEMENTS(sumspattemp) gt 0 then begin
          OBSvars.cdelt2NUV = OBSvars.cdelt2NUV / sumspattemp * OBSvars.sumspatmin
          ;OBSvars.cdelt3NUV = OBSvars.cdelt3NUV / sumspattemp * OBSvars.sumspatmin
        endif
      endif


      if keyword_set(fuv) && nregions EQ 1 then begin
        ;need to check whether this is a full CCD scan
        ;and if yes, we split the window into FUV1 and FUV2
        if OBSvars.FUVsc_rtype[0] lt constants->get_PixCCDx() && $
          OBSvars.FUVec_rtype[0] ge constants->get_PixCCDx() then begin
          ;region covers FUV1 and FUV2
          OBSvars.nregFUV = 2
          
          ;change FUV1 region limit
          OBSvars.FUVec_rtype[1] = OBSvars.FUVec_rtype[0] ;end column for FUV2 region
          OBSvars.FUVec_rtype[0] = constants->get_PixCCDx()-1
          OBSvars.FUVwavemax[0] = OBSvars.crval1FUV[0] + (OBSvars.FUVec_rtype[0]-OBSvars.FUVsc_rtype[0]) * OBSvars.cdelt1FUV[0]
          OBSvars.FUVdesc[0] = constants->get_WavelineDescription(OBSvars.FUVwavemin[0], OBSvars.FUVwavemax[0], wave=wave)
          OBSvars.FUVwave[0] = wave
          
          ;add FUV2 region
          OBSvars.FUVok_rtype[1] = 1
          OBSvars.FUVdet[1] = 'FUV2'
          OBSvars.cdelt1FUV[1] = OBSvars.cdelt1FUV2_l1
          crpix1 = OBSvars.crpix1FUV2_l1
          if N_ELEMENTS(sumspectemp) gt 0 then begin
            OBSvars.cdelt1FUV[1] = OBSvars.cdelt1FUV[1] / sumspectemp * OBSvars.sumspecminFUV
            crpix1 = crpix1 * sumspectemp / OBSvars.sumspecminFUV
          endif
          OBSvars.FUVsc_rtype[1] = constants->get_PixCCDx()
          OBSvars.crval1FUV[1] = OBSvars.crval1FUV2_l1 + (OBSvars.FUVsc_rtype[1]+1-crpix1) * OBSvars.cdelt1FUV[1]
          OBSvars.FUVwavemin[1] = OBSvars.crval1FUV[1]
          OBSvars.FUVwavemax[1] = OBSvars.crval1FUV[1] + (OBSvars.FUVec_rtype[1]-OBSvars.FUVsc_rtype[1]) * OBSvars.cdelt1FUV[1]
          OBSvars.FUVdesc[1] = constants->get_WavelineDescription(OBSvars.FUVwavemin[1], OBSvars.FUVwavemax[1], wave=wave)
          OBSvars.FUVwave[1] = wave
        endif
      endif


      ;these are global values, but iris_prep doesn't populate them with the keyword_only option
      ;therefore are those also in case 5
;      if keyword_set(nuv) then begin
;        OBSvars.IPRPFVERNUV = 0
;        if required_tags(hdr[0], /IPRPFVER) then $            ;       Recnum of flatfield
;          OBSvars.IPRPFVERNUV = IRISl12_mostcommonvalue(hdr.IPRPFVER)
;
;        OBSvars.IPRPGVERNUV = 0
;        if required_tags(hdr[0], /IPRPGVER) then $            ;       Version of geometry correction
;          OBSvars.IPRPGVERNUV = IRISl12_mostcommonvalue(hdr.IPRPGVER)
;
;        OBSvars.IPRPPVERNUV = 0
;        if required_tags(hdr[0], /IPRPPVER) then $            ;       Version of bad-pixel mask (not used)
;          OBSvars.IPRPPVERNUV = IRISl12_mostcommonvalue(hdr.IPRPPVER)
;      endif else begin
;        OBSvars.IPRPFVER = 0
;        if required_tags(hdr[0], /IPRPFVER) then $            ;       Recnum of flatfield
;          OBSvars.IPRPFVER = IRISl12_mostcommonvalue(hdr.IPRPFVER)
;
;        OBSvars.IPRPGVER = 0
;        if required_tags(hdr[0], /IPRPGVER) then $            ;       Version of geometry correction
;          OBSvars.IPRPGVER = IRISl12_mostcommonvalue(hdr.IPRPGVER)
;
;        OBSvars.IPRPPVER = 0
;        if required_tags(hdr[0], /IPRPPVER) then $            ;       Version of bad-pixel mask (not used)
;          OBSvars.IPRPPVER = IRISl12_mostcommonvalue(hdr.IPRPPVER)
;      endelse


    end ;case 3 ;window values for one rastertype/SJI CRS





    ;-------------------------------------------------------------
    ;values for one repetition of a rastertype
    4: begin

      dateok=0
      i=0
      while ~dateok do begin
        temp = hdr[i].DATE_OBS
        if valid_time(temp) then begin
          if OBSvars.Date_OBS eq '' then OBSvars.Date_OBS = temp $
          else if OBSvars.Date_OBS gt temp then OBSvars.Date_OBS = temp
          dateok=1
        endif else begin
          i=i+1
          if i eq N_ELEMENTS(hdr) then dateok=1
        endelse
      endwhile

      dateok=0
      i=N_ELEMENTS(hdr)-1
      while ~dateok do begin
        temp = hdr[i].DATE_OBS
        if valid_time(temp) then begin
          if OBSvars.Date_End eq '' then OBSvars.Date_End = temp $
          else if OBSvars.Date_End lt temp then OBSvars.Date_End = temp
          dateok=1
        endif else begin
          i=i-1
          if i eq -1 then dateok=1
        endelse
      endwhile


      ;      ;get the crvals from the first file if it is not SJI
      ;      if hdr[0].instrume ne 'SJI' then begin
      ;        OBSvars.crval2FUV = gt_tagval(hdr[0], 'CRVAL2', missing=0.0)
      ;        OBSvars.crpix2FUV = gt_tagval(hdr[0], 'CRPIX2', missing=0.0)
      ;        cdelt2 = gt_tagval(hdr[0], 'CDELT2', missing=0.0)
      ;        y = OBSvars.tsr
      ;        OBSvars.crval2FUV = crval2 + (y-crpix2) * cdelt2
      ;        crval3 = gt_tagval(hdr[0], 'CRVAL3', missing=0.0)
      ;        crpix3 = gt_tagval(hdr[0], 'CRPIX3', missing=0.0)
      ;        cdelt3 = gt_tagval(hdr[0], 'CDELT3', missing=0.0)
      ;        x = 1
      ;        OBSvars.crval3FUV = crval3 + (x-crpix3) * cdelt3
      ;        OBSvars.crval2NUV = OBSvars.crval2FUV
      ;        OBSvars.crval3NUV = OBSvars.crval3FUV
      ;      endif else begin
      ;        crval1 = gt_tagval(hdr[0], 'CRVAL1', missing=0.0)
      ;        crpix1 = gt_tagval(hdr[0], 'CRPIX1', missing=0.0)
      ;        cdelt1 = gt_tagval(hdr[0], 'CDELT1', missing=0.0)
      ;        crval2 = gt_tagval(hdr[0], 'CRVAL2', missing=0.0)
      ;        crpix2 = gt_tagval(hdr[0], 'CRPIX2', missing=0.0)
      ;        cdelt2 = gt_tagval(hdr[0], 'CDELT2', missing=0.0)
      ;      endelse


      ;mean of PC-matrix, valid values only (should be between -1 and +1)
      ;correction: values can also ve outside of this range
      if required_tags(hdr[0], /PC1_1) then begin
        ;ind = where((hdr.PC1_1 gt -1.1) AND (hdr.PC1_1 lt 1.1), count)
        ;if count gt 0 then $
          OBSvars.PC1_1 = mean(hdr.PC1_1)
      endif
      if required_tags(hdr[0], /PC1_2) then begin
        ;ind = where((hdr.PC1_2 gt -1.1) AND (hdr.PC1_2 lt 1.1), count)
        ;if count gt 0 then $
          OBSvars.PC1_2 = mean(hdr.PC1_2)
      endif
      if required_tags(hdr[0], /PC2_1) then begin
        ;ind = where((hdr.PC2_1 gt -1.1) AND (hdr.PC2_1 lt 1.1), count)
        ;if count gt 0 then $
          OBSvars.PC2_1 = mean(hdr.PC2_1)
      endif
      if required_tags(hdr[0], /PC2_2) then begin
        ;ind = where((hdr.PC2_2 gt -1.1) AND (hdr.PC2_2 lt 1.1), count)
        ;if count gt 0 then $
          OBSvars.PC2_2 = mean(hdr.PC2_2)
      endif
      if required_tags(hdr[0], /PC3_1) then begin
        ;ind = where((hdr.PC3_1 gt -1.1) AND (hdr.PC3_1 lt 1.1), count)
        ;if count gt 0 then $
          OBSvars.PC3_1 = mean(hdr.PC3_1)
      endif
      if required_tags(hdr[0], /PC3_2) then begin
        ;ind = where((hdr.PC3_2 gt -1.1) AND (hdr.PC3_2 lt 1.1), count)
        ;if count gt 0 then $
          OBSvars.PC3_2 = mean(hdr.PC3_2)
      endif
      OBSvars.PC3_3 = OBSvars.PC2_2
      OBSvars.PC2_3 = -OBSvars.PC3_2

      ;mean of LUT ID
      if required_tags(hdr[0], /LUTID) then $
        OBSvars.LUTID = mean(hdr.LUTID)

      ;mean level of data source
      if required_tags(hdr[0], /LVL_NUM) then $
        OBSvars.DATA_SRC = mean(hdr.LVL_NUM)

      ;get the roll angle
      if required_tags(hdr[0], /SAT_ROT) then $
        OBSvars.SAT_ROT = mean(hdr.SAT_ROT)


      ;get event-, flare and aec-flag
      temp = gt_tagval(hdr, 'IAECEVFL', missing='')
      ind = where(temp eq 'YES', count)
      if count gt 0 then OBSvars.IAECEVFL='YES' $
      else OBSvars.IAECEVFL='NO'

      temp = gt_tagval(hdr, 'IAECFLAG', missing='')
      ind = where(temp eq 'YES', count)
      if count gt 0 then OBSvars.IAECFLAG='YES' $
      else OBSvars.IAECFLAG='NO'

      temp = gt_tagval(hdr, 'IAECFLFL', missing='')
      ind = where(temp eq 'YES', count)
      if count gt 0 then OBSvars.IAECFLFL='YES' $
      else OBSvars.IAECFLFL='NO'

      temp = gt_tagval(hdr, 'TR_MODE', missing='')
      ind = where(strcompress(temp, /remove_all) ne '', count)
      if count gt 0 then begin
        ind = where(temp eq 'ON', count)
        if count gt 0 then OBSvars.TR_MODE='ON' $
        else OBSvars.TR_MODE='OFF'
      endif

    end ;case 4 ;values for one repetition of a rastertype







    ;-------------------------------------------------------------
    ;values for one exposure
    5: begin

      nregions = gt_tagval(hdr, 'CRS_NREG', missing=1)

      if keyword_set(fuv) then begin
        OBSvars.sumspecFUV = gt_tagval(hdr, 'SUMSPTRL', missing=1)
        OBSvars.sumspatFUV = gt_tagval(hdr, 'SUMSPAT', missing=1)
        OBSvars.DSRCF = gt_tagval(hdr, 'LVL_NUM', missing=1.0)
        OBSvars.LUTIDfuv = gt_tagval(hdr, 'LUTID', missing=1)
      endif
      if keyword_set(nuv) then begin
        OBSvars.sumspecNUV = gt_tagval(hdr, 'SUMSPTRL', missing=1)
        OBSvars.sumspatNUV = gt_tagval(hdr, 'SUMSPAT', missing=1)
        OBSvars.DSRCN = gt_tagval(hdr, 'LVL_NUM', missing=1.0)
        OBSvars.LUTIDnuv = gt_tagval(hdr, 'LUTID', missing=1)
      endif
      if keyword_set(sji) then begin
        OBSvars.sumspecSJI = gt_tagval(hdr, 'SUMSPTRL', missing=1)
        OBSvars.sumspatSJI = gt_tagval(hdr, 'SUMSPAT', missing=1)
        OBSvars.DSRCS = gt_tagval(hdr, 'LVL_NUM', missing=1.0)
        OBSvars.LUTIDsji = gt_tagval(hdr, 'LUTID', missing=1)
      endif

      OBSvars.obs_vr = gt_tagval(hdr, 'OBS_VR', missing=0.0)
      OBSvars.ophase = gt_tagval(hdr, 'OPHASE', missing=0.0)

      ;we need to check the actual coordinates and compare witht the ones we use
      ;if they don't fit we have to write errorlog
      ;
      ;      tnames=TAG_NAMES(hdr[0])
      ;      for w=0,nregions-1 do begin
      ;        number=string(w+1, format='(I1)')
      ;        tind=where(strcmp(tnames,'TSR'+number) eq 1, count);due to flipping, row is x-axis
      ;        if count gt 0 then begin
      ;          if keyword_set(fuv) then OBSvars.FUVsr[w] = IRISl12_mostcommonvalue(hdr.(tind[0]))-1
      ;          if keyword_set(nuv) then OBSvars.NUVsr[w] = IRISl12_mostcommonvalue(hdr.(tind[0]))-1
      ;          if keyword_set(sji) then OBSvars.SJIsr[w] = IRISl12_mostcommonvalue(hdr.(tind[0]))-1
      ;        endif
      ;        tind=where(strcmp(tnames,'TER'+number) eq 1, count)
      ;        if count gt 0 then begin
      ;          if keyword_set(fuv) then OBSvars.FUVer[w] = IRISl12_mostcommonvalue(hdr.(tind[0]))-1
      ;          if keyword_set(nuv) then OBSvars.NUVer[w] = IRISl12_mostcommonvalue(hdr.(tind[0]))-1
      ;          if keyword_set(sji) then OBSvars.SJIer[w] = IRISl12_mostcommonvalue(hdr.(tind[0]))-1
      ;        endif
      ;        tind=where(strcmp(tnames,'TSC'+number) eq 1, count)
      ;        if count gt 0 then begin
      ;          if keyword_set(fuv) then OBSvars.FUVsc[w] = IRISl12_mostcommonvalue(hdr.(tind[0]))-1
      ;          if keyword_set(nuv) then OBSvars.NUVsc[w] = IRISl12_mostcommonvalue(hdr.(tind[0]))-1
      ;          if keyword_set(sji) then OBSvars.SJIsc[w] = IRISl12_mostcommonvalue(hdr.(tind[0]))-1
      ;        endif
      ;        tind=where(strcmp(tnames,'TEC'+number) eq 1, count)
      ;        if count gt 0 then begin
      ;          if keyword_set(fuv) then OBSvars.FUVec[w] = IRISl12_mostcommonvalue(hdr.(tind[0]))-1
      ;          if keyword_set(nuv) then OBSvars.NUVec[w] = IRISl12_mostcommonvalue(hdr.(tind[0]))-1
      ;          if keyword_set(sji) then OBSvars.SJIec[w] = IRISl12_mostcommonvalue(hdr.(tind[0]))-1
      ;        endif
      ;
      ;        if keyword_set(fuv) then begin
      ;          coords = IRISsim_flipcoords(OBSvars.FUVsc[w], OBSvars.FUVec[w], OBSvars.FUVsr[w], OBSvars.FUVer[w], $
      ;            OBSvars.sumspecFUV/OBSvars.sumspecmin, OBSvars.sumspatFUV/OBSvars.sumspatmin, $
      ;            /backwards,/summing_only,/startatzero)
      ;          sr = OBSvars.FUVsr_rtype[w]
      ;          er = OBSvars.FUVer_rtype[w]
      ;          sc = coords.tsc
      ;          ec = coords.tec
      ;          if sc eq OBSvars.FUVsc_rtype[w] && $
      ;            ec eq OBSvars.FUVec_rtype[w] && $
      ;            sr eq OBSvars.FUVsr_rtype[w] && $
      ;            er eq OBSvars.FUVer[w] then begin
      ;            OBSvars.FUVok[w] = 1
      ;          endif else begin
      ;            box_message,['wrong coordinates, FUV', 'replacing with coordinates derived from whole rastertype']
      ;            coords = IRISsim_flipcoords(OBSvars.FUVsc_rtype[w], OBSvars.FUVec_rtype[w], OBSvars.FUVsr_rtype[w], OBSvars.FUVer_rtype[w], $
      ;              OBSvars.sumspecFUV/OBSvars.sumspecmin, OBSvars.sumspatFUV/OBSvars.sumspatmin, $
      ;              /summing_only,/startatzero)
      ;            OBSvars.FUVsr[w] = coords.tsr
      ;            OBSvars.FUVer[w] = coords.ter
      ;            OBSvars.FUVsc[w] = coords.tsc
      ;            OBSvars.FUVec[w] = coords.tec
      ;          endelse
      ;        endif
      ;
      ;        if keyword_set(nuv) then begin
      ;          coords = IRISsim_flipcoords(OBSvars.NUVsc[w], OBSvars.NUVec[w], OBSvars.NUVsr[w], OBSvars.NUVer[w], $
      ;            OBSvars.sumspecNUV/OBSvars.sumspecmin, OBSvars.sumspatNUV/OBSvars.sumspatmin, $
      ;            /backwards,/summing_only,/startatzero)
      ;          sr = coords.tsr
      ;          er = coords.ter
      ;          sc = coords.tsc
      ;          ec = coords.tec
      ;          if sc eq OBSvars.NUVsc_rtype[w] && $
      ;            ec eq OBSvars.NUVec_rtype[w] && $
      ;            sr eq OBSvars.NUVsr_rtype[w] && $
      ;            er eq OBSvars.NUVer[w] then begin
      ;            OBSvars.NUVok[w] = 1
      ;          endif else begin
      ;            box_message,['wrong coordinates, NUV', 'replacing with coordinates derived from whole rastertype']
      ;            coords = IRISsim_flipcoords(OBSvars.NUVsc_rtype[w], OBSvars.NUVec_rtype[w], OBSvars.NUVsr_rtype[w], OBSvars.NUVer_rtype[w], $
      ;              OBSvars.sumspecNUV/OBSvars.sumspecmin, OBSvars.sumspatNUV/OBSvars.sumspatmin, $
      ;              /summing_only,/startatzero)
      ;            OBSvars.NUVsr[w] = coords.tsr
      ;            OBSvars.NUVer[w] = coords.ter
      ;            OBSvars.NUVsc[w] = coords.tsc
      ;            OBSvars.NUVec[w] = coords.tec
      ;          endelse
      ;        endif
      ;
      ;        if keyword_set(sji) then begin
      ;          coords = IRISsim_flipcoords(OBSvars.SJIsc[w], OBSvars.SJIec[w], OBSvars.SJIsr[w], OBSvars.SJIer[w], $
      ;            OBSvars.sumspecSJI/OBSvars.sumspecmin, OBSvars.sumspatSJI/OBSvars.sumspatmin, $
      ;            /backwards,/summing_only,/startatzero)
      ;          sr = coords.tsr
      ;          er = coords.ter
      ;          sc = coords.tsc
      ;          ec = coords.tec
      ;          if sc eq OBSvars.SJIsc_rtype[w] && $
      ;            ec eq OBSvars.SJIec_rtype[w] && $
      ;            sr eq OBSvars.SJIsr_rtype[w] && $
      ;            er eq OBSvars.SJIer[w] then begin
      ;            OBSvars.SJIok[w] = 1
      ;          endif else begin
      ;            box_message,['wrong coordinates, SJI', 'replacing with coordinates derived from whole rastertype']
      ;            coords = IRISsim_flipcoords(OBSvars.SJIsc_rtype[w], OBSvars.SJIec_rtype[w], OBSvars.SJIsr_rtype[w], OBSvars.SJIer_rtype[w], $
      ;              OBSvars.sumspecSJI/OBSvars.sumspecmin, OBSvars.sumspatSJI/OBSvars.sumspatmin, $
      ;              /summing_only,/startatzero)
      ;            OBSvars.SJIsr[w] = coords.tsr
      ;            OBSvars.SJIer[w] = coords.ter
      ;            OBSvars.SJIsc[w] = coords.tsc
      ;            OBSvars.SJIec[w] = coords.tec
      ;          endelse
      ;        endif
      ;      endfor ;regions

      pzta = gt_tagval(hdr, 'ISQPZTA', missing=0) - $
        gt_tagval(hdr, 'IRTPZTA', missing=0) - $
        gt_tagval(hdr, 'IWBPZTA', missing=0)
      pztb = gt_tagval(hdr, 'ISQPZTB', missing=0) - $
        gt_tagval(hdr, 'IRTPZTB', missing=0) - $
        gt_tagval(hdr, 'IWBPZTB', missing=0)
      pztc = gt_tagval(hdr, 'ISQPZTC', missing=0) - $
        gt_tagval(hdr, 'IRTPZTC', missing=0) - $
        gt_tagval(hdr, 'IWBPZTC', missing=0)
      if (pzta gt -5000) && (pzta lt 5000) && $
        (pztb gt -5000) && (pztb lt 5000) && $
        (pztc gt -5000) && (pztc lt 5000) then begin
        if (pzta ne 0) || (pztb ne 0) || (pztc ne 0) then begin
          ;xy = IRIS_pzt2xy(pzta, pztb, pztc)
          xy = iris_isp2solar(hdr, db=iris_mk_pointdb(), roll_angle=hdr.sat_rot, /pzt2xy)
          xy = xy * hdr.cdelt2
          OBSvars.PZTx = xy[0]
          OBSvars.PZTy = xy[1]
        endif else begin
          ;if the values are not ok, we use the minimum PZT values
          OBSvars.PZTx = OBSvars.PZTxmin
          OBSvars.PZTy = OBsvars.PZTymin
        endelse
      endif else begin
        ;if the values are not ok, we use the minimum PZT values
        OBSvars.PZTx = OBSvars.PZTxmin
        OBSvars.PZTy = OBsvars.PZTymin
      endelse


      ;version numbers and temperatures
      
      ;global values (need to get them from the real run of iris_prep (no keywords_only option)
      if required_tags(hdr, /IPRPVER) then $            ;       Version of iris_prep
        OBSvars.IPRPVER = hdr.IPRPVER

      if required_tags(hdr, /IPRPPDBV) then $            ;       Version of iris_mk_pointdb
        OBSvars.IPRPPDBV = hdr.IPRPPDBV

      if required_tags(hdr, /IPRPDVER) then $            ;       Version of iris_make_dark
        OBSvars.IPRPDVER = hdr.IPRPDVER

      ;only populated in FUV
      if required_tags(hdr, /IPRPBVER) then begin            ;       Version of background subtraction
        if hdr.IPRPBVER gt 0 then $
          OBSvars.IPRPBVER = hdr.IPRPBVER
      endif


      ;IMG_PATH specific values
      if keyword_set(nuv) then begin
        OBSvars.IPRPFVERNUV = 0
        if required_tags(hdr[0], /IPRPFVER) then $            ;       Recnum of flatfield
          OBSvars.IPRPFVERNUV = IRISl12_mostcommonvalue(hdr.IPRPFVER)

        OBSvars.IPRPGVERNUV = 0
        if required_tags(hdr[0], /IPRPGVER) then $            ;       Version of geometry correction
          OBSvars.IPRPGVERNUV = IRISl12_mostcommonvalue(hdr.IPRPGVER)

        OBSvars.IPRPPVERNUV = 0
        if required_tags(hdr[0], /IPRPPVER) then $            ;       Version of bad-pixel mask (not used)
          OBSvars.IPRPPVERNUV = IRISl12_mostcommonvalue(hdr.IPRPPVER)
      endif else begin
        OBSvars.IPRPFVER = 0
        if required_tags(hdr[0], /IPRPFVER) then $            ;       Recnum of flatfield
          OBSvars.IPRPFVER = IRISl12_mostcommonvalue(hdr.IPRPFVER)

        OBSvars.IPRPGVER = 0
        if required_tags(hdr[0], /IPRPGVER) then $            ;       Version of geometry correction
          OBSvars.IPRPGVER = IRISl12_mostcommonvalue(hdr.IPRPGVER)

        OBSvars.IPRPPVER = 0
        if required_tags(hdr[0], /IPRPPVER) then $            ;       Version of bad-pixel mask (not used)
          OBSvars.IPRPPVER = IRISl12_mostcommonvalue(hdr.IPRPPVER)
      endelse



      if keyword_set(nuv) then begin

        OBSvars.IT01PMRFNUV = 0.0
        if required_tags(hdr, /IT01PMRF) then $            ;       PM Temperature (for pointing)
          OBSvars.IT01PMRFNUV = hdr.IT01PMRF

        OBSvars.IT06TELMNUV = 0.0
        if required_tags(hdr, /IT06TELM) then $            ;       MidTel Temperature (for pointing)
          OBSvars.IT06TELMNUV = hdr.IT06TELM

        OBSvars.IT14SPPXNUV = 0.0
        if required_tags(hdr, /IT14SPPX) then $            ;       Spectrograph +X Temperature (for wavelength)
          OBSvars.IT14SPPXNUV = hdr.IT14SPPX

        OBSvars.IT15SPPYNUV = 0.0
        if required_tags(hdr, /IT15SPPY) then $            ;       Spectrograph +Y Temperature (for wavelength)
          OBSvars.IT15SPPYNUV = hdr.IT15SPPY

        OBSvars.IT16SPNXNUV = 0.0
        if required_tags(hdr, /IT16SPNX) then $            ;       Spectrograph -X Temperature (for wavelength)
          OBSvars.IT16SPNXNUV = hdr.IT16SPNX

        OBSvars.IT17SPNYNUV = 0.0
        if required_tags(hdr, /IT17SPNY) then $            ;       Spectrograph -Y Temperature (for wavelength)
          OBSvars.IT17SPNYNUV = hdr.IT17SPNY

        OBSvars.IT18SPPZNUV = 0.0
        if required_tags(hdr, /IT18SPPZ) then $            ;       Spectrograph +Z Temperature (for wavelength)
          OBSvars.IT18SPPZNUV = hdr.IT18SPPZ

        OBSvars.IT19SPNZNUV = 0.0
        if required_tags(hdr, /IT19SPNZ) then $            ;       Spectrograph -Z Temperature (for wavelength)
          OBSvars.IT19SPNZNUV = hdr.IT19SPNZ

        OBSvars.IPRPDTMPNUV = ''
        if required_tags(hdr, /IPRPDTMP) then $            ;       Comma-delimited string list of temperatures used by iris_make_dark
          OBSvars.IPRPDTMPNUV = hdr.IPRPDTMP

        OBSvars.IPRPOFFXNUV = 0.0
        if required_tags(hdr, /IPRPOFFX) then $            ;       X (spectral direction) shift in pixels
          OBSvars.IPRPOFFXNUV = hdr.IPRPOFFX

        OBSvars.IPRPOFFYNUV = 0.0
        if required_tags(hdr, /IPRPOFFY) then $            ;       Y (spatial direction) shift in pixels
          OBSvars.IPRPOFFYNUV = hdr.IPRPOFFY

        OBSvars.IPRPOFFFNUV = 0
        if required_tags(hdr, /IPRPOFFF) then $            ;       Flag indicating source of X and Y offsets
          OBSvars.IPRPOFFFNUV = hdr.IPRPOFFF

      endif else begin

        OBSvars.IT01PMRF = 0.0
        if required_tags(hdr, /IT01PMRF) then $            ;       PM Temperature (for pointing)
          OBSvars.IT01PMRF = hdr.IT01PMRF

        OBSvars.IT06TELM = 0.0
        if required_tags(hdr, /IT06TELM) then $            ;       MidTel Temperature (for pointing)
          OBSvars.IT06TELM = hdr.IT06TELM

        OBSvars.IT14SPPX = 0.0
        if required_tags(hdr, /IT14SPPX) then $            ;       Spectrograph +X Temperature (for wavelength)
          OBSvars.IT14SPPX = hdr.IT14SPPX

        OBSvars.IT15SPPY = 0.0
        if required_tags(hdr, /IT15SPPY) then $            ;       Spectrograph +Y Temperature (for wavelength)
          OBSvars.IT15SPPY = hdr.IT15SPPY

        OBSvars.IT16SPNX = 0.0
        if required_tags(hdr, /IT16SPNX) then $            ;       Spectrograph -X Temperature (for wavelength)
          OBSvars.IT16SPNX = hdr.IT16SPNX

        OBSvars.IT17SPNY = 0.0
        if required_tags(hdr, /IT17SPNY) then $            ;       Spectrograph -Y Temperature (for wavelength)
          OBSvars.IT17SPNY = hdr.IT17SPNY

        OBSvars.IT18SPPZ = 0.0
        if required_tags(hdr, /IT18SPPZ) then $            ;       Spectrograph +Z Temperature (for wavelength)
          OBSvars.IT18SPPZ = hdr.IT18SPPZ

        OBSvars.IT19SPNZ = 0.0
        if required_tags(hdr, /IT19SPNZ) then $            ;       Spectrograph -Z Temperature (for wavelength)
          OBSvars.IT19SPNZ = hdr.IT19SPNZ

        OBSvars.IPRPDTMP = ''
        if required_tags(hdr, /IPRPDTMP) then $            ;       Comma-delimited string list of temperatures used by iris_make_dark
          OBSvars.IPRPDTMP = hdr.IPRPDTMP

        OBSvars.IPRPOFFX = 0.0
        if required_tags(hdr, /IPRPOFFX) then $            ;       X (spectral direction) shift in pixels
          OBSvars.IPRPOFFX = hdr.IPRPOFFX

        OBSvars.IPRPOFFY = 0.0
        if required_tags(hdr, /IPRPOFFY) then $            ;       Y (spatial direction) shift in pixels
          OBSvars.IPRPOFFY = hdr.IPRPOFFY

        OBSvars.IPRPOFFF = 0
        if required_tags(hdr, /IPRPOFFF) then $            ;       Flag indicating source of X and Y offsets
          OBSvars.IPRPOFFF = hdr.IPRPOFFF

      endelse

    end ;case 5 ;values for one exposure



    else: print, 'wrong case number: ', level, '  irisl12_getkeywords'

  endcase

  obj_destroy, constants

END
