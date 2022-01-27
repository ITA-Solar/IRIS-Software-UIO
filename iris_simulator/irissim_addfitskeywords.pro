pro IRISsim_AddFitsKeywords, header, OBSList, FRMList, FDBList, CRSList, SpecData, $
    type, currentFRM, currentFDB, currentCRS, $
    nrOBSrepeats, nrFRM, nrFRMrepeats, nrFDB, $
    time, pzt, filterwheel, Date_OBS, T_OBS, $
    sim1, sim2, sim3, sim4
    
  ; $Id: irissim_addfitskeywords.pro,v 1.6 2013/09/05 12:24:19 mawiesma Exp $  ;
    
  ;type: 1=SJI, 2=NUV, 3=FUV
    
  constants = obj_new('IRISsim_constants')
  
  ;adds keywords to fits-files created by syntool.pro
  ;should correspond to IRIS fits-files level 1
  
  fxaddpar, header, 'ACS_ECLP', 0     ;ACS eclipse flag
  fxaddpar, header, 'ACS_MODE', 0     ;ACS pointing mode
  fxaddpar, header, 'ACS_SAFE', 0     ;ACS safe hold flag
  fxaddpar, header, 'ACS_SUNP', 0     ;ACS sun presense flag
  fxaddpar, header, 'ASD_REC', 0      ;Ancillary Science Data series pointer
  ;  fxaddpar, header, 'BITPIX', 0       ;Bits/pixel
  fxaddpar, header, 'BLD_VERS', 0     ;"Build Version from jsoc_version.h"
  
  case type of
    0: cam=3
    1: cam=2
    2: cam=1
  endcase
  fxaddpar, header, 'CAMERA', cam   ;For IRIS: FUV=1, NUV=2, SJI=3
  
  case type of
    0: cdelt1=constants->get_solarxdisp()
    1: cdelt1=constants->get_nuvdisplambda()
    2: cdelt1=constants->get_fuv1displambda()
  endcase
  fxaddpar, header, 'CDELT1', cdelt1     ;image scale in the x-direction
  case type of
    0: cdelt1a=0.0
    1: cdelt1a=0.0
    2: cdelt1a=constants->get_fuv2displambda()
  endcase
  fxaddpar, header, 'CDELT1A', cdelt1a   ;image scale in the x-direction for second FUV CCD
  fxaddpar, header, 'CDELT2', constants->get_solarydisp()     ;image scale in the y-direction
  case type of
    0: cdelt3=0.0
    1: cdelt3=constants->get_slitwidth()
    2: cdelt3=constants->get_slitwidth()
  endcase
  fxaddpar, header, 'CDELT3', cdelt3      ;slit width for FUV,NUV
  
  ;fxaddpar, header, 'CHECKSUM', 0       ;
  fxaddpar, header, 'COMPID', 0      ;Compression ID; n,k
  fxaddpar, header, 'CROPID', 0      ;Crop table ID
  fxaddpar, header, 'CRPIX1', 0      ;CRPIX1: location of sun/wave center in CCD x direction
  fxaddpar, header, 'CRPIX1A', 0      ;CRPIX1: location of sun/wave center in CCD x direction (second FUV CCD)
  fxaddpar, header, 'CRPIX2', 0      ;CRPIX2: location of sun/wave center in CCD y direction
  fxaddpar, header, 'CRPIX2A', 0      ;CRPIX2: location of sun/wave center in CCD y direction (second FUV CCD)
  case type of
    0: crpix3=0
    1: crpix3=1
    2: crpix3=1
  endcase
  fxaddpar, header, 'CRPIX3', crpix3      ;"1" for FUV/NUV
  fxaddpar, header, 'CRPIX3A', crpix3      ;"1" for FUV/NUV, second CCD
  
  fxaddpar, header, 'CRS_DESC', CRSList[currentCRS].Description      ;Window desriptions: comma separated?. From IICRSID
  fxaddpar, header, 'NWIN', CRSList[currentCRS].SubRegions  ;Number of Regions
  for i=0,7 do begin
    number=string(i+1,format='(I1)')
    if i lt CRSList[currentCRS].SubRegions then begin
      case type of
        0: begin
          if (*(CRSList[currentCRS]).StartRow)[i] gt constants->get_PixCCDxSJI() then $
            coords = IRISsim_flipcoords( (*(CRSList[currentCRS]).StartCol)[i], (*(CRSList[currentCRS]).EndCol)[i], $
            (*(CRSList[currentCRS]).StartRow)[i], (*(CRSList[currentCRS]).EndRow)[i], $
            CRSList[currentCRS].Spectral, CRSList[currentCRS].Spatial, /sjifuv ) $
          else $
            coords = IRISsim_flipcoords( (*(CRSList[currentCRS]).StartCol)[i], (*(CRSList[currentCRS]).EndCol)[i], $
            (*(CRSList[currentCRS]).StartRow)[i], (*(CRSList[currentCRS]).EndRow)[i], $
            CRSList[currentCRS].Spectral, CRSList[currentCRS].Spatial, /sjinuv )
        end
        1:  coords = IRISsim_flipcoords( (*(CRSList[currentCRS]).StartCol)[i], (*(CRSList[currentCRS]).EndCol)[i], $
          (*(CRSList[currentCRS]).StartRow)[i], (*(CRSList[currentCRS]).EndRow)[i], $
          CRSList[currentCRS].Spectral, CRSList[currentCRS].Spatial, /nuv )
        2: coords = IRISsim_flipcoords( (*(CRSList[currentCRS]).StartCol)[i], (*(CRSList[currentCRS]).EndCol)[i], $
          (*(CRSList[currentCRS]).StartRow)[i], (*(CRSList[currentCRS]).EndRow)[i], $
          CRSList[currentCRS].Spectral, CRSList[currentCRS].Spatial, /fuv )
      endcase
      fxaddpar, header, 'TSC'+number, coords.tsc     ;Start row for window i
      fxaddpar, header, 'TEC'+number, coords.tec ;End row for window i
      fxaddpar, header, 'TSR'+number, coords.tsr     ;Start column for window i
      fxaddpar, header, 'TER'+number, coords.ter ;End column for window i
      if type gt 0 then begin
        for pda=0,N_ELEMENTS(SpecData)-1 do begin
          if (SpecData[pda].CRSindex eq currentCRS) && (SpecData[pda].SubRegionID eq (*(CRSList[currentCRS]).SubRegionID)[i]) then begin
            fxaddpar, header, 'TDESC'+number, SpecData[pda].name;, 'Name of Region '+number
            break
          endif
        endfor
      endif else fxaddpar, header, 'TDESC'+number, 'SJI_'+constants->get_FWname(filterwheel, /noAngstrom, /frequency)
    endif else begin
      fxaddpar, header, 'TSR'+number, 0     ;Start row for window i
      fxaddpar, header, 'TER'+number, 0 ;End row for window i
      fxaddpar, header, 'TSC'+number, 0     ;Start column for window i
      fxaddpar, header, 'TEC'+number, 0 ;End column for window i
      fxaddpar, header, 'TDESC'+number, ''
    endelse
  endfor
  
  case type of
    0: crstype='sji'
    1: crstype='nuv'
    2: crstype='fu1'
  endcase
  fxaddpar, header, 'CRS_TYPE', crstype   ;CRS Type
  case type of
    0: crval1=pzt[0]
    1: crval1=constants->get_nuvstartlambda()
    2: crval1=constants->get_fuv1startlambda()
  endcase
  fxaddpar, header, 'CRVAL1', crval1     ;SOLARX (SJI), wavelength (FUV&NUV)
  case type of
    0: crval1a=0
    1: crval1a=0
    2: crval1a=constants->get_fuv2startlambda()
  endcase
  fxaddpar, header, 'CRVAL1A', crval1a    ;wavelength for second FUV CCD
  fxaddpar, header, 'CRVAL2', pzt[1]     ;SOLARY
  fxaddpar, header, 'CRVAL2A', 0    ;SOLARY for second FUV CCD
  case type of
    0: crval3=0
    1: crval3=pzt[0]
    2: crval3=pzt[0]
  endcase
  fxaddpar, header, 'CRVAL3', crval3     ;SOLARX (FUV/NUV)
  fxaddpar, header, 'CRVAL3A', crval3    ;SOLARX, second FUV CCD
  
  case type of
    0: ctype1='SolarX'
    1: ctype1='Wave'
    2: ctype1='Wave'
  endcase
  fxaddpar, header, 'CTYPE1', ctype1     ;HPLN-TAN (SOLARX); WAVE for FUV/NUV
  fxaddpar, header, 'CTYPE1A', ctype1    ;HPLN-TAN (SOLARX); WAVE for second FUV CCD
  fxaddpar, header, 'CTYPE2', 'SolarY'     ;HPLT-TAN (SOLARY)
  fxaddpar, header, 'CTYPE2A', 'SolarY'    ;HPLT-TAN (SOLARY) for second FUV CCD
  case type of
    0: ctype3=''
    1: ctype3='SolarX'
    2: ctype3='SolarX'
  endcase
  fxaddpar, header, 'CTYPE3', ctype3     ;HPLN-TAN (SOLARX) for FUV/NUV
  fxaddpar, header, 'CTYPE3A', ctype3    ;HPLN-TAN (SOLARX) for second FUV CCD
  
  case type of
    0: cunit1='arcsec'
    1: cunit1='Angstrom'
    2: cunit1='Angstrom'
  endcase
  fxaddpar, header, 'CUNIT1', cunit1     ;arcsec for SJI, Angstrom for second FUV CCD
  fxaddpar, header, 'CUNIT1A', cunit1    ;arcsec for SJI, Angstrom for second FUV CCD
  fxaddpar, header, 'CUNIT2', 'arcsec'
  fxaddpar, header, 'CUNIT2A', 'arcsec'
  case type of
    0: cunit3=''
    1: cunit3='arcsec'
    2: cunit3='arcsec'
  endcase
  fxaddpar, header, 'CUNIT3', cunit3
  fxaddpar, header, 'CUNIT3A', cunit3
  
  fxaddpar, header, 'TELESCOP', 'IRIS'
  
  fxaddpar, header, 'IIOLNRPT', OBSList.Repeat_Obs;, 'OBS List Repeats'
  fxaddpar, header, 'IIOLRPT', nrOBSrepeats;, 'OBS List Repeat Count'
  fxaddpar, header, 'ISQOLTNX', OBSList.NumEntries;, 'OBS List Number of Entries'
  fxaddpar, header, 'ISQOLTDX', nrFRM;, 'OBS List Entry Count'
  fxaddpar, header, 'IIFLNRPT', (*(OBSList).Repeat_FRM)[nrFRM];, 'FRM List Repeats'
  fxaddpar, header, 'IIFLRPT', nrFRMrepeats;, 'FRM List Repeat Count'
  fxaddpar, header, 'ISQFLTNX', FRMList[currentFRM].NumEntries;, 'FRM List Number of Entries'
  fxaddpar, header, 'ISQFLTDX', nrFDB;, 'FRM List Entry Count'
  
  fxaddpar, header, 'IIOBSLID', OBSList.ID;, 'OBS List ID'
  fxaddpar, header, 'IIFRMLID', FRMList[currentFRM].ID;, 'FRM List ID'
  fxaddpar, header, 'IIFDBID', FDBList[currentFDB].ID;, 'FDB List ID'
  fxaddpar, header, 'IICRSID', CRSList[currentCRS].ID;, 'CRS List ID'
  
  fxaddpar, header, 'DATE_OBS', Date_OBS;, 'Date and time when exposure started'
  fxaddpar, header, 'T_OBS', T_OBS;, 'Date and time of midpoint of exposure'
  
  fxaddpar, header, 'SIMTIME', time;, 'Simulation Time [ms]'
  fxaddpar, header, 'PZTX', pzt[0];, 'PZT X [arcsec]'
  fxaddpar, header, 'PZTY', pzt[1];, 'PZT Y [arcsec]'
  case type of
    0: begin
      typetext='SJI'
      imgpath='SJI_'+constants->get_FWname(filterwheel, /noAngstrom)
    end
    1: begin
      typetext='NUV'
      imgpath='NUV'
    end
    2: begin
      typetext='FUV'
      imgpath='FUV'
    end
  endcase
  fxaddpar, header, 'INSTRUME', typetext
  fxaddpar, header, 'IMG_PATH', imgpath
  
  fxaddpar, header, 'IFWCTGT', filterwheel;, 'Commanded Filterwheel Position'
  fxaddpar, header, 'IFWPOS', filterwheel;, 'Actual Filterwheel Position'
  fxaddpar, header, 'SUMSPTRL', CRSList[currentCRS].Spectral;, 'Spectral Sum'
  fxaddpar, header, 'SUMSPAT', CRSList[currentCRS].Spatial;, 'Spatial Sum'
  fxaddpar, header, 'EXPTIME', FDBList[currentFDB].Exp_Duration;, 'Exposure Duration'
  fxaddpar, header, 'IMG_TYPE', FDBList[currentFDB].Exp_Type;, 'Exposure Type'
  ;fxaddpar, header, 'EXPMAX', FDBList[currentFDB].Exp_Max, 'Max. Exposure'
  ;fxaddpar, header, 'EXPMIN', FDBList[currentFDB].Exp_Min, 'Min. Exposure'
  ;fxaddpar, header, 'COMPRN', FDBList[currentFDB].CompN, 'Compression N'
  ;fxaddpar, header, 'COMPRK', FDBList[currentFDB].CompK, 'Compression K'
  ;fxaddpar, header, 'LUTID', FDBList[currentFDB].LookupTableID, 'Lookup Table ID'
  
  fxaddpar, header, 'LVL_NUM', 1.7
  
  ;if (*(FRMList[currentFRM]).Flush)[nrFDB] eq -1 then flushcurrent=(*(OBSList).Flush)[nrFRM] $
  ;else flushcurrent=(*(FRMList[currentFRM]).Flush)[nrFDB]
  ;fxaddpar, header, 'FLUSH', flushcurrent, 'Flush'
  ;fxaddpar, header, 'ISKIPOBS', (*(OBSList).InhibitSkip)[nrFRM], 'Inhibit Skip from OBS'
  ;fxaddpar, header, 'ISKIPFRM', (*(FRMList[currentFRM]).InhibitSkip)[nrFDB], 'Inhibit Skip from FRM'
  ;fxaddpar, header, 'TAGOBS', (*(OBSList).Tag)[nrFRM], 'Tag from OBS'
  ;fxaddpar, header, 'FOCUS', (*(FRMList[currentFRM]).Focus)[nrFDB], 'Focus from FRM'
  
  ;fxaddpar, header, 'AECNUC', (*(FRMList[currentFRM]).NUV_AEC)[nrFDB], 'AEC for NUV'
  ;fxaddpar, header, 'AECFUV', (*(FRMList[currentFRM]).FUV_AEC)[nrFDB], 'AEC for FUV'
  ;fxaddpar, header, 'AECSJI', (*(FRMList[currentFRM]).SJI_AEC)[nrFDB], 'AEC for SJI'
  
  
  fxaddpar, header, 'SIMFILE1', sim1;, 'Input 1 from FUV Simulation'
  fxaddpar, header, 'SIMFILE2', sim2;, 'Input 2 from FUV Simulation'
  fxaddpar, header, 'SIMFILE3', sim3;, 'Input 1 from NUV Simulation'
  fxaddpar, header, 'SIMFILE4', sim4;, 'Input 2 from NUV Simulation'
  
  obj_destroy, constants
  
end
