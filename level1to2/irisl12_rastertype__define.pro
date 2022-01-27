FUNCTION IRISl12_rastertype::init, craster

  if N_ELEMENTS(craster) gt 0 then begin
    self.OBSid = craster.OBSid
    self.OBStime = craster.OBStime
    self.ExpType = craster.ExpType
    self.nRegions = craster.nRegions
    self.startrow = craster.startrow
    self.endrow = craster.endrow
    self.startcol = craster.startcol
    self.endcol = craster.endcol
    self.nSteps = craster.nSteps
    self.PosX = craster.PosX
    self.sum = ptr_new(intarr(1,2,self.nSteps))
    (*(self).sum)[0,*,*] = *(craster).sum
    self.SimTime = ptr_new(lonarr(1,self.nSteps))
    (*(self).SimTime)[0,*] = *(craster).SimTime
    self.ExpDuration = ptr_new(lonarr(1,self.nSteps))
    (*(self).ExpDuration)[0,*] = *(craster).ExpDuration
    self.counters = ptr_new(lonarr(1,4,self.nSteps))
    (*(self).counters)[0,*,*] = *(craster).counters
    self.rastertime = ptr_new(dblarr(1))
    (*(self).rastertime)[0] = craster.rastertime
    self.nRasters = 1
  endif
  
  return, 1
END



PRO IRISl12_rastertype::cleanup
END



PRO IRISl12_rastertype::addStep, craster, member

  if self.OBSid eq craster.OBSid && $
    self.OBStime eq craster.OBStime && $
    self.ExpType eq craster.ExpType && $
    self.nRegions eq craster.nRegions && $
    self.nSteps eq craster.nSteps && $
    TOTAL( (self.startrow - craster.startrow) LE 0.3) EQ N_ELEMENTS(self.startrow) && $
    TOTAL( (self.endrow - craster.endrow) LE 0.3) EQ N_ELEMENTS(self.endrow) && $
    TOTAL( (self.startcol - craster.startcol) LE 0.3) EQ N_ELEMENTS(self.startcol) && $
    TOTAL( (self.endcol - craster.endcol) LE 0.3) EQ N_ELEMENTS(self.endcol) && $
    TOTAL( (*(self).PosX - *(craster).PosX) LE 0.03) EQ N_ELEMENTS(*(self).PosX) then begin
    
    ;if the new raster is identical with this rastertype, then add it
    *(self).rastertime = [*(self).rastertime, craster.rastertime]
    *(self).sum = [*(self).sum, (*(self).sum)[0,*,*]]
    (*(self).sum)[self.nRasters,*,*] = *(craster).sum
    *(self).SimTime = [*(self).SimTime, (*(self).SimTime)[0,*]]
    (*(self).SimTime)[self.nRasters,*] = *(craster).SimTime
    *(self).ExpDuration = [*(self).ExpDuration, (*(self).ExpDuration)[0,*]]
    (*(self).ExpDuration)[self.nRasters,*] = *(craster).ExpDuration
    *(self).counters = [*(self).counters, (*(self).counters)[0,*,*]]
    (*(self).counters)[self.nRasters,*,*] = *(craster).counters
    
    self.nRasters = self.nRasters + 1
    member=1
  endif else member=0
END



PRO IRISl12_rastertype::saveRaster, files, destination, rasternumber

  mreadfits, files, hdr
  
  for rast=0,self.nRasters-1 do begin
    data = ptrarr(self.nRegions) ;data array for all regions, but they have different sizes
    for region=0,self.nRegions-1 do $
      data[region] = ptr_new(dblarr(self.endrow[region]-self.startrow[region]+1, self.endcol[region]-self.startcol[region]+1, self.nSteps))
      
    for step=0,self.nSteps-1 do begin
    
    ;for traf=0,3 do print,(*(self).counters)[rast,traf,step]
    
      ;find the file with the right counters
      curfin=where(hdr.IIOLRPT eq (*(self).counters)[rast,0,step] AND $
        hdr.ISQOLTDX eq (*(self).counters)[rast,1,step] AND $
        hdr.IIFLRPT eq (*(self).counters)[rast,2,step] AND $
        hdr.ISQFLTDX eq (*(self).counters)[rast,3,step] AND $
        hdr.INSTRUME eq 'FUV', count)
      if count ne 1 then print, 'hm...', count else begin
        print, files[curfin]
        datacur = readfits(files[curfin], header, /silent)
        
        if step eq 0 then begin
          regdesc = fxpar(header, 'REGDESC*')
        endif
        
        
        srf = self.startrow[0:self.nRegions-1] / (*(self).sum)[rast,0,step]
        erg = self.endrow[0:self.nRegions-1] / (*(self).sum)[rast,0,step]
        scf = self.startcol[0:self.nRegions-1] / (*(self).sum)[rast,1,step]
        ecf = self.endcol[0:self.nRegions-1] / (*(self).sum)[rast,1,step]
        
        for region=0,self.nRegions-1 do begin
          if TOTAL((*(self).sum)[rast,*,step]) gt 2 then $
            (*data[region])[*,*,step] = congrid(datacur[srf[region]:erg[region], scf[region]:ecf[region]], $
            self.endrow[region]-self.startrow[region]+1, $
            self.endcol[region]-self.startcol[region]+1) $
          else (*data[region])[*,*,step] = datacur[srf[region]:erg[region], scf[region]:ecf[region]]
        endfor
      endelse
      
      
    endfor;step
    
    ;we have a new raster, now we save it into a fits file
    ;if keyword_set(nuv) then file2=dest+fns('IRIS_NUV_raster_t000_r####.fits',raster) $
    ;else file2=dest+fns('IRIS_FUV_raster_t000_r####.fits',raster)
    for region=0,self.nRegions-1 do begin
      if region eq 0 then begin
        ;first region is written into primary block
        mkhdr, header, *data[region], /extend
        ;add some information to the primary header
        ;sxaddpar, header, 'OBSID', regdata[region].OBSid, 'OBS List ID'
        sxaddpar, header, 'REGTOT', self.nRegions, 'Number of Regions'
        sxaddpar, header, 'COMMENT', 'Matrix: [wavelength, y, t/x]'
        sxaddpar, header, 'COMMENT', 'There is an additional extension with auxiliary data'
      endif else $
        mkhdr, header, *data[region], /image
        
      ;add information to all headers
      ;sxaddpar, header, 'FDBID', regdata[region].FDBid, 'FDB List ID'
      ;sxaddpar, header, 'CRSID', regdata[region].CRSid, 'CRS List ID'
      sxaddpar, header, 'ROWSTAR', self.startrow[region], 'Startrow'
      sxaddpar, header, 'ROWNUM', self.endrow[region]-self.startrow[region]+1, 'Number of Rows'
      sxaddpar, header, 'COLSTAR', self.startcol[region], 'Startcolumn'
      sxaddpar, header, 'COLNUM', self.endcol[region]-self.startcol[region]+1, 'Number of Columns'
      sxaddpar, header, 'DESC', regdesc[region], 'The name of the spectral line in this region'
      sxaddpar, header, 'CCDTYPE', 'FUV', 'Type of CCD (FUV/NUV)'
      
      file2 = fns(destination+'IRIS_raster_t###', rasternumber) + fns('_r####.fits', rast)
      if region eq 0 then $
        writefits, file2, *data[region], header $
      else $
        writefits, file2, *data[region], header, /append
        
    endfor ;regions
    
    ;write last extension with auxiliary data (time, PZTX, PZTY, Exposure duration)
    auxdata = dblarr(self.nSteps, 4)
    auxdata[*,0] = (*(self).SimTime)[rast,*]
    auxdata[*,1] = (*(self).PosX)[0,*]
    auxdata[*,2] = (*(self).PosX)[1,*]
    auxdata[*,3] = (*(self).ExpDuration)[rast,*]
    mkhdr, header, auxdata, /image
    sxaddpar, header, 'COMMENT', 'Auxiliary data'
    sxaddpar, header, 'COMMENT', '[*,0]=time [ms], [*,1]=PZTX [arcsec], [*,2]=PZTY [acrsce], [*,3]=Exposure [ms]'
    writefits, file2, auxdata, header, /append
    
  endfor;raster
  
END



PRO IRISl12_rastertype__define

  void = {IRISl12_rastertype, $
    OBSid:'', $
    nRasters:0, $
    OBStime:0d, $
    ExpType:0, $
    nRegions:0, $
    startrow:intarr(8), $
    endrow:intarr(8), $
    startcol:intarr(8), $
    endcol:intarr(8), $
    nSteps:0, $
    PosX:ptr_new(), $        ;[axis, step]
    sum:ptr_new(), $         ;[raster, axis, step]
    SimTime:ptr_new(), $ ;[raster, step]
    ExpDuration:ptr_new(), $ ;[raster, step]
    rastertime:ptr_new(), $  ;[raster]
    counters:ptr_new()}      ;[raster, counter, step]
    
END
