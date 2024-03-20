FUNCTION IRISsim_constants::init

  ; $Id: irissim_constants__define.pro,v 1.16 2017/03/02 13:47:30 mawiesma Exp $  ;

  ;more graph variables
  self.pcharsize=0.8       ;size of the text
  self.pcharthick=1        ;thickness of the text
  self.color_active=[255, 0, 0]      ;color of text, when window is active [r, g, b]
  self.color_inactive=[5, 180, 150]  ;color of text, when window is inactive [r, g, b]
  
  ;sizes of CCDs
  self.PixCCDx=2072 ;number of pixels on Spec CCD in x-direction
  self.PixCCDy=1096 ;number of pixels on Spec CCD in y-direction
  self.PixCCDxSJI=self.PixCCDx/2 ;number of pixels on SJI CCD in x-direction
  self.PZTnoChangeValue=9999     ; if all PZT variables in OBS have this value, the position is not changed
  self.FWnoChangeValue=9999  ; if filterwheel doesn't change, FW has this value
  
  self.FWinitial=31 ;initial value of filterwheel
  
  self.solarxdisp=0.16666
  self.solarydisp=0.16666
  self.slitwidth=0.33333
  
  self.fuv1displambda=0.01298d
  self.fuv1startlambda=1331.56d
  self.fuv2displambda=0.01272d
  self.fuv2startlambda=1380.45d
  self.nuvdisplambda=0.02546d
  self.nuvstartlambda=2782.56d
  
  self.lambda = [1333.79,1334.53,1335.20,1335.71,1338.61,1340.39,1345.95,1349.43,1351.66, $
    1354.29,1355.60,1355.84,1357.13,1357.66,1358.19,1358.51,1392.15,1392.59, $
    1392.82,1393.33,1393.78,1396.11,1397.22,1398.06,1399.03,1399.77,1399.97, $
    1401.16,1401.51,1402.77,1404.79,1404.82,1405.61,1406.04,2796.20,2803.40]
  self.intensities = [16.2555,2186.11,34.0248,2612.03,0.00000,12.6233,12.6756,14.9511,234.371, $
    41.2635,259.085,85.6258,43.6055,41.3662,25.5818,94.8254,30.0775,29.3156, $
    44.7522,51.7154,624.646,37.1238,26.3169,0.00000,38.7515,36.4457,29.4709, $
    90.8166,51.2426,291.413,66.1488,0.00000,61.5153,42.0613,750.000,700.000]
  self.names = ["S I 1334", "C II 1335", "Ni II 1335", "C II 1336", "O IV 1339", "Ni II 1340", $
    "Ni II 1346", "Fe XII 1349", "Cl I 1352", "C I 1354", "O I 1356", "C I 1356", $
    "C I 1357", "C I 1358", "C I 1358", "O I 1359", "Fe II 1392", "S I 1393", $
    "Fe II 1393", "Ni II 1393", "Si IV 1394", "S I 1396", "O IV 1397", "S IV 1398", $
    "Ni II 1399", "O IV 1400", "Fe II 1400", "O IV 1401", "S I 1402", "Si IV 1403", $
    "S IV 1405", "O IV 1405", "Fe II 1406", "S IV 1406", "Mg II k 2796", "Mg II h 2803"]
    
  self.SJIvalues = [1330, 1400, 2796, 2832]
  self.SJIhwhm = [20, 20, 2, 2]
  
  self.missingvalue = -32768

  self.data_path_lmsal_l2 = '/irisa/data/level2/'
  self.data_path_lmsal_l3 = '/irisa/data/level3/'
  self.data_path_uio_l2 = '/mn/stornext/d10/PRITS/iris/data/level2/'
  self.data_path_uio_l3 = '/mn/stornext/d10/PRITS/iris/data/level3/'

  return, 1
END


PRO IRISsim_constants::cleanup
END


FUNCTION IRISsim_constants::get_pcharsize
  return, self.pcharsize
END


FUNCTION IRISsim_constants::get_pcharthick
  return, self.pcharthick
END


FUNCTION IRISsim_constants::get_color_active
  return, self.color_active
END


FUNCTION IRISsim_constants::get_color_inactive
  return, self.color_inactive
END


FUNCTION IRISsim_constants::get_PixCCDx
  return, self.PixCCDx
END


FUNCTION IRISsim_constants::get_PixCCDy
  return, self.PixCCDy
END


FUNCTION IRISsim_constants::get_PixCCDxSJI
  return, self.PixCCDxSJI
END


FUNCTION IRISsim_constants::get_PZTnoChangeValue
  return, self.PZTnoChangeValue
END


FUNCTION IRISsim_constants::get_FWnoChangeValue
  return, self.FWnoChangeValue
END


FUNCTION IRISsim_constants::get_FWinitial
  return, self.FWinitial
END


FUNCTION IRISsim_constants::get_solarxdisp
  return, self.solarxdisp
END


FUNCTION IRISsim_constants::get_solarydisp
  return, self.solarydisp
END


FUNCTION IRISsim_constants::get_slitwidth
  return, self.slitwidth
END


FUNCTION IRISsim_constants::get_fuv1displambda
  return, self.fuv1displambda
END


FUNCTION IRISsim_constants::get_fuv1startlambda
  return, self.fuv1startlambda
END


FUNCTION IRISsim_constants::get_fuv2displambda
  return, self.fuv2displambda
END


FUNCTION IRISsim_constants::get_fuv2startlambda
  return, self.fuv2startlambda
END


FUNCTION IRISsim_constants::get_nuvdisplambda
  return, self.nuvdisplambda
END


FUNCTION IRISsim_constants::get_nuvstartlambda
  return, self.nuvstartlambda
END


FUNCTION IRISsim_constants::get_CRSlineDescription, SIMcoords, type, wave=wave
  case type of
    'fu1': begin
      displambda = self.fuv1displambda
      startlambda = self.fuv1startlambda
    end
    'fu2': begin
      displambda = self.fuv2displambda
      startlambda = self.fuv2startlambda - displambda * self.PixCCDx
    end
    'nuv': begin
      displambda = self.nuvdisplambda
      startlambda = self.nuvstartlambda
      if SIMcoords[0] gt self.PixCCDx then $
        startlambda = self.nuvstartlambda - displambda * self.PixCCDx
    end
  endcase
  
  startw = startlambda + displambda * SIMcoords[0]
  endw = startlambda + displambda * SIMcoords[4]
  
  ind1 = self.lambda gt startw
  ind2 = self.lambda lt endw
  ind = where(ind1 eq ind2, count)
  if count gt 0 then begin
    m = max(self.intensities[ind], maxi)
    wave = self.lambda[ind[maxi]]
    description = self.names[ind[maxi]]
  endif else begin
    wave = (startw+endw)/2
    description = string(wave, format='(i4)')
  endelse
  
  return, description
END


FUNCTION IRISsim_constants::get_WavelineDescription, wavemin, wavemax, wave=wave
  ind1 = self.lambda gt wavemin
  ind2 = self.lambda lt wavemax
  ind = where(ind1 eq ind2, count)
  if count gt 0 then begin
    m = max(self.intensities[ind], maxi)
    wave = double(self.lambda[ind[maxi]])
    description = self.names[ind[maxi]]
  endif else begin
    wave = double(wavemin+wavemax)/2d
    description = string(wave, format='(i4)')
  endelse
  return, description
END


FUNCTION IRISsim_constants::get_ExpTypeName, Exp_type
  case Exp_Type of
    1: name = 'Light'
    2: name = 'Dark'
    3: name = 'LED'
    6: name = 'Test'
    else: name = '???'
  endcase
  return, name
END


FUNCTION IRISsim_constants::get_FWname, fw, noAngstrom=noAngstrom, frequency=frequency
  case fw of
    1: if keyword_set(frequency) then name ='5000' else name = 'Glass' ;5000 A
    31: begin
      name = '1330'
      if ~keyword_set(noAngstrom) then name = name + ' ' + String("305B) ;FUV 1
    end
    61: begin
      name = '2796'
      if ~keyword_set(noAngstrom) then name = name + ' ' + String("305B) ;NUV 1
    end
    91: begin
      name = '1400'
      if ~keyword_set(noAngstrom) then name = name + ' ' + String("305B) ;FUV 2
    end
    121: begin
      name = '2832'
      if ~keyword_set(noAngstrom) then name = name + ' ' + String("305B);NUV 2
    end
    151: if keyword_set(frequency) then name ='1600' else name = 'Broadband' ;1600 A
    self.FWnoChangeValue: name = 'No Move'
    else: name = strtrim(string(fw),2)
  endcase
  return, name
END


FUNCTION IRISsim_constants::get_WaveRange, startrow, endrow, type
  case type of
    'FUV1': begin
      displambda = self.fuv1displambda
      startlambda = self.fuv1startlambda
    end
    'FUV2': begin
      displambda = self.fuv2displambda
      startlambda = self.fuv2startlambda - displambda * self.PixCCDx
    end
    'NUV': begin
      displambda = self.nuvdisplambda
      startlambda = self.nuvstartlambda - displambda * self.PixCCDx
    end
  endcase
  
  range = dblarr(2)
  range[0] = startlambda + displambda * startrow
  range[1] = startlambda + displambda * endrow
  
  return, range
END


FUNCTION IRISsim_constants::get_SJIwaverange, IMG_PATH, wavemin=wavemin, wavemax=wavemax
  strings = strsplit(IMG_PATH, '_', /extract)
  strings = strings[N_ELEMENTS(strings)-1]
  wave = 0
  wavemin = 0
  wavemax = 0
  if is_number(strings) then begin
    wave = fix(strings)
    ind = where(self.SJIvalues eq wave, count)
    if count gt 0 then begin
      wavemin = double(wave-self.SJIhwhm[ind[0]])
      wavemax = double(wave+self.SJIhwhm[ind[0]])
    endif
  endif
  return, double(wave)
END


FUNCTION IRISsim_constants::get_missingvalue
  return, self.missingvalue
END


FUNCTION IRISsim_constants::get_data_path_lmsal_l2
  return, self.data_path_lmsal_l2
END


FUNCTION IRISsim_constants::get_data_path_lmsal_l3
  return, self.data_path_lmsal_l3
END



FUNCTION IRISsim_constants::get_data_path_uio_l2
  return, self.data_path_uio_l2
END


FUNCTION IRISsim_constants::get_data_path_uio_l3
  return, self.data_path_uio_l3
END


PRO IRISsim_constants__define

  void = { IRISsim_constants, $
    pcharsize : 0d, $
    pcharthick : 0d, $
    color_active : intarr(3), $
    color_inactive : intarr(3), $
    PixCCDx : 0L, $
    PixCCDy : 0L, $
    PixCCDxSJI : 0L, $
    PZTnoChangeValue : 0L, $
    FWnoChangeValue : 0L, $
    FWinitial : 0, $
    solarxdisp : 0d, $
    solarydisp : 0d, $
    slitwidth : 0d, $
    fuv1displambda : 0d, $
    fuv1startlambda : 0d, $
    fuv2displambda : 0d, $
    fuv2startlambda : 0d, $
    nuvdisplambda : 0d, $
    nuvstartlambda : 0d, $
    lambda : dblarr(36), $
    intensities : dblarr(36), $
    names : strarr(36), $
    SJIvalues : intarr(4), $
    SJIhwhm : intarr(4), $
    missingvalue : 0, $
    data_path_lmsal_l2 : '', $
    data_path_lmsal_l3 : '', $
    data_path_uio_l2 : '', $
    data_path_uio_l3 : '' $
  }
    
END
