function IRISl12_ascii_template

  n=62l
  
  template = { version:1.0, $
    datastart:1l, $
    delimiter:44b, $
    missingvalue:!VALUES.F_NAN, $
    commentsymbol:'', $
    fieldcount:n, $
    fieldtypes:lonarr(n), $
    fieldnames:STRARR(n), $
    fieldlocations:LONARR(n), $
    fieldgroups:lonarr(n)}
    
  template.fieldtypes[*] = 3
  template.fieldtypes[[1, 7, 8]] = 4
  template.fieldtypes[[2, 15, 16, 17]] = 7
  
  template.fieldgroups = indgen(n)
  template.fieldgroups[8] = template.fieldgroups[7]
  template.fieldgroups[10:11] = template.fieldgroups[9]
  template.fieldgroups[27] = template.fieldgroups[26]
  for i=1,7 do begin
    for j=0,3 do begin
      template.fieldgroups[29+i*4+j] = template.fieldgroups[29+j]
    endfor
  endfor
  
  template.fieldnames[0:32] = ['ImageNr', $
    'Time', $
    'Type', $
    'OBSrep', $
    'OBSentry', $
    'FRMrep', $
    'FRMentry', $
    'PZTx', $
    'PZTy', $
    'PZTa', $
    'PZTb', $
    'PZTc', $
    'FW', $
    'Focus', $
    'Flush', $
    'FRMid', $
    'FDBid', $
    'CRSid', $
    'LUT', $
    'CompN', $
    'CompK', $
    'ExpTime', $
    'Filefuv1', $
    'Filefuv2', $
    'Filenuv1', $
    'Filenuv2', $
    'SumSpec', $
    'SumSpat', $
    'NrRegions', $
    'StartRow', $
    'EndRow', $
    'StartCol', $
    'EndCol']
  template.fieldnames[33:60] = 'dummy'
  template.fieldnames[61] = 'zero'
  
  return, template
end