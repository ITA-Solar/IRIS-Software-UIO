function IRISsim_structure, NumEntries, HEADER = HEADER_ONLY, $
    FrmList=FrameList, ObsList=ObsList, FDBList=FDBList, CRSList=CRSList
  ;+
  ;NAME:		IRISsim_structure
  ;PURPOSE:	Return the structure for an IRIS table
  ;CALLING SEQUENCE:
  ;	FrameList = IRISsim_structure(NumEntries,/Frm)
  ;	ObsList   = IRISsim_structure(NumEntries,/Obs,/Header)	; Header only
  ;
  ;INPUTS:
  ;	Num_entries	- Number of FDB entries
  ;	HEADER		- If this keyword is set, return only the HEADER
  ; FRMLIST   - If set, return FrameList structure
  ; OBSLIST   - If set, return Observing list structure
  ; FDBLIST   - If set, return FDBList structure
  ; CRSLIST   - If set, return CRSlist structure
  ;
  ;RETURNS:
  ;	Returns a blank nested structure that contains the specified list
  ;
  ;HISTORY:
  ; 19-Sep-2011, J. R. Lemen, Written
  ; 10-Oct-2011, J. R. Lemen, Added Flush to FrameList
  ; 17-Oct-2011, J. R. Lemen, Tref changing to floating. This is different
  ; 				from the on-orbit table, which will raw ticks
  ; 21-Mar-2012, M. Wiesmann, Changed the Data structures to adapt them to the planning tool
  ; 14-Jun-2012, M. Wiesmann, Changed structures to fit new schemas
  ; 18-Jun-2012, M. Wiesmann, Changed structures to have pointers to arrays, instead of arrays
    
; $Id: irissim_structure.pro,v 1.4 2013/09/04 15:25:38 mawiesma Exp $  ;

  if n_elements(NumEntries) then Num = long(NumEntries>1) else Num = 1L
  
  if keyword_set(Obslist) then Tab = 0 else $
    if keyword_set(FrameList) then Tab = 1 else $
    if keyword_set(FDBList) then Tab = 2 else $
    if keyword_set(CRSList) then Tab = 3 else $
    return, -1
    
  case Tab of
    0: TableSize = 6L*4 + (4+12+1+2*3)*4*Num + 4 ; Observing List TableSize
    1: TableSize = 3L*4 + 10*4* Num + 4          ; Frame List TableSize
    2: TableSize = 0L ; Frame Definition Block TableSize
    3: TableSize = 0L ; CCD Readout Spec TableSize
  endcase
  
  
  case Tab of
    ; Observing List Table (OBS)
    0: DATA = {ID:'', $           ;Header
      TableSize: TableSize, $
      Tref_Obs:0L, $
      NumEntries: Num, $
      Repeat_Obs:1L, $
      Cadence_Obs:0L, $
      ListType:0B, $
      Time:0d, $          ;Info
      DataSize:0d, $
      Description:'', $
      Tref_FRM:ptr_new(lonarr(Num)), $ ;Data
      FRM_ID:ptr_new(strarr(Num)), $
      Repeat_FRM:ptr_new(lonarr(Num)), $
      Flush:ptr_new(lonarr(Num)),   $
      InhibitSkip:ptr_new(lonarr(Num)),   $
      Tag:ptr_new(strarr(Num)), $
      Cadence_FRM:ptr_new(lonarr(Num)), $
      PZT_A_Abs:ptr_new(lonarr(Num)), $
      PZT_B_Abs:ptr_new(lonarr(Num)), $
      PZT_C_Abs:ptr_new(lonarr(Num)), $
      PZT_A_Step:ptr_new(lonarr(Num)), $
      PZT_B_Step:ptr_new(lonarr(Num)), $
      PZT_C_Step:ptr_new(lonarr(Num))}
      
    ; Frame List Table (FRM)
    1: DATA = {ID:'', $           ;Header
      TableSize: TableSize, $
      NumEntries: Num, $
      ListType:1B, $
      Time:0d, $          ;Info
      DataSize:0d, $
      Description:'', $
      Tref:ptr_new(lonarr(Num)), $ ;Data
      SJI_FDB_ID:ptr_new(strarr(Num)), $
      NUV_SG_FDB_ID:ptr_new(strarr(Num)), $
      FUV_SG_FDB_ID:ptr_new(strarr(Num)), $
      SJI_AEC:ptr_new(strarr(Num)), $
      NUV_AEC:ptr_new(strarr(Num)), $
      FUV_AEC:ptr_new(strarr(Num)), $
      Flush:ptr_new(lonarr(Num)), $
      InhibitSkip:ptr_new(lonarr(Num)),   $
      Focus:ptr_new(lonarr(Num)), $
      FW:ptr_new(lonarr(Num)), $
      PZT_A_Rel:ptr_new(lonarr(Num)), $
      PZT_B_Rel:ptr_new(lonarr(Num)), $
      PZT_C_Rel:ptr_new(lonarr(Num))}
      
    ; Frame Definition Block Table (FDB)
    2: DATA = {ID:'', $           ;Header
      TableSize: TableSize, $
      ListType:2B, $
      Time:0d, $          ;Info
      DataSize:0d, $
      Description:'', $
      Type:'', $
      CRSID:'', $         ;Data
      Exp_Duration:0L, $
      Exp_Type:0L, $
      CompN:0L, $
      CompK:0L, $
      LookupTableID:'', $
      Exp_Min:0L, $
      Exp_Max:0L}
      
    ; CCD Readout Spec Table (CRS)
    3: DATA = {ID:'', $           ;Header
      TableSize: TableSize, $
      SubRegions: Num, $
      Spectral:0L, $
      Spatial:0L, $
      ListType:3B, $
      Time:0d, $          ;Info
      DataSize:0d, $
      Description:'', $
      Type:'', $
      SubRegionID:ptr_new(lonarr(Num)), $ ;Data
      StartRow:ptr_new(lonarr(Num)), $
      EndRow:ptr_new(lonarr(Num)), $
      NumRows:ptr_new(lonarr(Num)), $
      StartCol:ptr_new(lonarr(Num)), $
      EndCol:ptr_new(lonarr(Num)), $
      NumCols:ptr_new(lonarr(Num))}
      
  endcase
  
  
  if keyword_set(HEADER_ONLY) then begin
  
    case Tab of
      ; Observing List Table (OBS)
      0: HEADER = {ID:'', $           ;Header
        TableSize: TableSize, $
        Tref_Obs:0L, $
        NumEntries: Num, $
        Repeat_Obs:1L, $
        Cadence_Obs:0L, $
        ListType:0B}
        
      ; Frame List Table (FRM)
      1: HEADER = {ID:'', $           ;Header
        TableSize: TableSize, $
        NumEntries: Num, $
        ListType:1B}
        
      ; Frame Definition Block Table (FDB)
      2: HEADER = {ID:'', $           ;Header
        TableSize: TableSize, $
        ListType:2B}
        
      ; CCD Readout Spec Table (CRS)
      3: HEADER = {ID:'', $           ;Header
        TableSize: TableSize, $
        SubRegions: Num, $
        Spectral:0L, $
        Spatial:0L, $
        ListType:3B}
        
    endcase
    return, HEADER
  endif ;HEADER_ONLY
  
  return, DATA
end
