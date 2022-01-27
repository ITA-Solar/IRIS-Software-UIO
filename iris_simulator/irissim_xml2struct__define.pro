; This object class definition is used to read and parse an IRIS planning table xml-file
; (OBS, FRM, FDB, CRS)
;
; Usage:
;  listObject = OBJ_NEW('IRISsim_xml2struct') ;create a IRISsim_xml2struct object, which inherits IDLffXMLSAX
;  listObject->parseFile, filename       ;read and parse file
;  ObsList = listObject->getArray()      ;get structure with observation list table
;  OBJ_DESTROY, listObject               ;destroy object
;
; The returned structure (using getArray()) depends on what table is loaded,
; the structure is defined in IRISsim_structure.pro
;
; History:
;     16/Mar/2012: M.Wiesmann
;     14/Jun/2012: M.Wiesmann, changed it to fit the new xml-schemas
; $Id: irissim_xml2struct__define.pro,v 1.9 2014/05/27 14:22:33 mawiesma Exp $  ;


;---------------------------------------------------------------------------
; Init method
; Called when the xmlstruct object is created.

FUNCTION IRISsim_xml2struct::Init
  ; Initialize the value of the dataNum counter. This
  ; will be incremented as elements are added to the
  ; data array.
  ; Might not be necessary, yep, not necessary
  ;self.dataNum = 0
  ;print,self.dataNum,'init'
  ;initialize the pointer to the data structure, required
  self.listStruct=ptr_new(/allocate_heap)
  RETURN, self->IDLffxmlsax::Init()
END


PRO IRISsim_xml2struct::Cleanup

  IF (PTR_VALID(self.listStruct)) THEN PTR_FREE, self.listStruct
  
END


;---------------------------------------------------------------------------
; Characters method
; Called when parsing character data within an element.
; Adds data to the charBuffer field.

PRO IRISsim_xml2struct::characters, data
  self.charBuffer = self.charBuffer + data
END

;---------------------------------------------------------------------------
; StartElement method
; Called when the parser encounters the start of an element.

PRO IRISsim_xml2struct::startElement, URI, local, strName, attrName, attrValue

  ; start of document, get list-type
  CASE strName OF
    "OBS": self.listtype = 0B           ; OBS
    "FRAMELIST": self.listtype = 1B     ; FRM
    "FDB": self.listtype = 2B           ; FDB
    "CRS": self.listtype = 3B           ; CRS
    
    ; The header, we first need to know the number of entries, before we can create the actual strucure
    ; therefore, let's save the date in temporary variables
    "Header": BEGIN; start of the header
      self.intvar1 = 0L
      self.intvar1 = 0L
      self.intvar2 = 0L
      self.intvar3 = 0L
      self.intvar4 = 0L
      self.intvar5 = 0L
      self.strvar1 = ''
    END
    ; Reinitialize the charBuffer.
    "Id": self.charBuffer = ''          ; All
    "Size": self.charBuffer = ''        ; All
    "Entries": self.charBuffer = ''     ; OBS, FRM
    "Tr": self.charBuffer = ''          ; OBS
    "Repeat": self.charBuffer = ''      ; OBS
    "Cadence": self.charBuffer = ''     ; OBS
    "Subregions": self.charBuffer = ''  ; CRS
    "Spectral": self.charBuffer = ''    ; CRS
    "Spatial": self.charBuffer = ''     ; CRS
    
    ; Info
    ; start of the info-element, reinitialize the temporary variables
    "Info": BEGIN
      self.time = 0D
      self.datas = 0D
      self.descr = ''
      self.type = ''
    END
    ; Reinitialize the charBuffer.
    "Time": self.charBuffer = '' ; All
    "DataSize": self.charBuffer = '' ; All
    "Description": self.charBuffer = '' ; All
    "Type": self.charBuffer = '' ; FDB, CRS
    
    ; Data
    ; start of a new datablock, reinitialize the temporary variables
    "Data": BEGIN
      self.intvar1 = 0L
      self.intvar1 = 0L
      self.intvar2 = 0L
      self.intvar3 = 0L
      self.intvar4 = 0L
      self.intvar5 = 0L
      self.intvar6 = 0L
      self.intvar7 = 0L
      self.intvar8 = 0L
      self.intvar9 = 0L
      self.intvar10 = 0L
      self.intvar11 = 0L
      self.strvar1 = ''
      self.strvar2 = ''
      self.strvar3 = ''
      self.strvar4 = ''
      self.strvar5 = ''
      self.strvar6 = ''
      self.strvar7 = ''
    END
    ; Reinitialize the charBuffer.
    "Tr": self.charBuffer = ''          ; OBS, FRM
    "FID": self.charBuffer = ''         ; OBS
    "Repeat": self.charBuffer = ''      ; OBS
    "Flush": self.charBuffer = ''       ; OBS, FRM
    "InhibitSkip": self.charBuffer = '' ; OBS, FRM
    "Tag": self.charBuffer = ''         ; OBS
    "Cadence": self.charBuffer = ''     ; OBS
    "PZTA": self.charBuffer = ''        ; OBS
    "PZTB": self.charBuffer = ''        ; OBS
    "PZTC": self.charBuffer = ''        ; OBS
    "PZTAStep": self.charBuffer = ''    ; OBS
    "PZTBStep": self.charBuffer = ''    ; OBS
    "PZTCStep": self.charBuffer = ''    ; OBS
    "PZTAOff": self.charBuffer = ''     ; FRM
    "PZTBOff": self.charBuffer = ''     ; FRM
    "PZTCOff": self.charBuffer = ''     ; FRM
    "SJI": self.charBuffer = ''         ; FRM
    "NUV": self.charBuffer = ''         ; FRM
    "FUV": self.charBuffer = ''         ; FRM
    "SJI_AEC": self.charBuffer = ''     ; FRM
    "NUV_AEC": self.charBuffer = ''     ; FRM
    "FUV_AEC": self.charBuffer = ''     ; FRM
    "FW": self.charBuffer = ''          ; FRM
    "Focus": self.charBuffer = ''       ; FRM
    "CrsId": self.charBuffer = ''       ; FDB
    "Exp_Duration": self.charBuffer = ''; FDB
    "Exp_Type": self.charBuffer = ''    ; FDB
    "CompN": self.charBuffer = ''       ; FDB
    "CompK": self.charBuffer = ''       ; FDB
    "Min_Exposure": self.charBuffer = ''; FDB
    "Max_Exposure": self.charBuffer = ''; FDB
    "LookupTableId": self.charBuffer = ''; FDB
    "SubregionId": self.charBuffer = '' ; CRS
    "StartRow": self.charBuffer = ''    ; CRS
    "EndRow": self.charBuffer = ''      ; CRS
    "StartCol": self.charBuffer = ''    ; CRS
    "EndCol": self.charBuffer = ''      ; CRS
    
    else: print, 'Unknown element: ', strName, ' ; should not happen!!!'
    
  ENDCASE
END

;---------------------------------------------------------------------------
; EndElement method
; Called when the parser encounters the end of an element.

PRO IRISsim_xml2struct::EndElement, URI, Local, strName
  ;print, strName, '  =  ', self.charBuffer
  ;print,self.dataNum
  ; end of document
  CASE strName OF ;info block content is copied into the structure here
    "OBS": BEGIN           ; OBS
      listStructTemp = *self.listStruct
      listStructTemp.Time = self.time
      listStructTemp.DataSize = self.datas
      listStructTemp.Description = self.descr
      *self.listStruct = listStructTemp
    END
    "FRAMELIST": BEGIN     ; FRM
      listStructTemp = *self.listStruct
      listStructTemp.Time = self.time
      listStructTemp.DataSize = self.datas
      listStructTemp.Description = self.descr
      *self.listStruct = listStructTemp
    END
    "FDB": BEGIN          ; FDB
      listStructTemp = *self.listStruct
      listStructTemp.Time = self.time
      listStructTemp.DataSize = self.datas
      listStructTemp.Description = self.descr
      listStructTemp.Type = self.type
      *self.listStruct = listStructTemp
    END
    "CRS": BEGIN        ; CRS
      listStructTemp = *self.listStruct
      listStructTemp.Time = self.time
      listStructTemp.DataSize = self.datas
      listStructTemp.Description = self.descr
      listStructTemp.Type = self.type
      *self.listStruct = listStructTemp
    END
    
    ; The header, we first need to know the number of entries, before we can create the actual strucure
    ; therefore, let's save the date in temporary variables
    ; end of the header, create the actual structure, and save the correct values in it
    "Header": BEGIN
      CASE self.listtype OF
        0B: BEGIN
          listStructTemp = IRISsim_structure(self.intvar5, /Obs)
          if isnumeric(self.strvar1) then strtmp=fns64('##########', long64(self.strvar1)) $
          else strtmp=self.strvar1
          listStructTemp.ID = strtmp
          listStructTemp.TableSize = self.intvar4
          listStructTemp.NumEntries = self.intvar5
          listStructTemp.Tref_Obs = self.intvar1
          listStructTemp.Repeat_Obs = self.intvar3
          listStructTemp.Cadence_Obs = self.intvar2
        END
        1B: BEGIN
          listStructTemp = IRISsim_structure(self.intvar5, /Frm)
          if isnumeric(self.strvar1) then strtmp=fns64('##########', long64(self.strvar1)) $
          else strtmp=self.strvar1
          listStructTemp.ID = strtmp
          listStructTemp.TableSize = self.intvar4
          listStructTemp.NumEntries = self.intvar5
        END
        2B: BEGIN
          listStructTemp = IRISsim_structure(1, /FDB)
          if isnumeric(self.strvar1) then strtmp=fns64('##########', long64(self.strvar1)) $
          else strtmp=self.strvar1
          listStructTemp.ID = strtmp
          listStructTemp.TableSize = self.intvar4
        END
        3B: BEGIN
          listStructTemp = IRISsim_structure(self.intvar5, /CRS)
          if isnumeric(self.strvar1) then strtmp=fns('#####', long(self.strvar1)) $
          else strtmp=self.strvar1
          listStructTemp.ID = strtmp
          listStructTemp.TableSize = self.intvar4
          listStructTemp.SubRegions = self.intvar5
          listStructTemp.Spectral = self.intvar1
          listStructTemp.Spatial = self.intvar2
        END
      ENDCASE
      ; copy the structure to the class data structure
      ; there might also be a direct way, without using the temporary structure listStructTemp
      *self.listStruct = listStructTemp
    END
    ; Set the value of the temporary variables equal to the value of charBuffer.
    ; Tr, Repeat, Cadence are also used in Data, but they will be handled here
    "Id": self.strvar1 = self.charBuffer         ; All
    "Size": self.intvar4 = self.charBuffer         ; All
    "Entries": self.intvar5 = self.charBuffer    ; OBS, FRM
    "Tr": self.intvar1 = self.charBuffer          ; OBS
    "Repeat": self.intvar3 = self.charBuffer       ; OBS
    "Cadence": self.intvar2 = self.charBuffer      ; OBS
    "Subregions": self.intvar5 = self.charBuffer   ; CRS
    "Spectral": self.intvar1 = self.charBuffer     ; CRS
    "Spatial": self.intvar2 = self.charBuffer      ; CRS
    
    ; Info
    ; end of the info-element, write the description into the structure
    "Info": ;Info block comes now at the beginning, so we don't have the listStruct yet
    
    ; Set the value of the temporary variables equal to the value of charBuffer.
    "Time": self.time = self.charBuffer  ; All
    "DataSize": self.datas = self.charBuffer  ; All
    "Description": self.descr = self.charBuffer  ; All
    "Type": self.type = self.charBuffer  ; FDB, CRS
    
    ; Data
    ; end of the datablock, write the values into the structure
    "Data": BEGIN
      listStructTemp = *self.listStruct
      CASE self.listtype OF
        0B: BEGIN
          (*(listStructTemp).Tref_FRM)[self.dataNum] = self.intvar1
          if isnumeric(self.strvar1) then strtmp=fns64('##########', long64(self.strvar1)) $
          else strtmp=self.strvar1
          (*(listStructTemp).FRM_ID)[self.dataNum] = strtmp
          (*(listStructTemp).Repeat_FRM)[self.dataNum] = self.intvar3
          (*(listStructTemp).Flush)[self.dataNum] = self.intvar4
          (*(listStructTemp).Tag)[self.dataNum] = self.strvar2
          (*(listStructTemp).Cadence_FRM)[self.dataNum] = self.intvar2
          (*(listStructTemp).PZT_A_Abs)[self.dataNum] = self.intvar5
          (*(listStructTemp).PZT_B_Abs)[self.dataNum] = self.intvar6
          (*(listStructTemp).PZT_C_Abs)[self.dataNum] = self.intvar7
          (*(listStructTemp).PZT_A_Step)[self.dataNum] = self.intvar8
          (*(listStructTemp).PZT_B_Step)[self.dataNum] = self.intvar9
          (*(listStructTemp).PZT_C_Step)[self.dataNum] = self.intvar10
          (*(listStructTemp).InhibitSkip)[self.dataNum] = self.intvar11
        END
        1B: BEGIN
          (*(listStructTemp).Tref)[self.dataNum] = self.intvar1
          (*(listStructTemp).PZT_A_Rel)[self.dataNum] = self.intvar5
          (*(listStructTemp).PZT_B_Rel)[self.dataNum] = self.intvar6
          (*(listStructTemp).PZT_C_Rel)[self.dataNum] = self.intvar7
          if isnumeric(self.strvar1) then strtmp=fns64('##########', long64(self.strvar1)) $
          else strtmp=self.strvar1
          (*(listStructTemp).SJI_FDB_ID)[self.dataNum] = strtmp
          if isnumeric(self.strvar2) then strtmp=fns64('##########', long64(self.strvar2)) $
          else strtmp=self.strvar2
          (*(listStructTemp).NUV_SG_FDB_ID)[self.dataNum] = strtmp
          if isnumeric(self.strvar3) then strtmp=fns64('##########', long64(self.strvar3)) $
          else strtmp=self.strvar3
          (*(listStructTemp).FUV_SG_FDB_ID)[self.dataNum] = strtmp
          (*(listStructTemp).SJI_AEC)[self.dataNum] = self.strvar5
          (*(listStructTemp).NUV_AEC)[self.dataNum] = self.strvar6
          (*(listStructTemp).FUV_AEC)[self.dataNum] = self.strvar7
          (*(listStructTemp).Flush)[self.dataNum] = self.intvar4
          if abs(self.intvar3 - 1) le 3 then fw=1 $
          else if abs(self.intvar3 - 31) le 3 then fw=31 $
          else if abs(self.intvar3 - 61) le 3 then fw=61 $
          else if abs(self.intvar3 - 91) le 3 then fw=91 $
          else if abs(self.intvar3 - 121) le 3 then fw=121 $
          else if abs(self.intvar3 - 151) le 3 then fw=151 $
          else fw=self.intvar3
          (*(listStructTemp).FW)[self.dataNum] = fw
          (*(listStructTemp).Focus)[self.dataNum] = self.intvar2
          (*(listStructTemp).InhibitSkip)[self.dataNum] = self.intvar11
        END
        2B: BEGIN
          if isnumeric(self.strvar1) then strtmp=fns('#####', long(self.strvar1)) $
          else strtmp=self.strvar1
          listStructTemp.CRSID = strtmp
          listStructTemp.Exp_Duration = self.intvar1
          listStructTemp.Exp_Type = self.intvar2
          listStructTemp.CompN = self.intvar3
          listStructTemp.CompK = self.intvar4
          listStructTemp.LookupTableID = self.strvar2
          listStructTemp.Exp_Max = self.intvar5
          listStructTemp.Exp_Min = self.intvar6
        END
        3B: BEGIN
          (*(listStructTemp).SubRegionID)[self.dataNum] = self.intvar1
          (*(listStructTemp).StartRow)[self.dataNum] = self.intvar2
          (*(listStructTemp).EndRow)[self.dataNum] = self.intvar3
          (*(listStructTemp).NumRows)[self.dataNum] = self.intvar3 - self.intvar2 + 1 ;in xml-files, this is actually EndRow
          (*(listStructTemp).StartCol)[self.dataNum] = self.intvar4
          (*(listStructTemp).EndCol)[self.dataNum] = self.intvar5
          (*(listStructTemp).NumCols)[self.dataNum] = self.intvar5 - self.intvar4 + 1 ;in xml-files, this is actually EndCol
        END
      ENDCASE
      *self.listStruct = listStructTemp
      self.dataNum = self.dataNum + 1
    END
    ; Set the value of the temporary variables equal to the value of charBuffer.
    "Tr": self.intvar1 = self.charBuffer         ; OBS, FRM, will be handled by the same element of the header
    "FID": self.strvar1 = self.charBuffer          ; OBS
    "Repeat": self.intvar3 = self.charBuffer       ; OBS, will be handled by the same element of the header
    "Flush": self.intvar4 = self.charBuffer        ; OBS, FRM
    "InhibitSkip": self.intvar11 = self.charBuffer   ; OBS, FRM
    "Tag": self.strvar2 = self.charBuffer          ; OBS
    "Cadence": self.intvar2 = self.charBuffer      ; OBS, will be handled by the same element of the header
    "PZTA": self.intvar5 = self.charBuffer         ; OBS
    "PZTB": self.intvar6 = self.charBuffer         ; OBS
    "PZTC": self.intvar7 = self.charBuffer         ; OBS
    "PZTAStep": self.intvar8 = self.charBuffer     ; OBS
    "PZTBStep": self.intvar9 = self.charBuffer     ; OBS
    "PZTCStep": self.intvar10 = self.charBuffer    ; OBS
    "PZTAOff": self.intvar5 = self.charBuffer         ; FRM
    "PZTBOff": self.intvar6 = self.charBuffer         ; FRM
    "PZTCOff": self.intvar7 = self.charBuffer         ; FRM
    "SJI": self.strvar1 = self.charBuffer         ; FRM
    "NUV": self.strvar2 = self.charBuffer         ; FRM
    "FUV": self.strvar3 = self.charBuffer         ; FRM
    "SJI_AEC": self.strvar5 = self.charBuffer         ; FRM
    "NUV_AEC": self.strvar6 = self.charBuffer         ; FRM
    "FUV_AEC": self.strvar7 = self.charBuffer         ; FRM
    "FW": self.intvar3 = self.charBuffer         ; FRM
    "Focus": self.intvar2 = self.charBuffer        ; FRM
    "CrsId": self.strvar1 = self.charBuffer        ; FDB
    "Exp_Duration": self.intvar1 = self.charBuffer ; FDB
    "Exp_Type": self.intvar2 = self.charBuffer     ; FDB
    "CompN": self.intvar3 = self.charBuffer        ; FDB
    "CompK": self.intvar4 = self.charBuffer        ; FDB
    "Max_Exposure": self.intvar5 = self.charBuffer        ; FDB
    "Min_Exposure": self.intvar6 = self.charBuffer        ; FDB
    "LookupTableId": self.strvar2 = self.charBuffer ; FDB
    "SubregionId": self.intvar1 = self.charBuffer  ; CRS
    "StartRow": self.intvar2 = self.charBuffer     ; CRS
    "EndRow": self.intvar3 = self.charBuffer     ; CRS
    "StartCol": self.intvar4 = self.charBuffer     ; CRS
    "EndCol": self.intvar5 = self.charBuffer     ; CRS
    
    else: print, 'Unknown element: ', strName, ' ; should not happen!!!'
    
  ENDCASE
  
END

;---------------------------------------------------------------------------
; GetArray method
; Returns the current array stored internally. If
; no data is available, returns -1.

FUNCTION IRISsim_xml2struct::GetArray
  IF (self.dataNum EQ 0) THEN $
    RETURN, -1 $
  ELSE BEGIN
  listStructTemp = *self.listStruct
  RETURN, listStructTemp
ENDELSE
END

;----------------------------------------------------------------------------
; Reset method (svhh)
;
PRO IRISsim_xml2struct::Reset
  self.dataNum = 0
END 
;---------------------------------------------------------------------------
; Object class definition method.

PRO IRISsim_xml2struct__define

  ; Define a useful structure
  ; Structure (listStruct) will be defined, after the header is read (see EndElement-method)
  void = {IRISsim_xml2struct, $
    INHERITS IDLffXMLSAX, $
    charBuffer : "", $
    dataNum : 0L, $
    listtype: 0B, $
    intvar1: 0L, $
    intvar2: 0L, $
    intvar3: 0L, $
    intvar4: 0L, $
    intvar5: 0L, $
    intvar6: 0L, $
    intvar7: 0L, $
    intvar8: 0L, $
    intvar9: 0L, $
    intvar10: 0L, $
    intvar11: 0L, $
    strvar1: '', $
    strvar2: '', $
    strvar3: '', $
    strvar4: '', $
    strvar5: '', $
    strvar6: '', $
    strvar7: '', $
    time: 0D, $
    datas: 0D, $
    descr: '', $
    type: '', $
    listStruct: PTR_NEW(/ALLOCATE_HEAP) $
    }
    
END

; listtype: 0 = Observation list (OBS)
;           1 = Framelist (FRAMELIST)
;           2 = (FDB)
;           3 = (CRS)
