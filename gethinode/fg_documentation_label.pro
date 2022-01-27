FUNCTION fg_documentation_label, iindex, label=label, type=type, unit=unit, QUIET=quiet
;+
;   Name: fg_documentation_label
;
;   Purpose: return text documentation for each observable and
;      optional label for use in an output filename
;
;   Input Parameters:
;      iindex -- SSW index header for any type of FG observable (if array, all must be same observable)
;
;   Output Paramters:
;      doc = string array of text describing each observable
;
;   Keyword Parameters:
;      label = returned string array of 2-character labels for each observable
;        for use in a filename. label(j) refers to data(*,*,j)
;      type = returned string (array) of data type (BTYPE)
;      unit = returned string (array) of data unit (BUNIT)
;      
;   Calling Sequence:
;      IDL> doc = fg_documentation_label(index[0],label=label)
;
;   History:
;v0.9   10-Oct-2007 - T. Tarbell, LMSAL
;v1.0   Minor syntax changes, logic fix on index[0] check. T. Berger,
;       LMSAL  18-Oct-2007.  
;v1.1   Added FG simple gen_id and changed "Unknown..." statement. TEB,
;       LMSAL, 19-Oct-2007.
;v1.11  Changed to gt_tagval and added missing values for handling
;       flats and darks. TEB 25-Oct-2007. 
;v1.12  Bug fix in assignment of index from iindex. TEB 6-Nov-2007.
;v1.13  New gen_id 7 (MG2 I & V but really Dopplergram) and new obs_id's 69-77. TDT 3-Jan-2008.
;v1.14  New gen_id's 4, 6, 9 and new obs_id's 78-85. TDT 8-Apr-2008.
;v1.15  Added old gen_id's 34-36 and old obs_id's 34-36, 45-47 for future use. TDT 16-Apr-2008.
;v1.16  New gen_id's 8, 18, and new obs_id's 86-9.  TDT 9-Mar-2009
;v1.17  2011-12-16 - Greg Slater, LMSAL. Handled gen_id 12 for focus
;                    scans.
;       23-Mar-2015  TDT  documentation lines for each observable,
;       some gaps still
;       20-Jan-2016  TDT  added label keyword
;v1.18  21-Jan-2016 - Added unit and type keywords,
;                     Initialising doc, label, type, unit as empty strings - Martin Wiesmann, ITA, UIO
;-
; $Id: fg_documentation_label.pro,v 1.5 2016/01/22 13:48:55 mawiesma Exp $  ;
;-----------------------------------------------------------------------------
;;Name, version
prognam = 'fg_documentation.pro'
progver = 'V1.18'

loud = 1 - KEYWORD_SET(quiet)
niv = REPLICATE(1,2)
niuv = REPLICATE(1,3)
niquv = REPLICATE(1,4)
nv_or_dg_only = [1]             ; Note these are already dark & flatfield corrected

if (N_ELEMENTS(iindex) gt 1) then index = REFORM(iindex[0]) else index=iindex
fgnint = gt_tagval(index,/fgnint,missing=1)
gen_id = gt_tagval(index,/gen_id,missing=1)
obs_id = gt_tagval(index,/obs_id,missing=1)

if (loud) then MESSAGE,/INFO,'Gen_id = '+STRTRIM(gen_id,2)

doc=''
label=''
type=''
unit=''

case gen_id of

  1: begin                                    ;FG (simple)
       neff = 1
       if (loud) then MESSAGE,/info,fg_genid(1)	; obs id 1, 21-7, 71-2
       doc = 'Simple filtergram, unit = DN = 64 photoelectrons'
       doc = [doc,'Use fg_bfi_units.pro to convert to physical units']
       label = ['FG']
       type = 'Simple filtergram'
       unit = 'DN = 64 photoelectrons'
    end

  2: begin
       neff = niv*fgnint*2                     ; FGIV
	if (loud) then MESSAGE,/info,fg_genid(2)	; obs id 2, 41, 42, 61-67
        doc = '[Stokes I, Stokes V], unit = DN = 64 photoelectrons'
        doc = [doc,'Made from '+string(neff,form='(I3)')+' exposures']
        label = ['I','V']
        type = ['Stokes I', 'Stokes V']
        unit = 'DN = 64 photoelectrons'
    end

  3: begin
       neff = niquv*fgnint*4                   ; FGIQUV  (but obs_id's 69-70 and 74-77 are really I&V DG
	if (loud) then MESSAGE,/info,fg_genid(3) 	; obs_id 3, 57, 58, 59, 60, 68-70, 74-77
        doc = '[Stokes I, Q, U, V], unit = DN = 64 photoelectrons'
        doc = [doc,'Made from '+string(neff,form='(I3)')+' exposures']
        label = ['I','Q','U','V']
        type = ['Stokes I', 'Stokes Q', 'Stokes U', 'Stokes V']
        unit = 'DN = 64 photoelectrons'
        if (obs_id ge 69) then begin
           doc = '[Stokes I_blue+I_red, I_b-I_r, V_r-V_b, V_r+V_b], unit = DN = 64 photoelectrons'
           doc = [doc,'Made from 7 exposures']
           label = ['I','DG','V','NG']
           type = ['Stokes I_blue+I_red', 'Stokes I_blue-I_red', 'Stokes V_red-V_blue', 'Stokes V_red+V_blue']
        end
    end

  4: begin
       neff = nv_or_dg_only*fgnint*8                     ; FG MG4 V/I (8 images)
	if (loud) then MESSAGE,/info,fg_genid(4)	; obs id 83, 85
        doc = '[(Stokes V / Stokes I)*4096], unit = dimensionless ratio'
        doc = [doc,'Made from '+string(neff,form='(I3)')+' exposures']
        LABEL = ['MG']
        type = '(Stokes V / Stokes I)*4096'
        unit = 'dimensionless ratio'
    end

  6: begin
       neff = nv_or_dg_only*fgnint*4                   ; FG MG2 V/I (4 images)
	if (loud) then MESSAGE,/info,fg_genid(6)	; obs id 81-82, 84
        LABEL = ['MG']
    end

  7: begin
       neff = niv*fgnint*4                   ; MG2 I & V, but really Fe I 5576 I & Dopplergram
	if (loud) then MESSAGE,/info,fg_genid(7) 	; obs_id 73
           label = ['I','DG']
    end

  8: begin
     neff = nv_or_dg_only*fgnint*2                     ; MG1 V/I (already dark & flatfield corrected)
	if (loud) then MESSAGE,/info,fg_genid(8) 	; obs_id 8, 88-89
        LABEL = ['MG']
    end

  9: begin
       neff = nv_or_dg_only*fgnint*4                     ; FG DG4 (only vel downloaded)
	if (loud) then MESSAGE,/info,fg_genid(9)	; obs id 78-80
           doc = '[Stokes (I_blue - I_red)/(I_blue + I_red)*4096], unit = dimensionless ratio'
           doc = [doc,'Made from '+string(neff,form='(I3)')+' exposures']
        LABEL = ['DG']
        type = 'Stokes (I_blue - I_red)/(I_blue + I_red)*4096'
        unit = 'dimensionless ratio'
    end

  12: begin
        neff = 1                                        ; FG focus scan                  
        if (loud) then MESSAGE,/info,fg_genid(12)	; obs id 12
       doc = 'Simple filtergram in focus scan, unit = DN = 64 photoelectrons'
       doc = [doc,'Use fg_bfi_units.pro to convert to physical units']
       LABEL = ['FG']
       type = 'Simple filtergram in focus scan'
       unit = 'DN = 64 photoelectrons'
     end

  17: begin
        neff = niv*fgnint*2                             ; FGIV (.1s) obsolete, probably never run in flight
        if (loud) then MESSAGE,/info,fg_genid(17)       ; obs_id 17
        doc = '[Stokes I, Stokes V], unit = DN = 64 photoelectrons'
        doc = [doc,'Made from '+string(neff,form='(I3)')+' exposures']
        label = ['I','V']
        type = ['Stokes I', 'Stokes V']
        unit = 'DN = 64 photoelectrons'
     end

  18: begin
        neff = niuv*4*(fgnint/7)                   ; FGIVDG shuttered IV & Dopplergram
        if (loud) then MESSAGE,/info,fg_genid(18)    ; obs_id 86-87
        doc = ['TBD']
        label = ['I','DG', 'V']
     end

  32: begin			; FGSIV
         neff = niv*fgnint*16	; obs_id 32
         if (obs_id eq 44) then neff(0) = neff(0)/2
        doc = '[Stokes I, Stokes V], unit = DN = 64 photoelectrons'
        doc = [doc,'Made from '+string(neff(1),form='(I3)')+' exposures']
         if (loud) then MESSAGE,/info,fg_genid(32)
        label = ['I','V']
        type = ['Stokes I', 'Stokes V']
        unit = 'DN = 64 photoelectrons'
      end	

  33: begin			; FGSIQUV
        neff = niquv*fgnint*8	; obs_id 33
	 if (obs_id gt 33) then neff(0) = neff(0)/2	; obs_id 43 or 54
	 if (obs_id eq 54) then neff(3) = neff(3)/2
        if (loud) then MESSAGE,/info,fg_genid(33)
        doc = '[Stokes I, Q, U, V], unit = DN = 64 photoelectrons'
        doc = [doc,'Made from '+string(neff(1),form='(I3)')+' exposures']
        label = ['I','Q','U','V']
        type = ['Stokes I', 'Stokes Q', 'Stokes U', 'Stokes V']
        unit = 'DN = 64 photoelectrons'
     end

  34: begin			; FGSIQ obsolete, probably never run in flight
         neff = niv*fgnint*16	; obs_id 34, needs to be checked against real data
         if (obs_id eq 45) then neff(0) = neff(0)/2
         if (loud) then MESSAGE,/info,fg_genid(34)
        doc = '[Stokes I, Stokes Q], unit = DN = 64 photoelectrons'
        label = ['I','Q']
        type = ['Stokes I', 'Stokes Q']
        unit = 'DN = 64 photoelectrons'
      end	

  35: begin			; FGSIU obsolete, probably never run in flight
         neff = niv*fgnint*16	; obs_id 35, needs to be checked against real data
         if (obs_id eq 46) then neff(0) = neff(0)/2
        doc = '[Stokes I, Stokes U], unit = DN = 64 photoelectrons'
         if (loud) then MESSAGE,/info,fg_genid(35)
        label = ['I','U']
        type = ['Stokes I', 'Stokes U']
        unit = 'DN = 64 photoelectrons'
      end	

  36: begin			; FGSIUV obsolete, probably never run in flight
         neff = niuv*fgnint*16	; obs_id 36, needs to be checked against real data
         if (obs_id eq 47) then neff(0) = neff(0)/2
         if (loud) then MESSAGE,/info,fg_genid(36)
        doc = '[Stokes I, Stokes U, Stokes V], unit = DN = 64 photoelectrons'
        label = ['I','U','V']
        type = ['Stokes I', 'Stokes U', 'Stokes V']
        unit = 'DN = 64 photoelectrons'
      end	

  38: begin			; FGSIV (.2s)
        neff = niv*fgnint*8	; obs_id 38
	 if (obs_id eq 55) then neff(0) = neff(0)/2
        if (loud) then MESSAGE,/info,fg_genid(38)
        doc = '[Stokes I, V], unit = DN = 64 photoelectrons'
        doc = [doc,'Made from '+string(neff(1),form='(I3)')+' exposures']
        label = ['I','V']
        type = ['Stokes I', 'Stokes V']
        unit = 'DN = 64 photoelectrons'
     end	

  39: begin			; FGSIV (.1s)
        neff = niv*fgnint*8	; obs_id 39
	 if (obs_id eq 56) then neff(0) = neff(0)/2
        if (loud) then MESSAGE,/info,fg_genid(39)
        doc = '[Stokes I, V], unit = DN = 64 photoelectrons'
        doc = [doc,'Made from '+string(neff(1),form='(I3)')+' exposures']
        label = ['I','V']
        type = ['Stokes I', 'Stokes V']
        unit = 'DN = 64 photoelectrons'
     end	

  else: begin			; other GEN_ID
          MESSAGE,/INFO,'GEN_ID, OBS_ID not yet implemented: ',gen_id, obs_id
	   if (index.naxis eq 3) then neff = REPLICATE(1,index.naxis3) else neff = 1
        doc = 'GEN_ID, OBS_ID not yet implemented'
        label = ['NG']
       end	
endcase

if (loud) then begin
  nim = N_ELEMENTS(neff)
  MESSAGE,/info,'Number of images in set = '+STRTRIM(nim,2)
  for i=0,nim-1 do MESSAGE,/info,'Number of effective exposures in image '+STRTRIM(i+1,2)+' = '+STRTRIM(neff[i],2)
end

RETURN, doc
END
