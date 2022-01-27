;+
; Project     :	STEREO
;
; Name        :	WCS_PROJ_TAB
;
; Purpose     :	Convert intermediate coordinates in TAB projection.
;
; Category    :	FITS, Coordinates, WCS
;
; Explanation :	This routine is called from WCS_GET_COORD to apply the
;               lookup table (TAB) projection to intermediate relative
;               coordinates.
;
; Syntax      :	WCS_PROJ_TAB, WCS, COORD
;
; Examples    :	See WCS_GET_COORD
;
; Inputs      :	WCS = A World Coordinate System structure, from FITSHEAD2WCS.
;               COORD = The intermediate coordinates, relative to the reference
;                       pixel (i.e. CRVAL hasn't been applied yet).
;
; Opt. Inputs :	None.
;
; Outputs     :	The projected coordinates are returned in the COORD array.
;
; Opt. Outputs:	None.
;
; Keywords    :	None.
;
; Calls       :	TAG_EXIST, NTRIM
;
; Common      :	None.
;
; Restrictions:	Because this routine is intended to be called only from
;               WCS_GET_COORD, no error checking is performed.
;
;               Currently, the projection is not applied when more than three
;               axes are linked through the same coordinate table array.
;
;               The EXTVER and EXTLEVEL parameters are not yet enforced.
;
; Side effects:	None.
;
; Prev. Hist. :	None.
;
; History     :	Version 1, 06-Jun-2005, William Thompson, GSFC
;               Version 2, 07-Apr-2017, Stein Haugan, ITA/UiO
;                       Fixed bug where coords used for look-up were
;                       overwritten before going on to next dimension
;                       using same look-up coordinates.
;               Version 3, 01-Jun-2017, Stein Haugan, ITA/UiO
;                       Fixed problem with reformat()'ing when coordinate
;                       table has a degenerate dimension.
;               2017-12-19: Gregal Vissers, added to CRISPEX distribution in
;                       aux/ninterpol. Version modified by Mats Löfdahl to
;                       include calls to crispex_interpolate_many
;
; Contact     :	WTHOMPSON
;-
;
pro crispex_wcs_proj_tab, wcs, coord
  ; Apparently necessary placeholder to have CRISPEX compile the right
  ; WCS_PROJ_TAB without errors
  wcs_proj_tab, wcs, coord
end

pro wcs_proj_tab, wcs, coord
on_error, 0
;
;  Step through each axis, and check if the axis uses the -TAB projection.
;
;  If this is the case, we must add in CRVAL and preserve these lookup
;  coordinates for when they are used later (not overwriting them as we
;  compute & store the actual coordinates).
; 
;  In order to save space, no values are stored for axes that are not involved
;  in any -TAB projection, accomplished by using a pointer array where
;  non-involved axes are represented by null-pointers
;
n_axis = n_elements(wcs.naxis)
lookup_coord = ptrarr(n_axis)
for i_axis = 0,n_axis-1 do begin
   ctype = wcs.ctype[i_axis]
   test = strupcase( strmid( ctype, 4, strlen(ctype)-4 ))
   if test eq '-TAB' then begin
;
;  Add in the reference value, whether or not the lookup table can be applied.
;  Store for later look-up use.
;
      coord[i_axis,*] = coord[i_axis,*] + wcs.crval[i_axis]
      lookup_coord[i_axis] = reform(ptr_new(coord[i_axis,*]))
   end
end   

;
for i_axis = 0,n_axis-1 do begin
    ctype = wcs.ctype[i_axis]
    test = strupcase( strmid( ctype, 4, strlen(ctype)-4 ))
    if test eq '-TAB' then begin
;
;  Make sure that the LOOKUP_TABLE structure is present.
;
        if tag_exist(wcs, 'LOOKUP_TABLE') then begin
            coordname = wcs.lookup_table.coordname
            indexname = wcs.lookup_table.indexname
            axisnum   = wcs.lookup_table.axisnum
;
;  See which other axes are associated through the same coordinate array.
;
            ww = where(coordname[i_axis] eq coordname, count)
;
;  If there's an index array, then interpolate from the intermediate values
;  into the index values.
;
            for i=0,count-1 do begin
                x = reform(*lookup_coord[ww[i]]) - 1
                if indexname[ww[i]] ne '' then begin
                    test = execute('y = wcs.lookup_table.' + indexname[ww[i]])
                    j = dindgen(n_elements(y))
                    x = interpol(j, y-1.d0, x)
                endif
                test = execute('x' + ntrim(axisnum[ww[i]]) + ' = x')
            endfor
;
;  Interpolate from the index values to the coordinate values.
;
;  A reform() is necessary to remove the *first* dimension of y after indexing
;  with axisnum[i_axis] (i.e. picking the values of the coordinate to be
;  looked up), BUT we should not remove any singular dimensions further on. So
;  we use the explicit form of reform(y,new_y_dims).
;
            test = execute('y = wcs.lookup_table.' + coordname[i_axis])
            y_dims = size(y,/dimensions)
            new_y_dims = y_dims[1:*]
            case count of
                1: begin
                    y = reform(y, new_y_dims)
                    coord[i_axis,*] = interpolate(y, x0)
                endcase
                2: begin
                    y = reform( y[ axisnum[i_axis], *, *], new_y_dims )
                    coord[i_axis,*] = interpolate(y, x0, x1)
                endcase
                3: begin
                    y = reform( y[ axisnum[i_axis], *, *, *], new_y_dims )
                    coord[i_axis,*] = interpolate(y, x0, x1, x2)
                endcase
                4: begin
                    y = reform( y[ axisnum[i_axis], *, *, *,*], new_y_dims )
                    coord[i_axis,*] = crispex_interpolate_many(y, x0, x1, x2, x3)
                endcase
                5: begin
                    y = reform( y[ axisnum[i_axis], *, *, *, *, *], new_y_dims )
                    coord[i_axis,*] = crispex_interpolate_many(y, x0, x1, x2, x3, x4)
                endcase
                6: begin
                    y = reform( y[ axisnum[i_axis], *, *, *, *, *, *], new_y_dims )
                    coord[i_axis,*] = crispex_interpolate_many(y, x0, x1, x2, x3, x4, x5)
                endcase
                7: begin
                    y = reform( y[ axisnum[i_axis], *, *, *, *, *, *, *], new_y_dims )
                    coord[i_axis,*] = crispex_interpolate_many(y, x0, x1, x2, x3, x4, x5, x6)
                endcase
                
                else: message, /continue, $
                  'N-dimensional > 7 TAB case not supported yet'
            endcase
        endif                   ;Lookup table exists
    endif                       ;Dimension uses table projection
endfor                          ;I_AXIS
;
return
end
