pro github_test,test=test

; $Id: 2023-01-25 13:33 PST $

  message,'*** This routine does nothing but test GitHub',/cont
  if n_elements(test) ne 0 then begin
     message,'*** changing routine works '+test,/cont
  endif
end
