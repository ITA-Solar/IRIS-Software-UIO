FUNCTION fns64, template, num

;converts a scalar number given in "num" to a string and puts
;it into a string given in "template" at the place in template
;marked by #-signs. Returns the merged string.  (After ANA routine FNS).


ON_ERROR,2

;delimiter: ASCII 35B = '#'
dl = 35B

;find number of #-signs in template to determine how many leading 
;zeros must be added to num before injection:
ts = byte(template)
nel = n_elements(ts)
nps = 0
for i=0,nel-1 do if ts(i) eq dl then nps = nps + 1
if nps eq 0 then begin
	message,'Invalid template: no #-signs'
	return,'-1'
end

;convert num to a string and add the leading zeros if necessary
snum = strtrim(string(long64(num)),2)
if strlen(snum) gt nps then begin
	message,'Invalid number: length is greater than template space'
	return,'-1'
end
while strlen(snum) lt nps do snum = '0' + snum

;create a COPY of template so the STRPUT doesn't overwrite it:
temp2 = template

;insert snum at location of first #-sign in template and return it:
strput,temp2,snum,strpos(template,'#')

return,temp2

END







