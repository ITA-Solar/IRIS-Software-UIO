function br_string3,i
;+
;  br_string3(i)
;
;            make a string of i, 3 characters padded from left with 0
;            if i gt 999, same number of characters as needed
;
;-
result=0
case 1 of
 (i lt 0)  : print,'i has an illegal value'
 (i le 9)  : result='00'+strtrim(string(i),2)
 (i le 99) : result='0'+strtrim(string(i),2)
 else      : result=strtrim(string(i),2)
endcase

return,result
end
