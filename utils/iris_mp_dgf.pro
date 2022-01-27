function iris_mp_dgf, x, y, e, fit0, fit1c, fit1a, dlambda, good, double = double

; f = x0 + x1*exp((x-x2)/x3)^2 + x4*exp((x-x5)/x6)^2
xlimited=[1,1]
; Background Intensity
x0={limited:xlimited, limits:fit0[0] + 0.25*[-1,1]*fit0[0], value:fit0[0]}

; First Component Peak Intensity
x1={limited:xlimited, limits:fit0[1] + 0.25*[-1,1]*fit0[1], value:fit0[1]}

; First Component Line Center Position
x2={limited:xlimited, limits:fit0[2] + 6*[-dlambda,dlambda], value:fit0[2]}

; First Component Gaussian Width
x3={limited:xlimited, limits:fit0[3] + 2*[-dlambda,dlambda], value:fit0[3]}

; Second Component Peak Intensity
l = [0.0 , 5]* fit1a * fit0[1] 
x4={limited:xlimited, limits:l, value:fit1a*fit0[1]}

; Second Component Line Center Position
x5={limited:xlimited, limits:fit1c + 6*[-dlambda , dlambda], value:fit1c}

; Second Component Gaussian Width
x6={limited:xlimited, limits:fit0[3] + 2*[-dlambda,dlambda], value:fit0[3]}

parinfo=[x0,x1,x2,x3,x4,x5,x6]
param = parinfo.value

res = mpfitfun('iris_doublegauss', x[good], y[good], e[good], param, parinfo=parinfo, $
	maxiter = 1000, dof = dof, bestnorm = bestnorm, yfit = yfit, double = double,$
	status = status, /quiet, perror=perror)

result = {b:res[0], i1:res[1], p1:res[2], w1:res[3], i2:res[4], p2:res[5], w2:res[6], fit:yfit, status:status, error:perror}
;if N_ELEMENTS(perr) gt 0 then result = {b:res[0], i1:res[1], p1:res[2], w1:res[3], i2:res[4], p2:res[5], w2:res[6], fit:yfit, status:status, error:perror} $
;  else result = {b:res[0], i1:res[1], p1:res[2], w1:res[3], i2:res[4], p2:res[5], w2:res[6], fit:yfit, status:status, error:-1}

return, result

end