function isnumeric,input
  ;+
  ; NAME:
  ;       isnumeric
  ;
  ; PURPOSE:
  ;       this function checks if a string can be converted into a numeric value
  ;
  ; CALLING SEQUENCE:
  ;       answer = isnumeric(inputstring)
  ;
  ; INPUT PARAMETER:
  ;       input: a string to be tested if it can be converted to a numeric value
  ;
  ; OUTPUTS:
  ;       returns 1 if the string can be converted to a numeric value, 0 otherwise
  ;
  ; COMMON BLOCKS:
  ;       none
  ;
  ; MODIFICATION HISTORY:
  ;       25-Apr-2013  M. Wiesmann, UIO, source: http://rosettacode.org/wiki/Determine_if_a_string_is_numeric
  ;
  ; $Id: isnumeric.pro,v 1.3 2013/07/19 00:53:34 mawiesma Exp $  ;
  ;-

  on_ioerror, false
  test = double(input)
  return, 1
  false: return, 0
end