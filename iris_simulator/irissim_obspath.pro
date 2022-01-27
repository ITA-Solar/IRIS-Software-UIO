PRO IRISsim_OBSpath, OBSpath, save=save

  ; $Id: irissim_obspath.pro,v 1.1 2013/05/09 19:06:21 mawiesma Exp $  ;

  file = IRISsim_appReadme() + '/irissim_obspath.sav'
  
  if keyword_set(save) then $
    save, OBSpath, filename=file $
  else $
    if FILE_TEST(file) then restore, filename=file $
  else OBSpath = ''
  
END
