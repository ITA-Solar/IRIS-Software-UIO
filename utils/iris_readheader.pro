function iris_readheader,f,struct=struct,extension=extension
;$Id: iris_readheader.pro,v 1.1 2013/09/30 08:28:05 viggoh Exp $
  if n_elements(extension) eq 0 then extension=0
  d=iris_obj(f)
  hdr=d->gethdr(extension,struct=struct)
  obj_destroy,d
  return,hdr
end
