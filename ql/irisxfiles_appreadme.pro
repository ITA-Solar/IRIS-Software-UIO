function IRISxfiles_appReadme
;This function initializes some readme-files for the user, which are saved in the hidden .idl folder in the home 
;directory of the user
;It returns the path to that directory, which is used to save the default parameters for IRIS simulator
; $Id: irisxfiles_appreadme.pro,v 1.1 2013/07/30 13:41:23 mawiesma Exp $  ;

    AuthorDirname = 'viggoh'
    AuthorDesc = 'Viggo Hansteen, Institute of Theoretical Astrophysics, University of Oslo'
    AppDirname = 'iris_xfiles'
    AppDesc = 'IRIS Xfiles'
    AppReadmeText = 'IRIS Xfiles, written by Viggo Hansteen, ITA, University of Oslo (viggo.hansteen@astro.uio.no)' + $
      '/n these files can be deleted without restrictions.'
    AppReadmeVersion=1
    return, APP_USER_DIR(AuthorDirname, AuthorDesc, AppDirname, AppDesc, AppReadmeText, AppReadmeVersion)
end