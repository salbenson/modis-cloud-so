;***************************************
;  No ceres file for 20181206 14hrs Aqua
;  No ceres file for 20181213 16hrs Aqua
;  20190107 14 hrs Aqua
;  20190213 15 hrs Aqua
;***************************************

pro read_ceres_ssf

;*********************************************
;  Input
;*********************************************
syear='2018'
smonth='01'
sday='29'
shour='23'

;do_hist='no'
do_hist='yes'
;*********************************************
;  Constants
;*********************************************

;path_prefix='/Volumes/'
path_prefix='/uufs/chpc.utah.edu/common/home/'

;  Modis histogram directory.  Histogram files contain the trajectory.
hdir=path_prefix+'mace-group4/modis/hysplit/modis_histograms_sm/'

;  Ceres data directory
cdir=path_prefix+'mace-group4/ceres/CERES_SSF/'

;  Ceres file name string
;cname='CER_SSF_Aqua-FM3-MODIS_Edition4A_*.' & eos='MYD06_L2.A'
cname='CER_SSF_Terra-FM2-MODIS_Edition4A_*.' & eos='MOD06_L2.A'

;  Histogram grid spacing
dlon=1.0 & dlat=0.5

;***********************************************
;  Loop through ceres files for this day
;***********************************************

filestr_ceres=cdir+syear+'/'+smonth+'/'+cname+syear+smonth+sday+shour+'*hdf'
files_ceres=file_search(filestr_ceres,count=num_files_ceres)

;  Loop through ceres files
for f=0,num_files_ceres-1 do begin
  
  print,files_ceres[f]
  get_cloudsat,'',files_ceres[f],'Time of observation',ctime
  get_cloudsat,'',files_ceres[f],'Colatitude of CERES FOV at surface',clats
  get_cloudsat,'',files_ceres[f],'Longitude of CERES FOV at surface',ccolons
  get_cloudsat,'',files_ceres[f],'CERES SW TOA flux - upwards',sw_up
  get_cloudsat,'',files_ceres[f],'CERES LW TOA flux - upwards',lw_up
  get_cloudsat,'',files_ceres[f],'TOA Incoming Solar Radiation',sw_toa
  r=where(ctime gt 2.48e6,c)
  if c gt 0 then begin
    r=where(ctime le 2.5e6)
    ctime=ctime[r]
    clats=clats[r]
    ccolons=ccolons[r]
    sw_up=sw_up[r]
    lw_up=lw_up[r]
    sw_toa=sw_toa[r]
    print,'got rid of bad time values'
  endif
 
  caldat,ctime,mm,dd,yy,hh,mi,ss
  print, yy[0],mm[0],dd[0],hh[0],mi[0],ss[0],'ceres start'
  print, yy[-1],mm[-1],dd[-1],hh[-1],mi[-1],ss[-1],'ceres end'
  
  ;  Converts co_latitude to latitude
  clats = (90.-clats)

  ;  Converts co_longitude to longitude
  clons=ccolons
  r=where(clons ge 180.,c)
  if c gt 0 then clons[r]=clons[r]-360.

  ;  set some fill values to missing
  r = where(sw_up gt 1e38,c)
  if c gt 0 then sw_up[r]=-9999.
  r = where(lw_up gt 1e38,c)
  if c gt 0 then lw_up[r]=-9999.
  r = where(sw_toa gt 1e38,c)
  if c gt 0 then sw_toa[r]=-9999.
  
  if do_hist eq 'yes' then begin
  
    ;  Extract the hour from the ceres file name
    parts=strsplit(file_basename(files_ceres[f]),'.',/extract)
    sday=strmid(parts[1],6,2)
    shour=strmid(parts[1],8,2)
 
    ;  Calculate day of year 0-365
    julian_date,syear,smonth,sday,doy  
    sdoy=string(doy,format='(I03)')
    print, syear,smonth,sday,shour,'doy',sdoy
    
    ;  Look for histograms for this hour of ceres data
    filestr_hist=eos+syear+sdoy+'.'+shour+'*'+'*histo.cdf'
    
    files_hist=file_search(hdir+syear+'/'+sdoy+'/'+filestr_hist,count=num_files_hist)
  
    print,'num histograms',num_files_hist
  
    if num_files_hist gt 0 then begin
  
;      print,files_ceres[f]
;      get_cloudsat,'',files_ceres[f],'Time of observation',ctime
;      get_cloudsat,'',files_ceres[f],'Colatitude of CERES FOV at surface',clats
;      get_cloudsat,'',files_ceres[f],'Longitude of CERES FOV at surface',ccolons
;      get_cloudsat,'',files_ceres[f],'CERES SW TOA flux - upwards',sw_up
;      get_cloudsat,'',files_ceres[f],'CERES LW TOA flux - upwards',lw_up
;      get_cloudsat,'',files_ceres[f],'TOA Incoming Solar Radiation',sw_toa
;    
;      ;  Converts co_latitude to latitude
;      clats = (90.-clats)
;
;      ;  Converts co_longitude to longitude
;      clons=ccolons
;      r=where(clons ge 180.,c)
;      if c gt 0 then clons[r]=clons[r]-360.
;
;      ;  set some fill values to missing
;      r = where(sw_up gt 1e38,c)
;      if c gt 0 then sw_up[r]=-9999.
;      r = where(lw_up gt 1e38,c)
;      if c gt 0 then lw_up[r]=-9999.
;      r = where(sw_toa gt 1e38,c)
;      if c gt 0 then sw_toa[r]=-9999.

      ;  Now loop through hist files
      hlat_array=make_array(num_files_hist,/float,value=-9999)
      hlon_array=make_array(num_files_hist,/float,value=-9999)
      for ff=0,num_files_hist-1 do begin
        fid=ncdf_open(files_hist[ff],/write)
        vid=ncdf_varid(fid,'julian_day') & ncdf_varget,fid,vid,jday_histo
        vid=ncdf_varid(fid,'center_latitude') & ncdf_varget,fid,vid,hlat
        vid=ncdf_varid(fid,'center_longitude') & ncdf_varget,fid,vid,hlon
        ;ncdf_close,fid
        hlat_array[ff]=hlat
        hlon_array[ff]=hlon
        hidx=where(clats ge hlat-dlat and clats le hlat+dlat and $
                   ccolons ge hlon-dlon and ccolons le hlon+dlon and $
                   sw_up ne -9999 and sw_toa ne -9999,chidx)
        print,files_hist[ff],chidx           
        if chidx gt 0 then begin
          temp_sw_up=sw_up[hidx]
          r=where(temp_sw_up lt 0,c)
          if c gt 0 then stop
          
          temp_sw_toa=sw_toa[hidx]
          r=where(temp_sw_toa lt 0,c)
          if c gt 0 then stop
          
          mean_sw_up=mean(sw_up[hidx])
          sdev_sw_up=stddev(sw_up[hidx])
          mean_lw_up=mean(lw_up[hidx])
          sdev_lw_up=stddev(lw_up[hidx])
          mean_sw_toa=mean(sw_toa[hidx])
          sdev_sw_toa=stddev(sw_toa[hidx])
        endif else begin
          mean_sw_up=-9999
          sdev_sw_up=-9999
          mean_lw_up=-9999
          sdev_lw_up=-9999
          mean_sw_toa=-9999
          sdev_sw_toa=-9999
        endelse
        sw_up_id=ncdf_varid(fid,'mean_sw_up')
        ncdf_control,fid,/redef
        if sw_up_id eq -1 then sw_up_id=ncdf_vardef(fid,'mean_sw_up')
        ncdf_attput,fid,sw_up_id,'long_name','mean CERES SW TOA flux - upwards'
        ncdf_attput,fid,sw_up_id,'units','W/m2'
        ncdf_attput,fid,sw_up_id,'file',file_basename(files_ceres[f])
        sw_up_sdev_id=ncdf_varid(fid,'sdev_sw_up')
        if sw_up_sdev_id eq -1 then sw_up_sdev_id=ncdf_vardef(fid,'sdev_sw_up')
        ncdf_attput,fid,sw_up_sdev_id,'long_name','sdev CERES SW TOA flux - upwards'
        ncdf_attput,fid,sw_up_sdev_id,'units','W/m2'
        ncdf_attput,fid,sw_up_sdev_id,'file',file_basename(files_ceres[f])
        lw_up_id=ncdf_varid(fid,'mean_lw_up')
        if lw_up_id eq -1 then lw_up_id=ncdf_vardef(fid,'mean_lw_up')
        ncdf_attput,fid,lw_up_id,'long_name','mean CERES LW TOA flux - upwards'
        ncdf_attput,fid,lw_up_id,'units','W/m2'
        ncdf_attput,fid,lw_up_id,'file',file_basename(files_ceres[f])
        lw_up_sdev_id=ncdf_varid(fid,'sdev_lw_up')
        if lw_up_sdev_id eq -1 then lw_up_sdev_id=ncdf_vardef(fid,'sdev_lw_up')
        ncdf_attput,fid,lw_up_sdev_id,'long_name','sdev CERES LW TOA flux - upwards'
        ncdf_attput,fid,lw_up_sdev_id,'units','W/m2'
        ncdf_attput,fid,lw_up_sdev_id,'file',file_basename(files_ceres[f])
        sw_toa_id=ncdf_varid(fid,'mean_sw_toa')
        if sw_toa_id eq -1 then sw_toa_id=ncdf_vardef(fid,'mean_sw_toa')
        ncdf_attput,fid,sw_toa_id,'long_name','mean TOA Incoming Solar Radiation'
        ncdf_attput,fid,sw_toa_id,'units','W/m2'
        ncdf_attput,fid,sw_toa_id,'file',file_basename(files_ceres[f])
        sw_toa_sdev_id=ncdf_varid(fid,'sdev_sw_toa')
        if sw_toa_sdev_id eq -1 then sw_toa_sdev_id=ncdf_vardef(fid,'sdev_sw_toa')
        ncdf_attput,fid,sw_toa_sdev_id,'long_name','sdev TOA Incoming Solar Radiation'
        ncdf_attput,fid,sw_toa_sdev_id,'units','W/m2'
        ncdf_attput,fid,sw_toa_sdev_id,'file',file_basename(files_ceres[f])
        ncdf_control,fid,/endef
        ncdf_varput,fid,sw_up_id,mean_sw_up
        ncdf_varput,fid,sw_up_sdev_id,sdev_sw_up
        ncdf_varput,fid,lw_up_id,mean_lw_up
        ncdf_varput,fid,lw_up_sdev_id,sdev_lw_up
        ncdf_varput,fid,sw_toa_id,mean_sw_toa    
        ncdf_varput,fid,sw_toa_sdev_id,sdev_sw_toa    
        ncdf_close,fid
          if 1 eq 0 then begin
            ;  Top is the last color to scale 256 colors, 0-255
            top_color=252
            ;  Colortable  0-252  253=white
            mytable=colortable(34,ncolors=254)
            ;mytable=colortable(5,ncolors=254)
            ;254=hot pink               ;gray=255
            mytable=[mytable,transpose([238,18,137]),transpose([180,180,180])]
            mycbtable=mytable[0:top_color,*]

            pxdim=900 & pydim=800
            xl=0.1 & xr=0.75
            yb=0.07 & yt=0.95
            sx=0.10
            sy=0.07
            numplots_x=1
            numplots_y=1
            position_plots,xl,xr,yb,yt,sx,sy,numplots_x,numplots_y,pos
            ;  Colorbar position
            cbpos=pos
            cbpos[*,0]=pos[*,2]+0.07
            cbpos[*,2]=cbpos[*,0]+0.007
            cbpos[*,1]=cbpos[*,1]+0.02
            cbpos[*,3]=cbpos[*,3]-0.02
            dx=0.01
            dy=0.01
            ;  Date format for plotting
            dummy=label_date(date_format=['%Z/%M/%D %H:%I'])
            pnum=0
            p0=plot([0,1],[0,1],position=pos[pnum,*],/buffer,dimensions=[pxdim,pydim],axis_style=4,/nodata)
            d=p0.convertcoord([pos[pnum,0],pos[pnum,2]],[pos[pnum,1],pos[pnum,3]],/normal,/to_device)
            isx=(d[0,1,0]-d[0,0,0])
            isy=(d[1,1,0]-d[1,0,0])
        
            r=where(sw_up ne -9999,c)
            dmax_sw_up=max(sw_up[r])
            dmin_sw_up=min(sw_up[r])
        
            map_projection='Mercator'
            ;map_projection='Mollweide'

            p0=image(sw_up[hidx],ccolons[hidx],clats[hidx],$
              ;limit=[llat,llon,ulat,rlon],$  ;map limit vector
              /current,$  ;Put it in the current open plot window p0
              grid_units='degrees',$
              /box_axes,$
              max_value=dmax_sw_up,min_value=dmin_sw_up,$
              ;center_longitude=(rcolon+lcolon)/2.0,$
              label_format='MapGrid_Labels',$
              label_position=0,$
              font_size=12,$  ;bigger font
              linestyle=2,$  ;grid will be dashed line
              rgb_table=mycbtable,$  ;use color table 5
              map_projection=map_projection,$  ;map projection
              position=pos[pnum,*])
              ;  Put on a colorbar
            c0=colorbar($
              target=p0,$
              ;range=[dmin_sw_up,dmax_sw_up],$  ;the colorbar is to match the image labeled p1
              orientation=1,position=reform(cbpos[pnum,*]),font_size=12,rgb_table=mycbtable,$
              tickdir=1,border_on=1,title='SW UP')
            imagename=strsplit(file_basename(files_ceres[f]),'hdf',/extract,/regex)+'box.'+string(ff,format='(I02)')+'.png'
            p0.save,imagename
          endif  ;end of do plot
      endfor  ;end of loop through histogram files
      help,hlon_array
      hcolon_array=hlon_array
      r=where(hlon_array gt 180,c)
      if c gt 0 then hlon_array[r]=hlon_array[r]-360.0
    endif else begin ;found histogram files files
      spawn,'rm '+files_ceres[f]
      print,'no histograms for ceres file'
    endelse
  endif  ;end of do hist
  
;*****************************************
;  Subset ceres in southern ocean
;*****************************************
  do_plot='no'
  if do_plot eq 'yes' and num_files_hist gt 0 then begin
   
    ulat=-20.0 & llat=-80.0
    llon=00.0 & rlon=-150.0
    ;ulat=90.0 & llat=-90.0
    ;llon=-180.0 & rlon=180.0

    if llon lt 0 then lcolon=360.0+llon else lcolon=llon
    if rlon lt 0 then rcolon=360.0+rlon else rcolon=rlon

    ridx=where(clats ge llat and clats le ulat and $
      ccolons ge lcolon and ccolons le rcolon,cidx)
    
      ;  Top is the last color to scale 256 colors, 0-255
      top_color=252
      ;  Colortable  0-252  253=white
      mytable=colortable(34,ncolors=254)
      ;mytable=colortable(5,ncolors=254)
      ;254=hot pink               ;gray=255
      mytable=[mytable,transpose([238,18,137]),transpose([180,180,180])]
      mycbtable=mytable[0:top_color,*]
 
      pxdim=900 & pydim=800
      xl=0.08 & xr=0.75
      yb=0.07 & yt=0.95
      sx=0.10
      sy=0.07
      numplots_x=1
      numplots_y=1
      position_plots,xl,xr,yb,yt,sx,sy,numplots_x,numplots_y,pos  
      ;  Colorbar position
      cbpos=pos
      cbpos[*,0]=pos[*,2]+0.07
      cbpos[*,2]=cbpos[*,0]+0.007
      cbpos[*,1]=cbpos[*,1]+0.02
      cbpos[*,3]=cbpos[*,3]-0.02
      dx=0.01
      dy=0.01
      ;  Date format for plotting
      dummy=label_date(date_format=['%Z/%M/%D %H:%I'])
      pnum=0
      p0=plot([0,1],[0,1],position=pos[pnum,*],/buffer,dimensions=[pxdim,pydim],axis_style=4,/nodata)
      d=p0.convertcoord([pos[pnum,0],pos[pnum,2]],[pos[pnum,1],pos[pnum,3]],/normal,/to_device)
      isx=(d[0,1,0]-d[0,0,0])
      isy=(d[1,1,0]-d[1,0,0])
      map_projection='Mercator'
      ;map_projection='Mollweide'
      
      lcolon=min(ccolons[ridx]) & rcolon=max(ccolons[ridx])
      
      ;  Calculate regular grid array of the plotting box
      delta=0.5
      ynum=fix((ulat-llat)/delta)
      grid_lat=findgen(ynum)*delta+llat
      
      xnum=fix((rcolon-lcolon)/delta)
      grid_colon=findgen(xnum)*delta+lcolon
      grid_lon=grid_colon
      r=where(grid_colon ge 180,c)
      if c gt 0 then grid_lon[r]=grid_lon[r]-360.0
     
      ;xnum=fix((rlon-llon)/delta)
      ;grid_lon=findgen(xnum)*delta+llon
      ;grid_colon=grid_lon
      ;r=where(grid_colon lt 0,c)
      ;if c gt 0 then grid_colon[r]=360.0+grid_colon[r]
  
      data_var=sw_up;[ridx]
      data_lat=clats;[ridx]
      data_lon=clons;[ridx]
      r=where(data_var ne -9999,c)
      dmax_sw_up=max(data_var[r])
      dmin_sw_up=min(data_var[r])
      ;  Triangulate the data
      print,'start qhull'
      qhull,data_lon,data_lat,triangles,/delaunay,sphere=s
      print,'end qhull,start griddata'
      grid_var=griddata(data_lon,data_lat,data_var,xout=grid_lon,yout=grid_lat,$
        /grid,/degrees,/sphere,triangles=triangles,$
        /kriging,min_points=16,sectors=8,empty_sectors=3,missing=-9999)
      print,'end griddata'
      ;   Bytscal the data
      data_image=bytscl(grid_var,top=top_color,min=dmin_sw_up,max=dmax_sw_up)
      result=where(grid_var eq -9999,count)
      if count gt 0 then data_image[result]=255  ;gray
      pnum=0
      print,'start image'
      p1=image(data_image,$
        ;grid_lon,grid_lat,limit=[llat,llon,ulat,rlon],$  ;map limit vector
        grid_colon,grid_lat,limit=[llat,lcolon,ulat,rcolon],$  ;map limit vector
        /current,$  ;Put it in the current open plot window p0
        grid_units='degrees',$
        /box_axes,$
        center_longitude=(rcolon+lcolon)/2.0,$
        label_position=0,$
        font_size=12,$  ;bigger font
        linestyle=2,$  ;grid will be dashed line
        rgb_table=mytable,$  ;use color table 5
        map_projection=map_projection,$  ;map projection
        position=pos[pnum,*])
      print,'end image'
      ;p2=mapcontinents(color='white',/hires,thick=2)  ;add the continent lines
      p2=mapcontinents(color='white',thick=2)  ;add the continent lines
      ;  Grid box size
      if do_hist eq 'yes' then begin
      print,'plot boxes'
      for i=0,num_files_hist-1 do begin
        p3=plot([hlon_array[i]-dlon,hlon_array[i]+dlon,hlon_array[i]+dlon,hlon_array[i]-dlon,hlon_array[i]-dlon],$
          [hlat_array[i]-dlat,hlat_array[i]-dlat,hlat_array[i]+dlat,hlat_array[i]+dlat,hlat_array[i]-dlat],$
          /overplot,/data,color='green',thick=3)
      endfor
      endif
      ;  Put on a colorbar
      c0=colorbar(range=[dmin_sw_up,dmax_sw_up],$  ;the colorbar is to match the image labeled p1
        orientation=1,position=reform(cbpos[pnum,*]),font_size=12,rgb_table=mycbtable,$
        tickdir=1,border_on=1,title='SW UP')
      imagename=strsplit(file_basename(files_ceres[f]),'hdf',/extract,/regex)+'png'
      p0.save,imagename
      ;stop
  endif  ;do plot
  ctime=!NULL
  clats=!NULL
  ccolons=!NULL
  sw_up=!NULL
  lw_up=!NULL
  sw_toa=!NULL
endfor  ;loop through ceres
ctime=!NULL
clats=!NULL
ccolons=!NULL
sw_up=!NULL
lw_up=!NULL
sw_toa=!NULL
  stop
  
;        ;  Plot lat-lon track and timeseries of variables
;        set_plot,'z'
;        common colors,r_orig,g_orig,b_orig,r_curr,g_curr,b_curr
;        loadct,39
;        !p.background=!d.n_colors-1
;        device,set_resolution=[900,700]
;
;        ;  Plot time format
;        dummy=label_date(date_format=['%Z/%M/%D!C%H:%I'])
;
;        ;  Set up the map coordinates
;        erase
;        map_set,0,-160,/noerase,/isotropic,/cylindrical,latdel=10,londel=20,$
;          /grid,/continents,color=0,/noborder,label=1,position=[0.1,0.4,0.9,0.9]
;        oplot,geo_lons,geo_lats,color=70,thick=3  ;geoprof
;        oplot,fflons,fflats,color=250,thick=1
;
;        dmax=1.1;max(alb)
;        dmin=-0.1;min(alb)
;        plot,gp_jds,alb,color=0,position=[0.1,0.1,0.9,0.4],xtickformat='label_date',$
;          xstyle=1,ystyle=1,/noerase,yrange=[dmin,dmax]
;        imagename='ceres_alb.gif'
;        write_gif,imagename,tvrd()
;endif  ;end of do plot



end