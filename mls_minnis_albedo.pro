pro mls_minnis_albedo,result,const

; this procedure parameterizes the abedo for a given visble optical depth 
; and solar zenith angle following Minnis et al. (1998) Figure 7.
;DOI: https://doi.org/10.1175/1520-0469(1998)055<3313:PORAEE>2.0.CO;2

mu0=[$
  0.05,0.05,0.05,0.05,$
  0.10,0.10,0.10,0.10,$
  0.15,0.15,0.15,0.15,$
  0.20,0.20,0.20,0.20,$
  0.25,0.25,0.25,0.25,$
  0.30,0.30,0.30,0.30,$
  0.35,0.35,0.35,0.35,$
  0.40,0.40,0.40,0.40,$
  0.45,0.45,0.45,0.45,$
  0.50,0.50,0.50,0.50,$
  0.55,0.55,0.55,0.55,$
  0.60,0.60,0.60,0.60,$
  0.65,0.65,0.65,0.65,$
  0.70,0.70,0.70,0.70,$
  0.75,0.75,0.75,0.75,$
  0.80,0.80,0.80,0.80]
  

tau=[$
  2.,8,32.,64.,$
  2.,8,32.,64.,$
  2.,8,32.,64.,$
  2.,8,32.,64.,$
  2.,8,32.,64.,2.,8,32.,64.,$
  2.,8,32.,64.,2.,8,32.,64.,2.,8,32.,64.,2.,8,32.,64.,2.,8,32.,64.,2.,8,32.,64.,$
  2.,8,32.,64.,2.,8,32.,64.,2.,8,32.,64.,2.,8,32.,64.]

alb=[$
  0.69, 0.80, 0.93, 0.95,$
  0.62, 0.75,0.90,0.95,$
  0.56,0.73,0.88,0.95,$
  0.51,0.70, 0.87,0.95,$
  0.46,0.675,0.86,0.94,$
  0.42,0.65,0.85,0.94,$
  0.38,0.625,0.84,0.94,$
  0.34,0.60,0.83,0.93,$
  0.3, 0.58,0.82,0.93,$
  0.27, 0.55,0.81, 0.93,$
  0.24,0.53,0.80,0.93,$
  0.22, 0.51,0.785,0.93,$
  0.19,0.48,0.78,0.92,$
  0.17,0.46,0.77,0.92,$
  0.15,0.44,0.76,0.92,$
  0.14,0.42,0.75,0.92]
  
  
result=regress(rotate([[sqrt(mu0)],[alog(tau)]],4),alb,chisq=chisq,const=const,$
  ftest=ftest,mcorrelation=mcorrelation,sigma=sigma,status=status,yfit=yfit )  ;, alb, order, ana=analysis, si=significance)

;acos(mu0*!pi/180.)

;  Set up the positions
pxdim=900 & pydim=700  
xl=0.07 & xr=0.97
yb=0.12 & yt=0.90
sx=0.10
sy=0.13
numplots_x=2
numplots_y=1
position_plots,xl,xr,yb,yt,sx,sy,numplots_x,numplots_y,pos

pnum=0
p0=plot(alb,yfit,/buffer,dimensions=[pxdim,pydim],position=pos[pnum,*],$
  symbol='*',color='black',xtitle='actual albedo', ytitle='fitted albedo',$
  title='Alb_mu0_param_scatter',$
  xrange=[0.,1.],yrange=[0.,1.],linestyle=6,font_size=12)
p1=plot([0.,1.],[0.,1.],color='red',/overplot)
t1=text(pos[pnum,0]+0.05,pos[pnum,3]-0.05,'cor='+strcompress(correlate(alb, yfit),/remove_all),font_size=14)
p0.save,'Alb_mu0_param_scatter.png',height=pydim

plot_mu0=mu0[where(tau eq 2.)]
plot_tau=tau[where(tau eq 2.)]
plot_alb=alb[where(tau eq 2.)]
  
regress_alb=fltarr(n_elements(plot_mu0))
regress_alb=const+(result[0]*(sqrt(plot_mu0)))+(result[1]*(alog(plot_tau)))

pnum=1
p2=plot(plot_mu0,plot_alb,/current,position=pos[pnum,*],$
  color='black',xtitle='Cosine of the Solar Zenith Angle',ytitle='0.65 micron Albedo', $
  title='Albedo versus mu0',symbol='*',$
  yrange=[0.,1.],xrange=[0.,1.],font_size=12)
p3=plot(plot_mu0,regress_alb,/overplot,color='black',linestyle=2)

plot_mu0=mu0[where(tau eq 8.)]
plot_tau=tau[where(tau eq 8.)]
plot_alb=alb[where(tau eq 8.)]
  
regress_alb=fltarr(n_elements(plot_mu0))
regress_alb=const+(result[0]*(sqrt(plot_mu0)))+(result[1]*(alog(plot_tau)))

p4=plot(plot_mu0,plot_alb,/overplot,color='blue',symbol='*')
p5=plot(plot_mu0,regress_alb,/overplot,color='blue',linestyle=2)

plot_mu0=mu0[where(tau eq 32.)]
plot_tau=tau[where(tau eq 32.)]
plot_alb=alb[where(tau eq 32.)]

regress_alb=fltarr(n_elements(plot_mu0))
regress_alb=const+(result[0]*(sqrt(plot_mu0)))+(result[1]*(alog(plot_tau)))

p6=plot(plot_mu0,plot_alb,/overplot,color='red',symbol='*')
p7=plot(plot_mu0,regress_alb,/overplot,color='red',linestyle=2)

plot_mu0=mu0[where(tau eq 64.)]
plot_tau=tau[where(tau eq 64.)]
plot_alb=alb[where(tau eq 64.)]
regress_alb=fltarr(n_elements(plot_mu0))
regress_alb=const+(result[0]*(sqrt(plot_mu0)))+(result[1]*(alog(plot_tau)))

p8=plot(plot_mu0,plot_alb,/overplot,color='green',symbol='*')
p9=plot(plot_mu0,regress_alb,/overplot,color='green',linestyle=2)

p0.save,'Alb_mu0_param_scatter.png',height=pydim


;mu0=[0.0,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,1.0]
sza0_deg=acos(mu0)*!radeg

end
