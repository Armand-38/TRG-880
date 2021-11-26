title 'Forward Model Building: Before pruning';
proc g3grid data=final out=fil;
   grid x1*x2=yhat;
   run;
quit;
proc g3d data=fil;
   plot x1*x2=yhat /ctop='red';
run;
