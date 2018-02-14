function [histout,outvar,dvar]=histweight(npart,nbin,varmin,varmax,var,hweight,iwrong)
%% ----------------------------------------------------------------------------------
%% INPUT ARGUMENTS:
%% ----------------------------------------------------------------------------------
%%    - NPART   = NUMBER OF EVENTS TO FILL THE HISTOGRAM
%%    - NBIN    = REQUIRED NUMBER OF BINS IN THE HISTOGRAM
%%    - VARMIN  = LOWER LIMIT OF THE VARIABLE TO LOOK AT
%%    - VARMAX  = UPPER LIMIT OF THE VARIABLE TO LOOK AT
%%    - VAR     = VECTOR OF THE VARIABLE TO LOOK AT (LENGTH=NPART)
%%    - HWEIGHT = VECTOR OF WEIGHT OF EACH EVENT (LENGTH=NPART)
%%    - IWRONG  = 0 TO CRASH IF VAR(I)>VARMAX OR VAR(I)<VARMIN, 1 TO REMOVE THE EVENT
%% ----------------------------------------------------------------------------------
%% OUTPUT ARGUMENTS:
%% ----------------------------------------------------------------------------------
%%    - HISTOUT = VECTOR OF HEIGHTS OF HISTOGRAM BINS
%%    - OUTVAR  = BINNED VECTOR OF VARIABLE TO LOOK AT
%%    - DVAR    = WIDTH OF A BIN
%% ----------------------------------------------------------------------------------

histout = 0;
outvar  = 0;
dvar    = 0;

nbad    = 0;

if nargin == 6
  iwrong = 1;
end

if nargin < 6
  disp('-------------------------------------------------------------------------')
  disp('Use with at least 6 arguments:')
  disp('[histout,outvar,dvar]=histweight(npart,nbin,varmin,varmax,var,hweight)');
  disp('7th optional argument: 0 to crash if var(i)>varmax, 1 to remove the event');
  disp('-------------------------------------------------------------------------')
else
  
  histout = zeros(nbin,1);
  dvar = (varmax-varmin)/(nbin-1);

  for i=1:npart
    vart = (var(i)-varmin) / dvar;
    ibin = vart-mod(vart,1)+1;
    if ibin > nbin
      if iwrong == 0
	disp('---------------------------------------------------------------------------');
	disp(' Problem in histweight.m: histogram goes beyond the required maximum limit.')
	disp(' You have to change ''varmax'' in the call of histweight')
	disp([' varmax = ',num2str(varmax)])
	disp([' var(i) = ',num2str(var(i))])
	disp('---------------------------------------------------------------------------');
	keyboard
      else
	nbad = nbad+1;
	if nbad == 1
	  disp('-------------------------------------')
	end
	disp(['Event removed: ',num2str(var(i))])
      end
    elseif ibin < 1
      if iwrong == 0
	disp('---------------------------------------------------------------------------');
	disp(' Problem in histweight.m: histogram goes beyond the required maximum limit.')
	disp(' You have to change ''varmin'' in the call of histweight')
	disp([' varmin = ',num2str(varmin)])
	disp([' var(i) = ',num2str(var(i))])
	disp('---------------------------------------------------------------------------');
	keyboard
      else
	nbad = nbad+1;
	if nbad == 1
	  disp('-------------------------------------')
	end
	disp(['Event removed: ',num2str(var(i))])
      end
    else
      histout(ibin) = histout(ibin) + hweight(i);
    end
  end
  
  outvar = linspace(varmin,varmax,nbin)';

  if nbad > 0
    disp('-------------------------------------')
    disp(['Total number of removed events = ',num2str(nbad)])
    disp('-------------------------------------')
  end
  
end
