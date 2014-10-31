f24_R2_cycling=function(x, t=2*(0:(length(x)-1)), period=24, offset=0)
{
	
	kk = which(!is.na(x)==TRUE)
	x = x[kk]
	t = t[kk]
	n=length(x)
	#mu=mean(x)
	nb.timepoints=length(x)
	if(n<4)
	{ 
		if(n==0) c(nb.timepoints=nb.timepoints, mean=NA, amp=NA, relamp=NA,phase=NA,pval=NA) 
		else 
		{
			c(nb.timepoints=nb.timepoints, mean=mean(x), amp=NA, relamp=NA,phase=NA,pval=NA)
		}
	}
	else
	{
		sig2=var(x)
		c=cos(2*pi*t/period)
		s=sin(2*pi*t/period)
		A = mean(x*c)-mean(x)*mean(c)
		B = mean(x*s)-mean(x)*mean(s)
		c1 = mean(c^2)-mean(c)^2
		c2 = mean(c*s)-mean(c)*mean(s)
		c3 = mean(s^2)-mean(s)^2
		b = (A*c2-B*c1)/(c2^2-c1*c3)
		a = (A-b*c2)/c1
		mu = mean(x)-a*mean(c)-b*mean(s)
		#	b=2*mean(x*s)
		x.hat=mu+a*c+b*s
		sig2.1=var(x-x.hat)
		if(is.na(a)||is.na(b)) {c(nb.timepoints=nb.timepoints, mean=mean(x), amp=NA, relamp=NA,phase=NA,pval=NA)}
		else
		{
			p=3
			R2=0
			if(sig2>0) R2=1-sig2.1/sig2
			# http://www.combustion-modeling.com/downloads/beta-distribution-for-testing-r-squared.pdf
			# I checked that it works
			amp=max(x)-min(x)
			phase=period/(2*pi)*atan2(b, a)
			if(phase<0) phase=phase+period
			if(phase>period) phase=phase-period
			phase=(phase+offset)%%period
			pval = pbeta(R2, (p-1)/2, (n-p)/2, lower.tail = FALSE, log.p = FALSE)
			
			c(nb.timepoints=nb.timepoints, mean =mean(x), amp=2*sqrt(a^2+b^2),relamp=sqrt(a^2+b^2)/(mu),phase=phase, pval=pval)
		}
	}
}
