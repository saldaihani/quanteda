###########################################
### DEFINE WORDFISH ESTIMATION FUNCTION ###
###########################################

require(Rcpp)
require(RcppArmadillo)
require(inline)

wordminnowSrc <- '

	// DEFINE INPUTS
	
		Rcpp::NumericMatrix Y(wfm); 
		Rcpp::NumericVector priorvec(priors);
		Rcpp::NumericVector tol(tolerances); 	
		
		double prioralpha = priorvec(0);
		double priorpsi = priorvec(1);
		double priorbeta = priorvec(2);
		double priortheta = priorvec(3);		
		
		int N = Y.nrow();
		int K = Y.ncol();

	// SET INITIAL VALUES
	
		Rcpp::NumericVector alpha(N); 
		Rcpp::NumericVector psi(K); 
		Rcpp::NumericVector beta(K); 
		Rcpp::NumericVector theta(N); 
		
		// Construct Chi-Sq Residuals	
		arma::mat C(Y.begin(),N,K); 
		arma::colvec rsum = sum(C,1);
		arma::rowvec csum = sum(C,0);
		double asum = sum(rsum);		
		for (int i=0; i < N; i++){
			for (int k=0; k < K; k++){
				C(i,k) = (Y(i,k) - rsum(i)*csum(k)/asum)/sqrt(rsum(i)*csum(k)/asum);	
			}
		}
		
		// Singular Value Decomposition of Chi-Sq Residuals
		arma::mat U(N,N);
		arma::vec s(N);
		arma::mat V(K,N);
		svd(U,s,V,C);
	
		// Load initial values
		for (int i=0; i < N; i++) theta(i) = pow(rsum(i)/asum,-0.5) * U(i,0);
		for (int k=0; k < K; k++) beta(k) = 0; // pow(csum(k)/asum,-0.5) * V(k,0);
		for (int i=0; i < N; i++) alpha = log(rsum);
		psi = log(csum/N);
		
		alpha = alpha - log(mean(rsum));
		theta = (theta - mean(theta))/sd(theta);
		
		// Create temporary variables
		Rcpp::NumericMatrix pars(2,1);
		Rcpp::NumericMatrix newpars(2,1);		
		Rcpp::NumericMatrix G(2,1);
		Rcpp::NumericMatrix H(2,2);
		double loglambdaik;
		Rcpp::NumericVector lambdai(K);
		Rcpp::NumericVector lambdak(N);
		double stepsize = 1.0;
		double cc = 0.0;
		int inneriter = 0;
		int outeriter = 0;
		
		double lastlp = -2000000000000.0;
		double lp = -1.0*(sum(0.5 * ((alpha*alpha)/(prioralpha*prioralpha))) + sum(0.5 * ((psi*psi)/(priorpsi* priorpsi))) + sum(0.5 * ((beta*beta)/(priorbeta* priorbeta))) + sum(0.5 * ((theta*theta)/(priortheta*priortheta))));
			for (int i=0; i < N; i++){
				for (int k=0; k < K; k++){
					loglambdaik = alpha(i) + psi(k) + beta(k)*theta(i);
					lp = lp + loglambdaik*Y(i,k)-exp(loglambdaik);
				}
			}
	
	// BEGIN WHILE LOOP
	while(((lp - lastlp) > tol(0)) && outeriter < 100){	
		outeriter++;
	
		// UPDATE WORD PARAMETERS
			for (int k=0; k < K; k++){
				cc = 1;
				inneriter = 0;
				if (outeriter == 1) stepsize = 0.5;
				while ((cc > tol(1)) && inneriter < 10){
					inneriter++;
					lambdak = exp(alpha + psi(k) + beta(k)*theta);
					G(0,0) = sum(Y(_,k) - lambdak) - psi(k)/(priorpsi*priorpsi);
					G(1,0) = sum(theta*(Y(_,k) - lambdak)) - beta(k)/(priorbeta*priorbeta);
					H(0,0) = -sum(lambdak) - 1/(priorpsi* priorpsi);
					H(1,0) = -sum(theta*lambdak);
					H(0,1) = H(1,0);
					H(1,1) = -sum((theta*theta)*lambdak) - 1/(priorbeta* priorbeta);
					pars(0,0) = psi(k);
					pars(1,0) = beta(k);
					newpars(0,0) = pars(0,0) - stepsize*(H(1,1)*G(0,0) - H(0,1)*G(1,0))/(H(0,0)*H(1,1) - H(0,1)*H(1,0));
					newpars(1,0) = pars(1,0) - stepsize*(H(0,0)*G(1,0) - H(1,0)*G(0,0))/(H(0,0)*H(1,1) - H(0,1)*H(1,0));
					psi(k) = newpars(0,0);
					beta(k) = newpars(1,0);
					cc = max(abs(newpars - pars));
					stepsize = 1.0;
				}	
			}	

		
		// UPDATE DOCUMENT PARAMETERS
			for (int i=0; i < N; i++){
				cc = 1;
				inneriter = 0;
				if (outeriter == 1) stepsize = 0.5;
				while ((cc > tol(1)) && inneriter < 10){
					inneriter++;
					lambdai = exp(alpha(i) + psi + beta*theta(i));
					G(0,0) = sum(Y(i,_) - lambdai) - alpha(i)/(prioralpha* prioralpha);
					G(1,0) = sum(beta*(Y(i,_) - lambdai)) - theta(i)/(priortheta* priortheta);		
					H(0,0) = -sum(lambdai) - 1/(prioralpha* prioralpha);
					H(1,0) = -sum(beta*lambdai);
					H(0,1) = H(1,0);
					H(1,1) = -sum((beta* beta)*lambdai) - 1/(priortheta* priortheta);
					pars(0,0) = alpha(i);
					pars(1,0) = theta(i);
					newpars(0,0) = pars(0,0) - stepsize*(H(1,1)*G(0,0) - H(0,1)*G(1,0))/(H(0,0)*H(1,1) - H(0,1)*H(1,0));
					newpars(1,0) = pars(1,0) - stepsize*(H(0,0)*G(1,0) - H(1,0)*G(0,0))/(H(0,0)*H(1,1) - H(0,1)*H(1,0));
					alpha(i) = newpars(0,0);
					theta(i) = newpars(1,0);
					cc = max(abs(newpars - pars));	
					stepsize = 1.0;
				}	
			}
		
		alpha = alpha - mean(alpha);
		theta = (theta - mean(theta))/sd(theta);		
		
		// CHECK LOG-POSTERIOR FOR CONVERGENCE
			lastlp = lp;
			lp = -1.0*(sum(0.5 * ((alpha*alpha)/(prioralpha*prioralpha))) + sum(0.5 * ((psi*psi)/(priorpsi* priorpsi))) + sum(0.5 * ((beta*beta)/(priorbeta* priorbeta))) + sum(0.5 * ((theta*theta)/(priortheta*priortheta))));
			for (int i=0; i < N; i++){
				for (int k=0; k < K; k++){
					loglambdaik = alpha(i) + psi(k) + beta(k)*theta(i);
					lp = lp + loglambdaik*Y(i,k)-exp(loglambdaik);
				}
			}
			// Rprintf("%d: %f2\\n",outeriter,lp);

	// END WHILE LOOP		
	} 
		
	// DEFINE OUTPUT	
	
	// return(theta);	
	return Rcpp::List::create(Rcpp::Named("theta") = theta,
                          Rcpp::Named("alpha") = alpha,
                          Rcpp::Named("psi") = psi,
                          Rcpp::Named("beta") = beta);

'

############################################
### DEFINE WORDFISH ESTIMATION R WRAPPER ###
############################################

wordminnow <- cxxfunction(signature(
	wfm = "numeric",
	priors = "numeric",
	tolerances = "numeric"
	), wordminnowSrc, plugin = "RcppArmadillo")


