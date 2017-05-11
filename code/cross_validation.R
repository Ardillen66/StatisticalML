
#Code fo K-fold cross validation
crossval_nn <- function(x, y, n_folds=10, size=2, maxit=1000) {
	index=sample((1:length(y)))
	cv_size<-floor(length(y)/n_folds)
	cv_err<-numeric(n_folds)
	for (k in 1:n_folds) {
		k_ts<-(((k-1)*size.CV + 1) : (k*size.CV))
		k_tr<-setdiff(index, k_ts)
		x_test<-x[k_ts, ]
		y_test<-y[k_ts]
		x_train<-[k_tr, ]
		y_train<-[k_tr]

		model<-nnet(y_train~., x_train, size, maxit, trace=FALSE, linout=FALSE)

		y_hat<-predict(model, x_test)

		cv_err[k]<-mean((y_hat-y_test)^2)

	}
}