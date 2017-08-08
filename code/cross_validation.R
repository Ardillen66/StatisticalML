library(nnet)
library(rpart) 

#Code fo K-fold cross validation

# Cross validates data for neural network with given parameters
crossval_nn <- function(x, y, n_folds=10, size=2, maxit=1000) {
	cv_size<-floor(length(y)/n_folds)
	cv_err<-numeric(n_folds)
	for (k in 1:n_folds) {
		k_ts<-(((k-1)*cv_size + 1) : (k*cv_size))
		k_tr<-setdiff(1:length(y), k_ts)
		x_test<-x[k_ts,]
		y_test<-y[k_ts]
		x_train<-x[k_tr,]
		y_train<-y[k_tr]

		model<-nnet(x_train, y_train, size=size, maxit=maxit, trace=FALSE, linout=TRUE)

		y_hat<-predict(model, x_test)

		cv_err[k]<-sqrt(mean((log(y_hat)-log(y_test))^2))

	}
	cv_err
}

# Cros validate simple linear model
crossval_lm <- function(x, y, n_folds=10) {
	cv_size<-floor(length(y)/n_folds)
	cv_err<-numeric(n_folds)
	for (k in 1:n_folds) {
		k_ts<-(((k-1)*cv_size + 1) : (k*cv_size))
		k_tr<-setdiff(1:length(y), k_ts)
		x_test<-x[k_ts,]
		y_test<-y[k_ts]
		x_train<-x[k_tr,]
		y_train<-y[k_tr]

		ds <- cbind(x_train,sale_price=y_train)

		model<-lm(sale_price~., ds)

		y_hat<-predict(model, x_test)

		cv_err[k]<-sqrt(mean((log(y_hat)-log(y_test))^2))

	}
	cv_err
}

# Cross validate polynomial model from linear combination of linear models
crossval_pol <- function(x, y, n_folds=10, degree=1) {
	cv_size<-floor(length(y)/n_folds)
	cv_err<-numeric(n_folds)
	for (k in 1:n_folds) {
		k_ts<-(((k-1)*cv_size + 1) : (k*cv_size))
		k_tr<-setdiff(1:length(y), k_ts)
		x_test<-x[k_ts,]
		y_test<-y[k_ts]
		x_train<-x[k_tr,]
		y_train<-y[k_tr]

		xp_train<-NULL
		xp_test<-NULL
		for (deg in 1:degree) {
			xp_train<-cbind(xp_train, x_train^deg)
			xp_test<-cbind(xp_test, x_test^deg)
		}
		ds <- data.frame(cbind(y_train, xp_train))
		names(ds)[1]<-"sale_price"

		model<-lm(sale_price~., ds)

		test_data<-data.frame(xp_test)
 		names(test_data)<-names(ds[2:NCOL(ds)])

		y_hat<-predict(model, test_data)

		cv_err[k]<-sqrt(mean((log(y_hat)-log(y_test))^2))

	}
	cv_err
}

#Cross validate model with default, prunded and random forest fits
crossval_tree <- function(x, y, n_folds=10) {
	cv_size<-floor(length(y)/n_folds)
	cv_err<-matrix(nrow=2, ncol=n_folds)
	for (k in 1:n_folds) {
		k_ts<-(((k-1)*cv_size + 1) : (k*cv_size))
		k_tr<-setdiff(1:length(y), k_ts)
		x_test<-x[k_ts,]
		y_test<-y[k_ts]
		x_train<-x[k_tr,]
		y_train<-y[k_tr]

		ds<-cbind(x_train, sale_price=y_train)
		#Fit tree
		fit<-rpart(sale_price~., method="anova", data=ds)
		#Find complexity parameter with lowest cv error
		cp<-fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
		#prune with found parameter
		pfit<-prune(fit, cp=cp)

		y_hat<-predict(fit, x_test)
		y_hatp<-predict(pfit, x_test)

		cv_err[1, k]<-sqrt(mean((log(y_hat)-log(y_test))^2))
		cv_err[2, k]<-sqrt(mean((log(y_hatp)-log(y_test))^2))
	}
	cv_err
}

crossval_ensemble <- function(x, y, weights, n_folds=10, degree=1, nn_size=2) {
	cv_size<-floor(length(y)/n_folds)
	cv_err<-numeric(n_folds)
	for (k in 1:n_folds) {
		k_ts<-(((k-1)*cv_size + 1) : (k*cv_size))
		k_tr<-setdiff(1:length(y), k_ts)
		x_test<-x[k_ts,]
		y_test<-y[k_ts]
		x_train<-x[k_tr,]
		y_train<-y[k_tr]


		linear<-lm(sale_price~., ds)

		ds<-cbind(x_train, sale_price=y_train)
		#Fit models
		tree<-rpart(sale_price~., method="anova", data=ds)
		linear<-lm(sale_price~., ds)
		nn<-nnet(x_train, y_train, size=nn_size, maxit=1000, trace=FALSE, linout=TRUE)


		y_hat<-weights[1]*predict(linear, x_test) + weights[2]*predict(tree, x_test) + weights[3]*predict(nn, x_test)

		cv_err[k]<-sqrt(mean((log(y_hat)-log(y_test))^2))

	}
	cv_err
}